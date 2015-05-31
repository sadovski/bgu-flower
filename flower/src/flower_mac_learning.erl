-module(flower_mac_learning).

-behaviour(gen_server).

%% API
-export([start_link/0, insert/2, insert/3, lookup/2,
	 expire/0, may_learn/1, may_learn/2, eth_addr_is_reserved/1,
	 dump/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
% each new MAC table entry gets an expiry time of 60 seconds.
-define(MAC_ENTRY_IDLE_TIME, 60).

-record(state, {
	  timer,
	  lru
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

insert(MAC, Port) ->
    insert(MAC, 0, Port).

insert(MAC, Switch, Port) ->
    gen_server:call(?SERVER, {insert, MAC, Switch, Port}).

lookup(MAC, Switch) ->
    gen_server:call(?SERVER, {lookup, MAC,Switch}).


dump() ->
    gen_server:call(?SERVER, {dump}).

expire() ->
    gen_server:cast(?SERVER, expire).

% Returns true if the MAC address is unicast (check bit #8) 
may_learn(<<_:7, BCast:1, _/binary>> = _MAC) ->
    (BCast =/= 1).

may_learn(<<_:7, BCast:1, _/binary>> = _MAC, _VLan) ->
    (BCast =/= 1).


%%
%% Some well known Ethernet multicast addresses[11]
%% Ethernet multicast addressType FieldUsage
%% 01-00-0C-CC-CC-CC  0x0802      CDP (Cisco Discovery Protocol),
%%                                VTP (VLAN Trunking Protocol)
%% 01-00-0C-CC-CC-CD  0x0802      Cisco Shared Spanning Tree Protocol Address
%% 01-80-C2-00-00-00  0x0802      Spanning Tree Protocol (for bridges) IEEE 802.1D
%% 01-80-C2-00-00-08  0x0802      Spanning Tree Protocol (for provider bridges) IEEE 802.1AD
%% 01-80-C2-00-00-02  0x8809      Ethernet OAM Protocol IEEE 802.3ah (A.K.A. "slow protocols")
%% 01-00-5E-xx-xx-xx  0x0800      IPv4 Multicast (RFC 1112)
%% 33-33-xx-xx-xx-xx  0x86DD      IPv6 Multicast (RFC 2464)
%%
%% Returns true if it is a reserved multicast address, that a bridge must
%% never forward, false otherwise.
%%
eth_addr_is_reserved(<<16#01, 16#80, 16#C2, 16#00, 16#00, 0:4, _:4>>) ->
    true;
eth_addr_is_reserved(_Addr) ->
    false.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    LRU = lrulist:new(),
	% start a timer that repeatedly calls 'expire' at intervals of X
	% returns a timer reference
    {ok, Timer} = timer:apply_interval(1000, ?MODULE, expire, []),
    {ok, #state{timer = Timer, lru = LRU}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({insert, MAC, Switch, Port}, _From, #state{lru = LRU} = State) ->
	% get the port asociated with this MAC and VLAN. 'none' if there isn't a match
    {Result, LRU0} =  lrulist:get({MAC,Switch}, LRU),
    {Reply, LRU1} = case Result of
			none ->
			    {ok, NewLRU} = lrulist:insert({MAC,Switch}, Port, LRU0, [{slidingexpire, ?MAC_ENTRY_IDLE_TIME}]),
			    {new, NewLRU};
			{ok, Data} ->
			    if (Data =/= Port) ->
				    {ok, NewLRU} = lrulist:insert({MAC,Switch}, Port, LRU0, [{slidingexpire, ?MAC_ENTRY_IDLE_TIME}]),
				    {updated, NewLRU};
			       true ->
						% we DO NOT need to update the expiry date on this entry
						% the function lrulist:get does that for us.
					   {ok, LRU0}
			    end
		    end,
    {reply, Reply, State#state{lru = LRU1}};

handle_call({lookup, MAC, Switch}, _From, #state{lru = LRU} = State) ->
    {Result, LRU0} =  lrulist:peek({MAC, Switch}, LRU),
    {reply, Result, State#state{lru = LRU0}};

handle_call({dump}, _From, #state{lru = LRU} = State) ->
    Result =  [{Mac,Val} || {{Mac,_VLan},Val} <- lrulist:dump(LRU)],
    {reply, {ok, Result}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(expire, #state{lru = LRU} = State) ->
    LRU0 = lrulist:purge(LRU),
    {noreply, State#state{lru = LRU0}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
