-module(vrrpd_mon_srv).
-behaviour(gen_server).


%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).


-define (SERVER, ?MODULE).
-define (SCRIPT, "vrrpd_mon.sh").
-define (BACKUP, "we are now a backup router.").
-define (MASTER, "we are now the master router.").
-define (VRRPD_SYSLOG_LINE, "vrrpd:").

-record(mon_state, {
                      mon_port	:: any(),
		      current_str :: any()
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
	timer:sleep(200),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
        Dir = priv_dir(?MODULE),
        ScriptName = filename:join(Dir,"vrrpd_mon.sh"),
	io:format("Running script from ~p ~p ~n",[Dir,ScriptName]), 	     
	
        Port = open_port({spawn, ScriptName}, [{line,120}]),
	{ok, #mon_state{mon_port=Port,current_str=[]}}.

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

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

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
handle_cast(_Msg, State) ->
	{noreply, State}.

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
handle_info({_Port,{exit_status,Status}}, State) ->
        io:format("~p external module failure ~p terminating ~n", [?MODULE, Status]),
	{stop, Status, State};

handle_info({_Port, {data, {Flag, Line}}}, State) ->
        case Flag of
	   eol ->
	     #mon_state{current_str=AccumLine}=State,
	     io:format("~p eol Received: ~p ~n", [?MODULE, AccumLine ++ Line]),
	       LineToProcess = AccumLine ++ Line,
	       case is_vrrpd_master(LineToProcess) of
	         true -> 
% put actual processing here
                  f:s(),
% f:s() stands for:
%                 application:start(sasl),application:start(gen_listener_tcp),application:start(regine),application:start(flower),
%		  flower_simple_switch:start_link(),
%		  flower_tcp_transport:listen(6633,[]),
	          io:format("~n~n Activate  SDN Controller ~n");
		 false -> 
	            case is_vrrpd_backup(LineToProcess) of
		      true ->
% put actual processing here
                          application:stop(flower),
	                  io:format("~n~n Deactivate  SDN Controller ~n");
	              false ->
% do nothing		      
		         []
	            end		 
	     end,
	     NewState=State#mon_state{current_str=[]},
	     {noreply, NewState};

          noeol ->
	     #mon_state{current_str=AccumLine}=State,
	     NewAccumLine = AccumLine ++ Line,
	     io:format("~p noeol Received: ~p ~n", [?MODULE, AccumLine ++ Line]),
	     NewState=State#mon_state{current_str=NewAccumLine},
	     {noreply, NewState}
	end;

handle_info(_Info, State) ->
        io:format("Info!~n"),
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

is_vrrpd_master(Line) -> 
 (string:len(Line) > 0) and (string:str(Line,?VRRPD_SYSLOG_LINE)>0) and  (string:str(Line,?MASTER) > 0).

is_vrrpd_backup(Line) ->
  (string:len(Line) > 0) and (string:str(Line,?VRRPD_SYSLOG_LINE)>0) and (string:str(Line,?BACKUP) > 0).

priv_dir(Mod) ->
Ebin = filename:dirname(code:which(Mod)),
    filename:join(filename:dirname(Ebin), "priv").
