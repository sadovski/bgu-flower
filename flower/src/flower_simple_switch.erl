-module(flower_simple_switch).
-behaviour(gen_server).

-include("flower_debug.hrl").
-include("flower_packet.hrl").
-include("flower_flow.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).


-define (SERVER, ?MODULE). 
%% Topology representation:
%% {V,List}
%% Where 
%%		V 		: The name of the vertex (switch), NOT the label.
%%		List	: A list of all the vertices (switches) this vertex is connected to (undirected)
-define (SINGLE, [{"s1",[]}]).
-define (LINEAR_2, [{"s1",["s2"]}, {"s2",["s1"]}]). 

-record(controller_state, {
						   network_graph	:: digraph:graph()
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
	flower_dispatcher:join({packet, in}), %% register this tuple in the regine server so all packet_in events are sent to us
	flower_dispatcher:join(features_reply), %% register this event in the regine server so all feature_reply events are sent to us
	Graph = digraph:new([private]),
	insert_topology(Graph,?LINEAR_2),
	{ok, #controller_state{network_graph=Graph}}.

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

handle_cast({{packet, in}, InDataPath, Msg}, State) ->
%	InSwitch = find_vert_by_label(Graph,digraph:vertices(Graph),InDataPath),
	Flow = (catch flower_flow:flow_extract(0, Msg#ofp_packet_in.in_port, Msg#ofp_packet_in.data)),
	case Flow of
		#flow{} ->
%			io:format("A message came from {~p, ~p} (~p), dst_MAC is ~p. ",[InSwitch,Msg#ofp_packet_in.in_port,Flow#flow.dl_src, Flow#flow.dl_dst]),
			Destination = choose_destination(Flow#flow.in_port, InDataPath,Flow#flow.dl_src,Flow#flow.dl_dst),
			%% current problem! when s2 gets an arp request from s1, it makes the controller think h1 is at port2
			%% of s2. we need to distinguish each MAC as to which port it is on according to the asking switch.
			%% almost like a seperate table for each switch. key should be {MAC, asking_switch} and value is the same value
			%io:format("it's destination is ~p.~n",[Destination]),
			case Destination of
				none -> 
					%%=======================================
					%% Amir: this happens for all non-flood destinations. what do we have to do differently? drop packet maybe?
					Match = case Flow#flow.dl_type of
								arp -> flower_match:encode_ofp_matchflow([dl_src, dl_dst, tp_dst, tp_src, nw_proto, dl_type], Flow);
								_ -> flower_match:encode_ofp_matchflow([{nw_src_mask,0}, {nw_dst_mask,0}, tp_dst, tp_src, nw_proto, dl_type], Flow)
							end,% case Flow.dl_type
					%%                                                                vHEREv is where the *old* Actions was, now an empty list
					flower_datapath:install_flow(InDataPath, Match, 0,   60,         0,   []   ,Msg#ofp_packet_in.buffer_id,0,    Msg#ofp_packet_in.in_port,Msg#ofp_packet_in.data);
				%%=======================================
				flood->
					%% We don't know that MAC, or we don't set up flows.  Send along the packet without setting up a flow.
					flower_datapath:send_packet(InDataPath, Msg#ofp_packet_in.buffer_id, Msg#ofp_packet_in.data, [#ofp_action_output{port = flood, max_len = 0}], Msg#ofp_packet_in.in_port);
				DstPort ->
%					ShortPathList = case DstSwitch of
%										InSwitch-> [{InSwitch,InDataPath}];
%										_-> lists:map(fun(V)-> digraph:vertex(Graph, V) end, lists:reverse(digraph:get_short_path(Graph, InSwitch, DstSwitch)))
%									end,
					%% The output port is known, so add a new flow.
					%%=========================================================================
					%% Matchflow differentiation according to dl_type shall be added -- AkivaS
					%%
					Match = case Flow#flow.dl_type of
								% return a record of type #ofp_match which has set the params in vTHISv list according to
								% the params in Flow. match#ofp_match(wildcards) will have it's param bits set to 0 for all
								% parameters changed. '1' means "was not touched"
								arp -> flower_match:encode_ofp_matchflow([dl_src, dl_dst, tp_dst, tp_src, nw_proto, dl_type], Flow);
								% all other
								%
								% According to OpenFlow v1.0 spec, the numerical parameter (by default 32) specifies how many bits in the
								% address field to ignore. 0 - use exact matching, 1 - ignore LSB etc. 32 - ignore all bits of IP address
								_ -> flower_match:encode_ofp_matchflow([{nw_src_mask,0}, {nw_dst_mask,0}, tp_dst, tp_src, nw_proto, dl_type], Flow)
							end,% case Flow.dl_type
					%%=========================================================================
					%		    Match = flower_match:encode_ofp_matchflow([{nw_src_mask,32}, {nw_dst_mask,32}, nw_dst, nw_src, tp_dst, tp_src, nw_proto, dl_type], Flow),
					%               install_flow(Sw,      Match,Cookie,IdleTimeout,HardTimeout,           Actions,                              BufferId,               Priority,InPort,                   Packet)
					flower_datapath:install_flow(InDataPath, Match, 0,   60,         0,         [#ofp_action_output{port = DstPort, max_len = 0}],Msg#ofp_packet_in.buffer_id,0,  Msg#ofp_packet_in.in_port,Msg#ofp_packet_in.data)
			end;% case Destination
		_ ->
			io:format("Flow not found! That's bad...~n")
	end,% case Flow=...
	{noreply, State};


handle_cast(_Msg, State) ->
	{noreply, State};

handle_cast({features_reply, DataPath, #ofp_switch_features{ports=Ports}= _Msg}, #controller_state{network_graph = Graph}=State) ->
	#ofp_phy_port{name=PortNameBin}=lists:last(Ports),
	PortName = binary_to_list(PortNameBin),
	case digraph:vertex(Graph, PortName) of
		false->
			{stop, unkown_vertex,State};
		_->
			digraph:add_vertex(Graph, PortName, DataPath),
			{noreply, State}
	end.
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

%choose_destination(#flow{in_port = Port, dl_src = DlSrc, dl_dst = DlDst}, DataPath) ->
choose_destination(InPort, InSwitch, DlSrc, DlDst) ->
	% start by checking whether the DlSrc address is a reserved Multicast address
	case flower_mac_learning:eth_addr_is_reserved(DlSrc) of
		% Always use VLan = 0 to implement Shared VLAN Learning
		false -> 
			learn_mac(DlSrc, InSwitch, InPort),
			find_out_dest(DlDst, InSwitch, InPort);
		true -> none
	end.

% insert a new {MAC,Port} key-value to the MAC list or refresh the already known set.
learn_mac(DlSrc, InSwitch, InPort) ->		 
	case flower_mac_learning:may_learn(DlSrc) of
		true -> % bit #8 (last in first byte) is 0-> unicast address 
			flower_mac_learning:insert(DlSrc, InSwitch, InPort); % insert {MAC, InSwitch}, as key and InPort as it's value
		false ->
			not_learned
	end.

find_out_dest(DlDst, InSwitch, InPort) ->
	case flower_mac_learning:lookup(DlDst,InSwitch) of
		none -> flood;
		{ok, InPort} -> none; %% Don't send a packet back out its input port.
		{ok, OutputPort} -> OutputPort
	end.

insert_topology(Graph, Topology)->
	lists:foreach(fun(A)->
						  {V,List} = A,
						  case digraph:vertex(Graph,V) of
							  false->	digraph:add_vertex(Graph, V);
							  _	->	ok
						  end,
						  lists:foreach(fun(V2)->
												case digraph:vertex(Graph,V2) of
													false->	digraph:add_vertex(Graph, V2);
													_->		ok
												end,
												digraph:add_edge(Graph,V,V2),
												digraph:add_edge(Graph,V2,V)
										end,List)
				  end,Topology).


find_vert_by_label(_Graph,[],_Label)->	error;
find_vert_by_label(Graph, [V|T],Label)->
	case digraph:vertex(Graph, V) of
		{_V,Label}->
			V;
		_->
			find_vert_by_label(Graph, T,Label)
	end.


