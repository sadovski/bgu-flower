-module(flower_tcp_transport).
-behaviour(gen_listener_tcp).

-include("flower_debug.hrl").

%% API
-export([listen/2, connect/3, shutdown/2]).

%% Transport Modules Callbacks
-export([listener_spec/1, connect/2, close/1, send/2]).

%% Listener exports
-export([start_link/2]).

%% gen_listener_tcp callbacks
-export([init/1, handle_accept/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TCP_CLIENT_OPTS, [binary, inet,
			  {active,       false},
			  {send_timeout, 5000},
			  {nodelay,      true},
			  {packet,       raw},
			  {reuseaddr,    true}]).

-define(TCP_SERVER_OPTS, [binary, inet,
			  {ip,           {0,0,0,0}},
			  {active,       false},
			  {send_timeout, 5000},
			  {backlog,      10},
			  {nodelay,      true},
			  {packet,       raw},
			  {reuseaddr,    true}]).

%%%===================================================================
%%% API
%%%===================================================================

%% start a TCP listener process on the given Port with Options
%% no longer is in use
listen(Port, Options) ->
    flower_sup:start_listener(?MODULE, {Port, Options}).

shutdown(Port, Options) ->
    flower_sup:stop_listener(?MODULE, {Port, Options}).

connect(Host, Port, Options) ->
    flower_datapath:connect(?MODULE, {Host, Port, Options}).

%%%===================================================================
%%% Transport Module Callbacks
%%%===================================================================

%% return a supervisor spec to start a listener
listener_spec({Port, Options}) ->
    {{?MODULE, Port},
     {?MODULE, start_link, [Port, Options]},
     permanent, 5000, worker, [?MODULE]}.

connect({Host, Port, Options}, Timeout) ->
    gen_tcp:connect(Host, Port, Options ++ ?TCP_CLIENT_OPTS, Timeout).

close(Socket) ->
    gen_tcp:close(Socket).

send(Socket, Packet) ->
    gen_tcp:send(Socket, Packet).

%%%===================================================================
%%% Listener Callbacks
%%%===================================================================

start_link(Port, Options) ->
    gen_listener_tcp:start_link({local, ?MODULE}, ?MODULE, {Port, Options}, [{debug,[trace]}]).

init({Port, Options}) ->
    {ok, {Port, lists:merge(lists:sort(Options), lists:sort(?TCP_SERVER_OPTS))}, nil}.

handle_accept(Sock, State) ->
%%%===================================================================
%%% AkivaS
%%%===================================================================
    case inet:peername(Sock) of
        {ok,{IpAddress,Port}} ->
			ok;
        {error,Why} ->
          io:format(">> flower_tcp_transport:handle_accept Cannot get information about connection ~p~n",[Why])
    end,
%%%===================================================================
%%% AkivaS
%%%===================================================================
    case flower_datapath:start_connection(?MODULE) of % starts flower_datapath by adding it to datapath_sup and connecting it to this module
	{ok, Pid} -> 
	    ok = gen_tcp:controlling_process(Sock, Pid), % connecting the flower_datapath to the socket. it will handle all the packets to and from the switch 
	    flower_datapath:accept(Pid, Sock);
	_ ->
	    error_logger:error_report([{event, accept_failed}]),
	    gen_tcp:close(Sock)
    end,
    {noreply, State}.

handle_call(Request, _From, State) ->
    {reply, {illegal_request, Request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
