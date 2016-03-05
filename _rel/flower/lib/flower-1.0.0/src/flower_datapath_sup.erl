-module(flower_datapath_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_connection/1, datapaths/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_connection(TransportMod) ->% adds the flower_datapath module to the supervision tree, thus starting the module using start_link
    supervisor:start_child(?MODULE, [TransportMod]). 

datapaths() ->
    lists:map(fun({_, Child, _, _}) -> Child end, supervisor:which_children(?MODULE)).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Element = {flower_datapath, {flower_datapath, start_link, []},
    			temporary, brutal_kill, worker, [flower_datapath]},
	Children = [Element],
	RestartStrategy = {simple_one_for_one, 0, 1},% simple_one_for_one means none of these will be started following this and they should already be up 
    {ok, {RestartStrategy, Children}}.
