-module(flower_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case flower_sup:start_link() of
		{ok,Pid}->
%		        flower_simple_switch:start_link(),
			{ok,Pid};
		Other->
			{error, Other}
	end.

stop(_State) ->
    ok.
