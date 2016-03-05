%% @author sdn
%% @doc @todo Add description to flower.


-module(f).

%% ====================================================================
%% API functions
%% ====================================================================
-export([s/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================


s()->
	application:start(sasl), 
	application:start(gen_listener_tcp), application:start(regine),application:start(flower), flower_simple_switch:start_link().
