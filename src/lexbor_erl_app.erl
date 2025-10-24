-module(lexbor_erl_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    % Get pool size from configuration, default to number of schedulers
    PoolSize = application:get_env(lexbor_erl, pool_size, 
                                   erlang:system_info(schedulers_online)),
    lexbor_erl_sup:start_link(PoolSize).

stop(_State) ->
    ok.
