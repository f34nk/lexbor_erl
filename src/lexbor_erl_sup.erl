-module(lexbor_erl_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% Public API
%% ===================================================================

-spec start_link(pos_integer()) -> {ok, pid()} | {error, term()}.
start_link(PoolSize) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [PoolSize]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([PoolSize]) ->
    SupFlags = #{
        strategy => one_for_one,     % Restart only failed child
        intensity => 5,              % Max 5 restarts
        period => 10                 % In 10 seconds
    },
    
    % Pool manager coordinates workers but doesn't manage them
    PoolSpec = #{
        id => lexbor_erl_pool,
        start => {lexbor_erl_pool, start_link, [PoolSize]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [lexbor_erl_pool]
    },
    
    % Each worker as a separate supervised child
    WorkerSpecs = [
        #{
            id => {lexbor_erl_worker, I},
            start => {lexbor_erl_worker, start_link, [I]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [lexbor_erl_worker]
        } || I <- lists:seq(1, PoolSize)
    ],
    
    % Start pool manager first, then workers
    {ok, {SupFlags, [PoolSpec | WorkerSpecs]}}.
