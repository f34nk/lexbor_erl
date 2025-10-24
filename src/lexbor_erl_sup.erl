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
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    
    % Single child: the pool manager
    % The pool manager itself will start and manage worker processes
    ChildSpecs = [
        #{
            id => lexbor_erl_pool,
            start => {lexbor_erl_pool, start_link, [PoolSize]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [lexbor_erl_pool]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
