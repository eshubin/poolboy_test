-module(poolboy_test_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("poolboy_test.hrl").
%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    PoolArgs = [
                {name, {local, ?POOL_NAME}},
                {worker_module, worker},
                {size, 1}, {max_overflow, 0}
                ],
    PoolSpec = poolboy:child_spec(?POOL_NAME, PoolArgs),

    {ok, {SupFlags, [PoolSpec]}}.

