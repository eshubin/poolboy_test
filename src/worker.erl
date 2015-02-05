-module(worker).

-behaviour(gen_server).

%% API
-export([start_link/1, stop_normal/1, stop_error/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link([]) ->
    gen_server:start_link(?MODULE, [], []).

stop_normal(Pid) ->
    gen_server:call(Pid, normal).

stop_error(Pid) ->
    gen_server:call(Pid, error).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(normal, _From, State) ->
    {stop, normal, ok, State};
handle_call(error, _From, State) ->
    {stop, error, error, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
