-module(poolboy_test).


%% API
-export([stop_normal/0, stop_error/0]).

-include("poolboy_test.hrl").

stop_normal() ->
    poolboy:transaction(
        ?POOL_NAME,
        fun(Worker) ->
            worker:stop_normal(Worker)
        end
    ).

stop_error() ->
    poolboy:transaction(
        ?POOL_NAME,
        fun(Worker) ->
            worker:stop_error(Worker)
        end
    ).

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        [
            ?_assertEqual(ok, stop_normal()),
            ?_assertEqual(error, stop_error()),
            fun test_double_normal/0,
            fun test_normal_error/0,
            fun test_error_normal/0,
            fun test_double_error/0
        ]
    }.

test_double_normal() ->
    ?assertEqual(ok, stop_normal()),
    ?assertEqual(ok, stop_normal()).

test_normal_error() ->
    ?assertEqual(ok, stop_normal()),
    ?assertEqual(error, stop_error()).

test_error_normal() ->
    ?assertEqual(error, stop_error()),
    ?assertEqual(ok, stop_normal()).

test_double_error() ->
    ?assertEqual(error, stop_error()),
    ?assertEqual(error, stop_error()),
    ?assertEqual(ok, stop_normal()).



setup() ->
    ?assertMatch({ok, _}, application:ensure_all_started(poolboy_test)).

teardown(ok) ->
    application:stop(poolboy_test).