-module(poolboy_test).


%% API
-export([stop_normal/0, stop_error/0, stop_normal/1, stop_error/1]).

-include("poolboy_test.hrl").

-define(DEFAULT_TIMEOUT, 5000).

stop_normal(Timeout) ->
    execute_on_worker(fun worker:stop_normal/1, Timeout).

stop_normal() ->
    stop_normal(?DEFAULT_TIMEOUT).

stop_error(Timeout) ->
    execute_on_worker(fun worker:stop_error/1, Timeout).

stop_error() ->
    stop_error(?DEFAULT_TIMEOUT).



execute_on_worker(Fun, Timeout) ->
    try
        poolboy:transaction(
            ?POOL_NAME,
            fun(Worker) ->
                Fun(Worker)
            end,
            Timeout
        )
        catch
            exit:{timeout,_} ->
                timeout
    end.

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

failing_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        [
            ?_assertEqual({ready,1,0,0}, poolboy:status(?POOL_NAME)),
            ?_assertEqual(timeout, stop_normal(0)),
            ?_assertEqual({full,0,0,1}, poolboy:status(?POOL_NAME)), %% WHY?
            ?_assertEqual(timeout, stop_normal(4000))  %% After timeout in poolboy:transaction every call times out.
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