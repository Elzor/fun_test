-module(producer_tests).

-include("test.hrl").

-define(APPS, [ fun_test ]).
-define(RL, 500).

setup()->
    mock_redis_server:start(),
    ?START_APPS(?MODULE, ?APPS, [
        {fun_test, [
            {redis_module, mock_redis_server},
            {redis_pool, mock},

            {rate_limit, ?RL},
            {n, 100},
            {queuekey, "gen"},
            {resultsetkey, "res"},
            {run_consumer, false}
        ]}
    ]),
    ok.


cleanup(_)->
    ?STOP_APPS(?APPS),
    mock_redis_server:stop(),
    ok.

main_test_()->
    ?FIXTURE(fun()->
                application:stop(fun_test),
                application:start(fun_test),
                timer:sleep(1000),
                {ok,Len} = mock_redis_server:q(mock, ["LLEN", "gen"]),
                ?assert(check_limit(?RL, Len)),
                application:start(fun_test),
                timer:sleep(1000),
                application:stop(fun_test),
                {ok,Len2} = mock_redis_server:q(mock, ["LLEN", "gen"]),
                ?assert(check_limit(?RL*2, Len2)),
                application:start(fun_test),
                ok
             end).

sec3_test_()->
    ?FIXTURE(fun()->
                application:stop(fun_test),
                application:start(fun_test),
                timer:sleep(1000),
                {ok, Len} = mock_redis_server:q(mock, ["LLEN", "gen"]),
                ?assert(check_limit(?RL, Len)),
                timer:sleep(1000),
                {ok,Len2} = mock_redis_server:q(mock, ["LLEN", "gen"]),
                ?assert(check_limit(?RL*2, Len2)),
                timer:sleep(1000),
                {ok,Len3} = mock_redis_server:q(mock, ["LLEN", "gen"]),
                ?assert(check_limit(?RL*3, Len3)),
                ok
             end).

check_limit(Limit, Value) ->
    (Limit - Limit*0.05) < Value andalso Value < (Limit + Limit*0.05).
