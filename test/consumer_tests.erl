-module(consumer_tests).

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
            {resultsetkey, "res"}
        ]}
    ]),
    ok.


cleanup(_)->
    ?STOP_APPS(?APPS),
    mock_redis_server:stop(),
    ok.

prime_test()->
    CheckList = [
        {-1, false},
        {0, false},
        {1, false},
        {2, true},
        {3, true},
        {4, false},
        {5, true},
        {6, false},
        {7, true},
        {8, false},
        {9, false},
        {10, false},
        {11, true},
        {12, false},
        {13, true},
        {14, false},
        {15, false}
    ],
    [ ?assertEqual(Res, fun_consumer:is_prime(N)) ||{N, Res} <- CheckList ],
    ok.

main_test_()->
    ?FIXTURE(fun()->
                timer:sleep(1000),
                {ok,Len} = mock_redis_server:q(mock, ["LLEN", "gen"]),
                ?assert(10 < Len),
                {ok, ResLen} = mock_redis_server:q(mock, ["HLEN", "res"]),
                ?assert(0 < ResLen),
                ok
             end).