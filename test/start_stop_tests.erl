-module(start_stop_tests).

-include("test.hrl").

-define(APPS, [ ]).

setup()->
    mock_redis_server:start(),
    ?START_APPS(?MODULE, ?APPS, [ ]),
    ok.


cleanup(_)->
    ?STOP_APPS(?APPS),
    mock_redis_server:stop(),
    ok.

main_test_()->
    ?FIXTURE(fun()->
                ?assertEqual(ok, application:load(fun_test)),
                AppParams = [
                    {redis_module, mock_redis_server},
                    {redis_pool, mock},

                    {rate_limit, 500},
                    {n, 100},
                    {queuekey, "gen"},
                    {resultsetkey, "res"}
                ],
                lists:foreach(fun({Key, Value}) -> ?assertEqual(ok, application:set_env(fun_test, Key, Value)) end,AppParams),
                ?assertEqual(ok, application:start(fun_test)),
                timer:sleep(250),
                ?assertEqual({error,{already_started, fun_test}},
                             application:start(fun_test)),
                ?assertEqual(ok, application:stop(fun_test)),
                ?assertEqual(ok, application:unload(fun_test)),
                ok
             end).