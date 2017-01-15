-module(mock_redis_server_tests).

-include("test.hrl").

setup() ->
    Res = mock_redis_server:start(),
    ?assertMatch(ok, Res),
    timer:sleep(100),
    ok.

cleanup(_) ->
    ?assertEqual(ok, mock_redis_server:stop()),
    ok.

start_stop_test() ->
    ?assertMatch(ok, mock_redis_server:start()),
    ?assertEqual(ok, mock_redis_server:stop()).

get_set_test_() ->
    ?FIXTURE(fun() ->
        Key = <<"test_key">>,
        Val = <<"test_val">>,
        SetResponse = mock_redis_server:q(mock, ["SET", Key, Val]),
        ?assertMatch({ok, _}, SetResponse),
        GetResponse = mock_redis_server:q(mock, ["GET", Key]),
        ?assertEqual({ok, Val}, GetResponse),
        ok
    end).

del_test_() ->
    ?FIXTURE(fun() ->
        Key = <<"test_key">>,
        Val = <<"test_val">>,
        SetResponse = mock_redis_server:q(mock, ["SET", Key, Val]),
        ?assertMatch({ok, _}, SetResponse),
        GetResponse = mock_redis_server:q(mock, ["GET", Key]),
        ?assertEqual({ok, Val}, GetResponse),
        DelResponse = mock_redis_server:q(mock, ["DEL", Key]),
        ?assertMatch({ok, _}, DelResponse),
        Get1Response = mock_redis_server:q(mock, ["GET", Key]),
        ?assertEqual({ok,undefined}, Get1Response),
        ok
    end).


rpush_test_() ->
    ?FIXTURE(fun() ->
        Key = <<"test_key">>,
        Val = <<"test_val">>,
        Response1 = mock_redis_server:q(mock, ["RPUSH", Key, Val]),
        ?assertMatch({ok, 1}, Response1),
        Response2 = mock_redis_server:q(mock, ["RPUSH", Key, Val]),
        ?assertMatch({ok, 2}, Response2),
        Response3 = mock_redis_server:q(mock, ["RPUSH", Key, Val]),
        ?assertMatch({ok, 3}, Response3),
        ok
    end).

lpop_test_() ->
    ?FIXTURE(fun() ->
        Key = <<"test_key">>,
        Response1 = mock_redis_server:q(mock, ["RPUSH", Key, 1]),
        ?assertEqual({ok, 1}, Response1),
        Response2 = mock_redis_server:q(mock, ["RPUSH", Key, 2]),
        ?assertEqual({ok, 2}, Response2),
        Response3 = mock_redis_server:q(mock, ["RPUSH", Key, 3]),
        ?assertEqual({ok, 3}, Response3),

        Response4 = mock_redis_server:q(mock, ["LPOP", Key]),
        ?assertEqual({ok, <<"1">>}, Response4),
        Response5 = mock_redis_server:q(mock, ["LPOP", Key]),
        ?assertEqual({ok, <<"2">>}, Response5),
        Response6 = mock_redis_server:q(mock, ["LPOP", Key]),
        ?assertEqual({ok, <<"3">>}, Response6),

        Response7 = mock_redis_server:q(mock, ["LPOP", Key]),
        ?assertEqual({ok, undefined}, Response7),
        ok
    end).

llen_test_() ->
    ?FIXTURE(fun() ->
        Key = <<"test_key">>,

        mock_redis_server:q(mock, ["RPUSH", Key, 1]),
        Response11 = mock_redis_server:q(mock, ["LLEN", Key]),
        ?assertEqual({ok, 1}, Response11),

        mock_redis_server:q(mock, ["RPUSH", Key, 2]),
        Response12 = mock_redis_server:q(mock, ["LLEN", Key]),
        ?assertEqual({ok, 2}, Response12),

        mock_redis_server:q(mock, ["RPUSH", Key, 3]),
        Response13 = mock_redis_server:q(mock, ["LLEN", Key]),
        ?assertEqual({ok, 3}, Response13),

        mock_redis_server:q(mock, ["LPOP", Key]),
        Response14 = mock_redis_server:q(mock, ["LLEN", Key]),
        ?assertEqual({ok, 2}, Response14),

        mock_redis_server:q(mock, ["LPOP", Key]),
        Response15 = mock_redis_server:q(mock, ["LLEN", Key]),
        ?assertEqual({ok, 1}, Response15),

        mock_redis_server:q(mock, ["LPOP", Key]),
        Response16 = mock_redis_server:q(mock, ["LLEN", Key]),
        ?assertEqual({ok, 0}, Response16),

        mock_redis_server:q(mock, ["LPOP", Key]),
        Response17 = mock_redis_server:q(mock, ["LLEN", Key]),
        ?assertEqual({ok, 0}, Response17),
        ok
    end).

hset_test_() ->
    ?FIXTURE(fun() ->
        Key = <<"test_hash_set">>,

        Response1 = mock_redis_server:q(mock, ["HSET", Key, 1, 1]),
        ?assertEqual({ok, 1}, Response1),

        Response2 = mock_redis_server:q(mock, ["HSET", Key, 2, 2]),
        ?assertEqual({ok, 1}, Response2),

        Response3 = mock_redis_server:q(mock, ["HSET", Key, 3, 3]),
        ?assertEqual({ok, 1}, Response3),

        {ok, Result} = mock_redis_server:q(mock, ["HKEYS", Key]),
        ?assert(is_list(Result)),
        SortedRes = lists:usort(Result),
        ?assertEqual([<<"1">>,<<"2">>,<<"3">>], SortedRes),

        {ok, Len} = mock_redis_server:q(mock, ["HLEN", Key]),
        ?assertEqual(3, Len),

        ok
    end).