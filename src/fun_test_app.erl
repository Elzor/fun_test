%%%-------------------------------------------------------------------
%% @doc fun_test public API
%% @end
%%%-------------------------------------------------------------------

-module(fun_test_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    case application:get_env(fun_test, redis_module) of
        {ok, eredis_pool} -> eredis_pool:start();
        _Else -> mocking_mode
    end,
    fun_test_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
