-module(fun_consumer).

-behaviour(gen_server).

%% API
-export([start_link/1, is_prime/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% includes
-include("log.hrl").

%% defines

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(_Args) ->
    {ok, RedisModule} = application:get_env(fun_test, redis_module),
    {ok, RedisPool} = application:get_env(fun_test, redis_pool),
    {ok, QueueKey} = application:get_env(fun_test, queuekey),
    {ok, ResultsetKey} = application:get_env(fun_test, resultsetkey),
    case application:get_env(fun_test, run_consumer) of
        {ok, false} -> pass;
        _Else -> erlang:send(self(), consume)
    end,
    {ok, #{redis_module => RedisModule,
           redis_pool => RedisPool,
           queuekey => QueueKey,
           resultsetkey => ResultsetKey}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
    ?ERR("unhandled call: ~p", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    ?ERR("unhandled cast: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(consume, State) ->
    check(State),
    {noreply, State};

handle_info(Info, State) ->
    ?ERR("unhandled info: ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
check(#{redis_module := RedisModule, redis_pool := RedisPool,
        queuekey := QueueKey, resultsetkey := ResultsetKey} = State) ->
    case RedisModule:q(RedisPool, ["LPOP", QueueKey]) of
        {ok, BinInt} when is_binary(BinInt) ->
            Number = binary_to_integer(BinInt),
            case is_prime(Number) of
                true -> RedisModule:q(RedisPool, ["HSET", ResultsetKey, Number, Number]);
                _ -> skip
            end;
        _Else ->
            timer:sleep(100)
    end,
    check(State).

divisors(N) -> [ X || X <- lists:seq(1,N), (N rem X)==0 ].
is_prime(N) when N =< 0; N == 1 -> false;
is_prime(2) -> true;
is_prime(N) -> divisors(N) == [1,N].