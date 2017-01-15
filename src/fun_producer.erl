-module(fun_producer).

-behaviour(gen_server).

%% API
-export([start_link/1]).

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
-define(JITTER, 5).

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
    {ok, RateLimit} = application:get_env(fun_test, rate_limit),
    {ok, N} = application:get_env(fun_test, n),
    {ok, QueueKey} = application:get_env(fun_test, queuekey),
    Interval = floor((1000/RateLimit)*1000), %microseconds
    case application:get_env(fun_test, run_producer) of
        {ok,false} -> pass;
        _ -> erlang:send(self(), gen)
    end,
    {ok, #{redis_module => RedisModule,
           redis_pool => RedisPool,
           rate_limit => RateLimit,
           interval => Interval,
           m => 2,
           n => N,
           queuekey => QueueKey}}.

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
handle_info(gen, State) ->
    gen(State),
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
gen(#{m := M, n := N,
      interval := Interval, redis_module := RedisModule,
      redis_pool := RedisPool, queuekey := QueueKey} = State) ->
    T1 = ts_micro(cur_timestamp()),
    RedisModule:q(RedisPool, ["RPUSH", QueueKey, urand(M, N)]),
    Diff = ts_micro(cur_timestamp()) - T1,
    microsleep(Interval - Diff),
    gen(State).

-ifdef('ERLANG_OTP_VERSION_17').
urand(From, To) -> floor(random:uniform() * To) + From.
-else.
urand(From, To) -> floor(rand:uniform() * To) + From.
-endif.

floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) ->
    trunc(X).


add_microsec(Micro, {Mega0, Sec0, Micro0}) ->
    Micro1 = Micro0 + Micro,
    Sec1 = Sec0 + (Micro1 div 1000000),
    Mega1 = Mega0 + (Sec1 div 1000000),
    {Mega1, (Sec1 rem 1000000), (Micro1 rem 1000000)}.

busywait_until(Target, Loops) ->
    case cur_timestamp() of
        Now when Now >= Target ->
            {Now, Loops};
        _ ->
            erlang:yield(),
            busywait_until(Target, 1 + Loops)
    end.

microsleep(MicroSec) ->
    Target = add_microsec(MicroSec, cur_timestamp()),
    AdjMsec = MicroSec - ?JITTER,
    case AdjMsec > 10000 of
        true -> timer:sleep(AdjMsec div 1000);
        false -> ok
    end,
    {Finish, Loops} = busywait_until(Target, 1),
    {timer:now_diff(Finish, Target), Loops}.

-ifdef('ERLANG_OTP_VERSION_17').
cur_timestamp()-> erlang:now().
-else.
cur_timestamp() -> erlang:timestamp().
-endif.

ts_micro({MegaSecs,Secs,Usecs}) ->
    (MegaSecs * 1000000 + Secs) * 1000000 + Usecs.