-module(mock_redis_server).

-behaviour(gen_server).

-include("log.hrl").
-include("test.hrl").

%% API
-export([start/0, stop/0]).

-export([q/2, q/3]).

%% gen_server callbacks
-export([init/1, handle_cast/2, terminate/2,
         code_change/3, handle_call/3, handle_info/2]).

%%=============================================================================
%% API
%%-----------------------------------------------------------------------------
start() ->
    start_worker(),
    ok.

stop() ->
    stop_worker(),
    ok.

% function for emulate redis library
q(Pool, Params) ->
    q(Pool, Params, 50000).

q(error_pool, _Params, _Timeout) ->
    {error, no_connection};
q(_Pool, Params, Timeout) ->
    % ?LOG("~p,~p,~p", [_Pool, Params, Timeout]),
    Response = process_command(Params, Timeout),
    {ok, Response}.


%%=============================================================================
%% gen_server callbacks
%%-----------------------------------------------------------------------------
-record(state, {values = dict:new()}).

init(_) ->
    pg2:create(?MODULE),
    pg2:join(?MODULE, self()),
    {ok, #state{}}.

terminate(Reason, _State) ->
    ?LOG("mock_redis_server:terminate(), Reason: ~p", [Reason]),
    pg2:leave(?MODULE, self()),
    ok.

handle_cast(stop, _State) ->
    {stop, normal, _State}.


handle_call({process_command, ["GET", Key]}, _From, #state{values = Dict} = State) ->
    case dict:find(Key, Dict) of
        {ok, V} -> {reply, to_binary(V), State};
        error -> {reply, undefined, State}
    end;

handle_call({process_command, ["HSET", Key, SubKey, Value]}, _From, #state{values = Dict} = State) ->
    EntityValue = case dict:find(Key, Dict) of
        {ok, V} -> dict:store(to_binary(SubKey), to_binary(Value), V);
        error -> dict:store(to_binary(SubKey), to_binary(Value), dict:new())
    end,
    {reply, 1, State#state{values = dict:store(Key, EntityValue, Dict)}};

handle_call({process_command, ["HKEYS", Key]}, _From, #state{values = Dict} = State) ->
    EntityValue = case dict:find(Key, Dict) of
        {ok, V} -> dict:fetch_keys(V);
        error -> []
    end,
    {reply, EntityValue, State};

handle_call({process_command, ["HLEN", Key]}, _From, #state{values = Dict} = State) ->
    EntityValue = case dict:find(Key, Dict) of
        {ok, V} -> dict:size(V);
        error -> 0
    end,
    {reply, EntityValue, State};

handle_call({process_command, ["RPUSH", Key, Value]}, _From, #state{values = Dict} = State) ->
    EntityValue = case dict:find(Key, Dict) of
        {ok, V} -> queue:in(to_binary(Value), V);
        error -> queue:in(to_binary(Value), queue:new())
    end,
    Len = queue:len(EntityValue),
    {reply, Len, State#state{values = dict:store(Key, EntityValue, Dict)}};

handle_call({process_command, ["LPOP", Key]}, _From, #state{values = Dict} = State) ->
    case dict:find(Key, Dict) of
        {ok, V} ->
            EntityValue = case queue:out(V) of
                {{value, Item}, Q} -> Q;
                {empty, Q} -> Item = undefined, Q
            end,
            {reply, Item, State#state{values = dict:store(Key, EntityValue, Dict)}};
        error -> {reply, undefined, State}
    end;

handle_call({process_command, ["LLEN", Key]}, _From, #state{values = Dict} = State) ->
    case dict:find(Key, Dict) of
        {ok, V} ->
            Len = queue:len(V),
            {reply, Len, State};
        error ->
            {reply, 0, State}
    end;

handle_call({process_command, ["SET", Key, Value]}, _From, #state{values = Dict} = State) ->
    {reply, <<"OK">>, State#state{values = dict:store(Key, to_binary(Value), Dict)}};


handle_call({process_command, ["DEL" | Keys]}, _From, #state{values = Dict} = State) ->
    ?assertMatch([ _ | _ ], Keys),
    CountBefore = dict:size(Dict),
    Dict1 = lists:foldl(fun(Key, Acc) -> dict:erase(Key, Acc) end, Dict, Keys),
    CountDeleted =  CountBefore - dict:size(Dict1),
    Result = integer_to_list(CountDeleted),
    {reply, to_binary(Result), State#state{values = Dict1}};


handle_call({process_command, ["KEYS" , KeysExp]}, _From, #state{values = Dict} = State) ->
    ?assert(string_x:ends_with(KeysExp, "*")),
    KeysCommonPrefix = string:substr(KeysExp, 1, length(KeysExp) - 1),
    ?assertNot(string_x:ends_with(KeysCommonPrefix, "*")),
    AllKeys = dict:fetch_keys(Dict),
    MatchedKeys = lists:foldl(fun(Key, Res) ->
          ListKey = if is_binary(Key) -> binary_to_list(Key);
                       true -> Key
          end,
          case string_x:starts_with(ListKey, KeysCommonPrefix) of
              true -> [Key | Res];
              false -> Res
          end
    end, [], AllKeys),
    {reply, MatchedKeys, State};


handle_call(_Request, _From, State) ->
    ?debugFmt("pid: ~p, Unexpected call request: ~p~n   State: ~p", [self(),_Request,State]),
    {reply, ok, State}.

handle_info(_Msg, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%=============================================================================
%% Helpers
%%-----------------------------------------------------------------------------

start_worker() ->
    gen_server:start(?MODULE, [], []).

stop_worker() ->
    Pid = get_pid(),
    exit(Pid, kill).

get_pid() ->
    case pg2:get_closest_pid(?MODULE) of
        {error, _ } -> undefined;
        Pid when is_pid(Pid) -> Pid
    end.

process_command(Params, Timeout) ->
    Pid = get_pid(),
    Result = gen_server:call(Pid, {process_command, Params}, Timeout),
    Result.

to_binary(Data) when is_list(Data) -> list_to_binary(Data);
to_binary(Data) when is_integer(Data) -> list_to_binary(integer_to_list(Data));
to_binary(Data) when is_binary(Data) -> Data.