%%%=============================================================================
%%% @doc woap_cache 
%%% 储存来自微信公众平台的数据，减少调用微信服务器
%%% @end
%%%=============================================================================
-module(woap_cache).
-author('J <j-som@foxmail.com>').
-compile({no_auto_import,[get/1]}).
-compile({no_auto_import,[set/2]}).
-include("woap_cache.hrl").
-define(ETS_NAME, ?MODULE).
-define(POOL_NAME, woap_cache_pool).

-type cache_key() :: binary(). 
-type cache_value() :: {ok,binary()}|undefined|expired.



-behaviour(gen_server).
-record(state, {storage_provider}).
-record(woap_cache, {
    key :: cache_key(), 
    value = undefined :: binary(), 
    expires_at = 0
}).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/1]).

-export([get/1,get/2]).
-export([set/2, set/3, set/4]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get(Type::binary(), Sub :: binary()|integer()|iolist()|atom()) -> Value :: cache_value().
get(Type, Sub) ->
    get(as_key(Type, Sub)).

-spec get(TableKey :: cache_key()) -> Value :: cache_value().
get(TableKey) ->
    case get_from_cache(TableKey) of 
        {ok, _Value} = Ret ->
            Ret;
        _ ->
            gen_server:call(?MODULE, {get, TableKey})
    end.

set(TableKey, Value) -> set(TableKey, Value, os:system_time(second) + ?DEFAULT_EXPIRES).
set(TableKey, Value, ExpiresAt) ->
    gen_server:cast(?MODULE, {set, TableKey, Value, ExpiresAt}).

set(Type, Sub, Value, ExpiresAt) ->
    set(as_key(Type, Sub), Value, ExpiresAt).

init([]) ->
    ets:new(?ETS_NAME, [set, named_table, protected, {keypos,#woap_cache.key}]),
    case application:get_env(woap, storage_provider) of 
        {ok, {mysql, ConnOpts, PoolOps}} ->
            application:ensure_all_started(mysql_poolboy),
            mysql_poolboy:add_pool(?POOL_NAME, PoolOps, ConnOpts),
            {ok, #state{storage_provider = mysql}};
        _ ->
            {ok, #state{storage_provider = none}}
    end.

handle_call({get, TableKey}, _From, State) ->
    case get_from_cache(TableKey) of 
        {ok, _} = Reply ->
            ok;
        expired = Reply ->
            ok;
        undefined ->
            case load_cache(State#state.storage_provider, TableKey) of
                {ok, _} = Reply ->
                    ok;
                _ ->
                    Reply = undefined
            end
    end,
    {reply, Reply, State};


handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({set, TableKey, Value, ExpiresAt}, State) ->
    Cache = #woap_cache{key = TableKey, value = Value, expires_at = ExpiresAt},
    ets:insert(?ETS_NAME, Cache),
    NewState = save_cache(State, Cache),
    io:format("sset ~s ~s", [TableKey, Value]),
    {noreply, NewState};


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_State) ->
    ok.



get_from_cache(TableKey) ->
    case ets:lookup(?ETS_NAME, TableKey) of 
        [#woap_cache{value = Value, expires_at = ExpiresAt}] ->
            case check_expired(ExpiresAt) of 
                false ->
                    {ok, Value};
                true ->
                    expired
            end;
        [] ->
            undefined
    end.

load_cache(mysql, TableKey) ->
    case mysql_poolboy:query(?POOL_NAME, "SELECT `value`, `expires_at` FROM `woap_cache` WHERE `key`= ?", [TableKey]) of
        {ok, _Fields, [ValBin, ExpiresAt]} ->
            Cache = #woap_cache{key= TableKey, value = ValBin, expires_at = ExpiresAt},
            ets:insert(woap_cache, Cache),
            case check_expired(ExpiresAt) of 
                true ->
                    expired;
                false ->
                    {ok, ValBin}
            end;
        _ ->
            Cache = #woap_cache{key= TableKey, value = undefined},
            ets:insert(woap_cache, Cache),
            false
    end;
load_cache(_, _TableKey) ->
    false.

save_cache(#state{storage_provider = mysql} = State, Cache) ->
    #woap_cache{key= TableKey, value = ValBin, expires_at = ExpiresAt} = Cache,
    Ret = mysql_poolboy:query(?POOL_NAME, "REPLACE INTO `woap_cache` (`key`,`value`,`expires_at`) VALUES(?,?,?)", [TableKey, ValBin, ExpiresAt]),
    io:format("save_cache ~s ~w~n", [ValBin, Ret]),
    State;

save_cache(State, _Cache) ->
    State.

check_expired(ExpiresAt) ->
    os:system_time(second) > ExpiresAt.

as_key(Type, Sub) ->
    SubBin = to_bin(Sub),
    <<Type/binary, "-", SubBin/binary>>.

to_bin(Sub) when is_binary(Sub) ->
    Sub;
to_bin(Sub) when is_integer(Sub) ->
    integer_to_binary(Sub);
to_bin(Sub) when is_list(Sub) ->
    list_to_binary(Sub);
to_bin(Sub) when is_atom(Sub) ->
    atom_to_binary(Sub).