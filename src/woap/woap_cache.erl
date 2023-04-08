%%%=============================================================================
%%% @doc woap_cache 
%%% 储存来自微信公众平台的数据，减少调用微信服务器
%%% @end
%%%=============================================================================
-module(woap_cache).
-author('J <j-som@foxmail.com>').
-compile({no_auto_import,[get/1]}).
-compile({no_auto_import,[set/2]}).


-behaviour(gen_server).
-record(state, {storage_provider}).
-record(woap_cache, {
    key = undefined, 
    value = undefined, 
    expries_at = 0
}).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/1]).
-define(ETS_NAME, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



init([]) ->
    case application:get_env(woap, storage_provider) of 
        {ok, {mysql, ConnOpts, PoolOps}} ->
            mysql_poolboy:add_pool(?MODULE, PoolOps, ConnOpts),
            {ok, #state{storage_provider = mysql}};
        _ ->
            ets:new(?ETS_NAME, [set, named_table, protected, {keypos,1}]),
            {ok, #state{storage_provider = ets}}
    end.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_State) ->
    ok.
