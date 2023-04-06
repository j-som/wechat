%%%=============================================================================
%%% @doc wechat_official_accounts_server 
%%% 微信公众账号
%%% @end
%%%=============================================================================
-module(wechat_official_accounts_server).
-author('J <j-som@foxmail.com>').

-behaviour(gen_server).

%% API
-export([start/0, stop/0, start_link/0]).
-export([get_token/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {dummy}).
-define(ETS_WOAP_ACCESS_TOKEN, woap_access_token).
start() ->
    wechat_auth_sup:start_child(?MODULE).

stop() ->
    gen_server:call(?MODULE, stop).

-spec get_token(AppId) -> {ok, AccessToken :: binary()} | {norfound, AppId} | {error, term()} when 
    AppId :: binary().
get_token(AppId) ->
    case get_token_from_cache(AppId) of 
        {ok, _AccessToken} = Ret ->
            Ret;
        _ ->
            gen_server:call(?MODULE, {get_token, AppId})
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    ets:new(?ETS_WOAP_ACCESS_TOKEN, [set, named_table, protected, {keypos,1}]),
    {ok, #state{dummy=1}}.

handle_call({get_token, AppId}, _From, State) ->
    % 先从缓存查
    % 过期或者没有，去公众号平台拿
    % 然后存缓存
    Reply = case get_token_from_cache(AppId) of 
        {ok, _AccessToken} = Ret ->
            Ret;
        _ ->
            case wechat_official_accounts:get_val(AppId, app_secret) of 
                false ->
                    {notfound, AppId};
                {app_secret, AppSecret} ->
                    case woap_api:get_app_access_token(AppId, AppSecret) of 
                        {ok, #{access_token := AccessToken, expires_in := ExpiresIn}} ->
                            ExpiresAt = os:system_time(second) + ExpiresIn,
                            set_token_to_cache(AppId, AccessToken, ExpiresAt),
                            {ok, AccessToken};
                        Error ->
                            Error
                    end
            end
    end,
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% internal function 

get_token_from_cache(AppId) ->
    case ets:lookup(?ETS_WOAP_ACCESS_TOKEN, AppId) of 
        [{AppId, AccessToken, ExpiresAt}] ->
            case os:system_time(second) < ExpiresAt of 
                true ->
                    {ok, AccessToken};
                false ->
                    expired
            end;
        _ ->
            undefined
    end.

set_token_to_cache(AppId, AccessToken, ExpiresAt) ->
    ets:insert(?ETS_WOAP_ACCESS_TOKEN, {AppId, AccessToken, ExpiresAt}).


