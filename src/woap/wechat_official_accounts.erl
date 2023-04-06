%%%=============================================================================
%%% @doc wechat_official_accounts 
%%% 微信公众号信息
%%% @end
%%%=============================================================================
-module(wechat_official_accounts).
-author('J <j-som@foxmail.com>').

-export([get/1, get_val/2]).

%%------------------------------------------------------------------------------
%% @doc get
%% 获取公众账号信息
%% @end
%%------------------------------------------------------------------------------
-spec get(AppId :: binary()) -> [{Key::atom(), Value::binary()}]|undefined.
get(<<"wxa62da9123a9a411b">>) ->
    [
        {app_id, <<"wxa62da9123a9a411b">>},
        {app_secret, <<"15b02b6558efaa48cd3a9be42669433b">>},
        {token, <<"2fd7574d9eccc974590633d5398e448e">>}
    ];
get(_) ->
    undefined.

%%------------------------------------------------------------------------------
%% @doc get_val
%% 获取公众账号信息中某个值
%% @end
%%------------------------------------------------------------------------------
-spec get_val(AppId::binary(), Key::atom()) -> {Key::binary(), Value::binary()}|false.
get_val(AppId, Key) ->
    lists:keyfind(Key, 1, ?MODULE:get(AppId)).