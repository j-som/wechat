%%%=============================================================================
%%% @doc wechat_official_accounts 
%%% 微信公众号信息
%%% @end
%%%=============================================================================
-module(wechat_official_accounts).
-author('J <j-som@foxmail.com>').

-type woap_account() :: [{Key::atom(), Value::binary()}].
-export_type([woap_account/0]).

-export([get/1, get_val/2]).

%%------------------------------------------------------------------------------
%% @doc get
%% 获取公众账号信息
%% @end
%%------------------------------------------------------------------------------
-spec get(AppId :: binary()) -> woap_account()|undefined.
get(<<"wxa62da9123a9a411b">>) ->
    [
        {app_id, <<"wxa62da9123a9a411b">>},
        {app_secret, <<"15b02b6558efaa48cd3a9be42669433b">>},
        {token, <<"2fd7574d9eccc974590633d5398e448e">>},
        {uid, <<"gh_e136c6e50636">>}
    ];
get(_) ->
    undefined.

%%------------------------------------------------------------------------------
%% @doc get_val
%% 获取公众账号信息中某个值
%% @end
%%------------------------------------------------------------------------------
-spec get_val(AppId::binary()|woap_account(), Key::atom()) -> {Key::binary(), Value::binary()}|false.
get_val(AppId, Key) when is_binary(AppId) ->
    lists:keyfind(Key, 1, ?MODULE:get(AppId));

get_val(Account, Key) when is_list(Account) ->
    lists:keyfind(Key, 1, Account).