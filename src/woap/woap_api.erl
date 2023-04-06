%%%=============================================================================
%%% @doc woap_api 
%%% 微信公众平台api
%%% @end
%%%=============================================================================
-module(woap_api).
-author('J <j-som@foxmail.com>').

-export([
    get_app_access_token/2,
    check_signture/4
]).


%%------------------------------------------------------------------------------
%% @doc Function name
%% 开发者通过检验signature对请求进行校验（下面有校验方式）。若确认此次GET请求来自微信服务器，请原样返回echostr参数内容，则接入生效，成为开发者成功，否则接入失败。加密/校验流程如下：
%% 1）将token、timestamp、nonce三个参数进行字典序排序
%% 2）将三个参数字符串拼接成一个字符串进行sha1加密
%% 3）开发者获得加密后的字符串可与signature对比，标识该请求来源于微信
%% @end
%%------------------------------------------------------------------------------
-spec check_signture(Signature, Timestamp, Nonce, Token) -> true|false when 
    Signature :: unicode:unicode_binary(),
    Timestamp :: unicode:unicode_binary(),
    Nonce :: unicode:unicode_binary(),
    Token :: unicode:unicode_binary()
.
check_signture(Signature, Timestamp, Nonce, Token) ->
    TmpStr = list_to_binary(lists:sort([Token, Timestamp, Nonce])),
    Signature1 = binary:encode_hex(crypto:hash(sha, TmpStr)),
    Signature =:= Signature1.

%%------------------------------------------------------------------------------
%% @doc get_app_access_token 
%% 错误码	错误码取值	    解决方案
%% -1	    system error	系统繁忙，此时请开发者稍候再试
%% 40001	invalid credential  access_token isinvalid or not latest	获取 access_token 时 AppSecret 错误，或者 access_token 无效。请开发者认真比对 AppSecret 的正确性，或查看是否正在为恰当的公众号调用接口
%% 40013	invalid appid	不合法的 AppID ，请开发者检查 AppID 的正确性，避免异常字符，注意大小写
%% @end
%%------------------------------------------------------------------------------
-spec get_app_access_token(AppId, AppSecret) -> {ok, Ret} | {error, term()} when 
    AppId :: unicode:chardata(),
    AppSecret :: unicode:chardata(),
    Ret :: #{access_token => AccessToken, expires_in => ExpiresTime},
    AccessToken :: unicode:unicode_binary(),
    ExpiresTime :: integer()
.
get_app_access_token(AppId, AppSecret) ->
    Path = "/cgi-bin/token",
    Gets = [{"grant_type", "client_credential"},{"appid", AppId}, {"secret", AppSecret}],
    case woap_util:http_get(Path, Gets) of 
        {ok, {_HttpStatus, _HttpHeaders, JsonStr}} ->
            {ok, jsx:decode(unicode:characters_to_binary(JsonStr), [{labels,atom},return_maps])};
        Err ->
            Err
    end.


