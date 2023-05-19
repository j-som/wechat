%%%=============================================================================
%%% @doc woap_cowboy_h 
%%% 微信服务器请求我方服务器的处理
%%% @end
%%%=============================================================================
-module(woap_cowboy_h).
-author('J <j-som@foxmail.com>').

%% Standard callbacks.
-export([init/2]).


init(Req, Opts) ->
    AppId = cowboy_req:binding(appid, Req),
    ReqN =
        case wechat_official_accounts:get(AppId) of
            undefined ->
                cowboy_req:reply(501, Req);
            Account ->
                case cowboy_req:method(Req) of
                    <<"GET">> ->
                        on_get(Account, Req);
                    <<"POST">> ->
                        on_post(Account, Req);
                    _ ->
                        Req1 = cowboy_req:set_resp_header(<<"allow">>, <<"GET, POST">>, Req),
                        cowboy_req:reply(405, Req1)
                end
        end,
    {ok, ReqN, Opts}.


on_get(Account, Req) ->
    case cowboy_req:match_qs([signature, timestamp, nonce, echostr], Req) of
        #{
            signature := Signature,
            timestamp := Timestamp,
            nonce := Nonce,
            echostr := Echostr
        } ->
            {token, Token} = wechat_official_accounts:get_val(Account, token),
            case woap_api:check_signture(Signature, Timestamp, Nonce, Token) of
                true ->
                    Req1 = cowboy_req:set_resp_body(Echostr, Req),
                    cowboy_req:reply(200, Req1);
                false ->
                    cowboy_req:reply(401, Req)
            end;
        _ ->
            cowboy_req:reply(501, Req)
    end.

on_post(Account, Req) ->
    XmlBin = cowboy_req:read_body(Req),
    Data = woap_xml:decode(XmlBin),
    {Resp, Args} =
        case application:get_env(woap, resp) of
            undefined ->
                {woap_resp, []};
            {ok, Ret} ->
                Ret
        end,
    case Resp:handle(Data, Account, Args) of
        {ok, Msg} ->
            Req1 = cowboy_req:set_resp_body(Msg, Req),
            cowboy_req:reply(200, Req1);
        _ ->
            cowboy_req:reply(200, Req)
    end.