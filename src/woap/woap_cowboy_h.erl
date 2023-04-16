%%%=============================================================================
%%% @doc woap_cowboy_h 
%%% 微信服务器请求我方服务器的处理
%%% @end
%%%=============================================================================
-module(woap_cowboy_h).
-author('J <j-som@foxmail.com>').

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([paste_text/2]).


init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[
        <<"GET">>, 
        <<"POST">>
    ], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"plain">>, []}, paste_text}
	], Req, State}.

resource_exists(Req, State) ->
    case cowboy_req:binding(appid, Req) of
        undefined ->
            {false, Req, State};
        AppId ->
            %% {ok, Token} = wechat_official_accounts_server:get_token(AppId),
            {token, Token} = wechat_official_accounts:get_val(AppId, token),
            {true, Req, [AppId, Token]}
    end.
paste_text(Req, [_AppId, Token]) ->
    case cowboy_req:match_qs([signature, timestamp, nonce, echostr], Req) of 
        #{
            signature := Signature,
            timestamp := Timestamp,
            nonce     := Nonce,
            echostr   := Echostr
        } ->
            case woap_api:check_signture(Signature, Timestamp, Nonce, Token) of 
                true ->
                    {Echostr, Req, []};
                false ->
                    {<<"false">>, Req, []}
            end;
        _ ->
            {<<"false">>, Req, []}
    end.