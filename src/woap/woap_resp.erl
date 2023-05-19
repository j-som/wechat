%%%-------------------------------------------------------------------
%%% @author j-som@foxmail.com
%%% @copyright (C) 2023, Zhakun Game
%%% @doc
%%%     处理来自微信服务器的请求
%%% @end
%%% Created : 18. 5月 2023 14:25
%%%-------------------------------------------------------------------
-module(woap_resp).
-author("j-som@foxmail.com").
-include("woap_xml.hrl").

%% API
-export([handle/3]).

-spec handle(Data :: #woap_req_data{}, wechat_official_accounts:woap_account(), any()) -> ok | {ok, Resp :: binary()}.
handle(_Data, _Account, _Args) ->
    wechat_logger:info("recieve woap request ~w", [_Data]),
    ok.
