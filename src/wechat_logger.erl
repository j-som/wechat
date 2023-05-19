%%%-------------------------------------------------------------------
%%% @author j-som@foxmail.com
%%% @copyright (C) 2023, Zhakun Game
%%% @doc
%%% 日志处理
%%% @end
%%% Created : 18. 5月 2023 20:34
%%%-------------------------------------------------------------------
-module(wechat_logger).
-author("j-som@foxmail.com").

-compile(inline).
%% API
-export([warning/2, info/2]).

warning(Format, Args) ->
    lager:warning(Format, Args).

info(Format, Args) ->
    lager:info(Format, Args).
