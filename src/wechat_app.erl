%%%-------------------------------------------------------------------
%% @doc wechat public API
%% @end
%%%-------------------------------------------------------------------

-module(wechat_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:ensure_all_started(lager),
    wechat_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
