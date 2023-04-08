%%%=============================================================================
%%% @doc woap_cowboy 接收处理微信服务器请求的http服务器
%%% @end
%%%=============================================================================
-module(woap_cowboy).
-author('J <j-som@foxmail.com>').

-export([start/0, start_tls/0, start_all/0]).
-export([stop/0, stop_tls/0, stop_all/0]).

%%------------------------------------------------------------------------------
%% @doc start
%% 启动http服务器
%% @end
%%------------------------------------------------------------------------------
start() ->
    application:ensure_all_started(cowboy),
    {ok, HttpOpts} = application:get_env(woap, http),
    cowboy:start_clear(woap_http, HttpOpts, #{
            env => #{dispatch => get_dispatch()},
            middlewares => [cowboy_router, req_print_h, cowboy_handler]
    }).

%%------------------------------------------------------------------------------
%% @doc start_tls
%% 启动https服务器
%% @end
%%------------------------------------------------------------------------------
start_tls() ->
    application:ensure_all_started(cowboy),
    {ok, HttpOpts} = application:get_env(woap, https),
    cowboy:start_tls(woap_https, HttpOpts, #{
            env => #{dispatch => get_dispatch()},
            middlewares => get_middlewares()
    }).

%%------------------------------------------------------------------------------
%% @doc start_all
%% 启动http和https服务器
%% @end
%%------------------------------------------------------------------------------
start_all() ->
    {ok, _} = start(),
    {ok, _} = start_tls(),
    ignore.

stop() ->
    _ = cowboy:stop_listener(woap_http).

stop_tls() ->
    _ = cowboy:stop_listener(woap_https).

stop_all() ->
    stop(),
    stop_tls(),
    ok.


get_dispatch() ->
    cowboy_router:compile([
        {'_', [
        % {"/static/[...]", cowboy_static, {priv_dir, fehuang_server, "static"}},
        {"/[:appid]", woap_cowboy_h, []}
        ]}
    ]).

get_middlewares() ->
    [cowboy_router, req_print_h, cowboy_handler].