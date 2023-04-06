%%%=============================================================================
%%% @doc woap_cowboy 接收处理微信服务器请求的http服务器
%%% @end
%%%=============================================================================
-module(woap_cowboy).
-author('J <j-som@foxmail.com>').

-export([start/0, stop/0, get_dispatch/0]).

start() ->
    application:ensure_all_started(cowboy),
    io:format("listenAndServe~n"),
    {ok, HttpOpts} = application:get_env(woap, http),
    cowboy:start_clear(http, HttpOpts, #{
            env => #{dispatch => get_dispatch()},
            middlewares => [cowboy_router, req_print_h, cowboy_handler]
    }).

stop() ->
    ok = cowboy:stop_listener(http).

get_dispatch() ->
    cowboy_router:compile([
        {'_', [
        % {"/static/[...]", cowboy_static, {priv_dir, fehuang_server, "static"}},
        {"/[:appid]", woap_cowboy_h, []}
        ]}
    ]).