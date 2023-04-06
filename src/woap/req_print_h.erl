%%%=============================================================================
%%% @doc req_print_h 打印请求
%%% @end
%%%=============================================================================
-module(req_print_h).
-author('J <j-som@foxmail.com>').


-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
	logger:debug("<<< ~s", [print_r(Req)]),
	{ok, Req, Env}.

print_r(Req) ->
    cowboy_req:uri(Req).