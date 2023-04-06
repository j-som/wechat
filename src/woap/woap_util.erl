%%%=============================================================================
%%% @doc woap_util 工具集
%%% @end
%%%=============================================================================
-module(woap_util).
-author('J <j-som@foxmail.com>').

-export([http_get/2]).

http_get(Path, Gets) ->
    URIMap = #{
        path => Path,
        scheme => "https",
        query => uri_string:compose_query(Gets)
    },
    get_from_domain(woap_hosts:get_all(), URIMap).

get_from_domain([Domain|T], URIMap) ->
    Uri = uri_string:recompose(URIMap#{host => Domain}),
    case httpc:request(Uri) of 
        {ok, Result} ->
            {ok, Result};
        {error, _Error} ->
            get_from_domain(T, URIMap)
    end;
get_from_domain([], _URIMap) -> {error, no_available_weixin_host}.