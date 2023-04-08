%%%=============================================================================
%%% @doc woap_sup 
%%% @end
%%%=============================================================================
-module(woap_sup).
-author('J <j-som@foxmail.com>').


-behaviour(supervisor).
-define(SERVER, ?MODULE).

-export([init/1]).

-export([start_link/0]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
    

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 10},
        ChildSpecs = [
        #{
            id => wechat_official_accounts_server,
            start => {wechat_official_accounts_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            typer => worker,
            modules => [wechat_official_accounts_server]
        },
        #{
            id => woap_cache,
            start => {woap_cache, start_link, []},
            restart => permanent,
            shutdown => 5000,
            typer => worker,
            modules => [woap_cache]
        },
        #{
            id => woap_cowboy,
            start => {woap_cowboy, start, []},
            restart => permanent,
            shutdown => 5000,
            typer => worker,
            modules => [woap_cowboy]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.