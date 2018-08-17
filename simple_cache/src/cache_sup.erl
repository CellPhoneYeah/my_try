-module(cache_sup).

-behaviour(supervisor).

-export([
         init/1
        ]).

-export([
         start_link/0
        ]).

-export([
         start_cache/1
        ]).

%%% =====
%%% API
%%% =====
start_cache(Data) ->
    supervisor:start_child(?MODULE, [Data]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlag = #{
      strategy => simple_one_for_one,
      intensity => 10,
      period => 10
     },
    CacheEle = #{
      id => cache_element,
      start => {cache_element, start_link, []},
      restart => temporary,
      shutdown => brutal_kill,
      type => worker,
      modules => [cache_element]
     },
    ChildSpec = [CacheEle],
    {ok, {SupFlag, ChildSpec}}.
