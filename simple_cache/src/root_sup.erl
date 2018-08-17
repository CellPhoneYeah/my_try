-module(root_sup).

-behaviour(supervisor).

-export([
         start_link/0,
         init/1
        ]).

-include("cache.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ?ETS_LOADED_MODULES = ets:new(?ETS_LOADED_MODULES, [set, public, named_table]),
    SupFlag = #{
      strategy => one_for_one,
      intensity => 4,
      period => 3600
     },
    CacheSup = #{
      id => cache_sup,
      start => {cache_sup, start_link, []},
      restart => permanent,
      shutdown => 2000,
      type => supervisor,
      modules => [cache_sup]
     },
    CacheEvent = #{
      id => cache_event,
      start => {cache_event, start_link, []},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [cache_event]
     },
    AutoLoad = #{
      id => auto_load,
      start => {auto_load, start_link, []},
      restart => temporary,
      shutdown => brutal_kill,
      type => worker,
      modules => [auto_load]
     },

    ChildSpec = [CacheSup, CacheEvent, AutoLoad],
    {ok, {SupFlag, ChildSpec}}.
