-module(cache_manager).

-export([
         get_cache/1,
         insert_cache/2
        ]).

-export([
         test/0
        ]).

-include("cache.hrl").
%%% =====
%%% test
%%% =====
test() ->
    insert_cache(1, good),
    insert_cache(2, morning),
    insert_cache(3, hello),
    insert_cache(4, world),
    get_cache(1),
    get_cache(2),
    get_cache(3),
    get_cache(4),
    insert_cache(4, world),
    get_cache(4).

get_cache(Id) ->
    case cache_store:get_data(Id) of
        {error, not_found}->
            {error, cache_not_found};
        {ok, Pid} ->
            case cache_element:get_data(Pid) of
                {ok, Data} ->
                    cache_event:lookup(Id),
                    Data;
                Error ->
                    cache_store:delete_data(Id),
                    cache_event:delete(Id),
                    Error
            end
    end.

insert_cache(Id, Data) ->
    case cache_store:get_data(Id) of
        {ok, Pid} ->
            cache_element:replace_data(Pid, Data),
            cache_event:replace(Id, Data);
        _ ->
            {ok, Pid} = cache_element:add_data(Data),
            cache_store:add_data(Id, Pid),
            cache_event:create(Id, Data)
    end.
