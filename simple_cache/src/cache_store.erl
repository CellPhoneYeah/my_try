-module(cache_store).

-export([
         init/0,
         get_data/1,
         replace_data/2,
         delete_data/1,
         add_data/2
        ]).

%%-define(ETS_DICT, ets_dcit).
%%-record(ets_dict, {
%%          id = 0,
%%          pid = undefined
%%         }).
-define(KEY_TO_PID, key_to_pid).
-record(key_to_pid, {
          key = 0,
          pid = undefined
         }).

-define(WAIT_FOR_TABLES, 5000).

init() ->
    %%ets:new(?ETS_DICT, [public, named_table, set, {keypos, #ets_dict.id}]).
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    {ok, CacheNodes} = resource_discovery:find_resource(simple_cache),
    mnesia:start(),
    dynamic_db_init(lists:delete(node(), CacheNodes)).

dynamic_db_init([]) ->
    {atomic, ok} = mnesia:create_table(?KEY_TO_PID,
                        [{index, [pid]},
                         {attributes, record_info(fields, key_to_pid)}]),
    io:format("create table success~n");
dynamic_db_init(CacheNodes) ->
    io:format("CacheNodes~p~n", [CacheNodes]),
    add_extra_nodes(CacheNodes).

add_extra_nodes([Node | Tail]) ->
    case mnesia:change_config(extra_db_nodes, [Node]) of
        {ok, [Node]} ->
            %% {atomic, ok} = mnesia:add_table_copy(schema, node(), ram_copies),
            
            {atomic, ok} = mnesia:add_table_copy(key_to_pid, node(), ram_copies),

            Tables = mnesia:system_info(tables),
            mnesia:wait_for_tables(Tables, ?WAIT_FOR_TABLES);
        _ ->
            add_extra_nodes(Tail)
    end.


get_data(Id) ->
    %%case ets:lookup(?ETS_DICT, Id) of
    %%    [#ets_dict{pid = Pid}] ->
    %%        {ok, Pid};
    %%    _ ->
    %%        {error, no_found}
    %%end.
    case mnesia:dirty_read(?KEY_TO_PID, Id) of
        [#key_to_pid{pid = Pid}] ->
            case is_alive(Pid) of
                true ->
                    {ok, Pid};
                false ->
                    {error, no_found}
            end;
        [] ->
            {error, no_found}
    end.

replace_data(Id, Pid) ->
    %% ets:insert(?ETS_DICT, #ets_dict{id = Id, pid = Pid}).
    mnesia:dirty_write(?KEY_TO_PID, #key_to_pid{key = Id, pid = Pid}).

delete_data(Id) ->
    %% ets:match_delete(?ETS_DICT, #ets_dict{id = Id, pid = '_'}).
    case mnesia:dirty_index_read(?KEY_TO_PID, Id, #key_to_pid.key) of
        [#key_to_pid{} = Record] ->
            mnesia:dirty_delete_object(Record);
        [] ->
            ok
    end.

add_data(Id, Pid) ->
    %% ets:insert(?ETS_DICT, #ets_dict{id = Id, pid = Pid}).
    mnesia:dirty_write(?KEY_TO_PID, #key_to_pid{key = Id, pid = Pid}).

is_alive(Pid) when node(Pid) == node() ->
    erlang:is_process_alive(Pid);
is_alive(Pid) ->
    lists:member(node(Pid), nodes())
    andalso
    rpc:call(node(Pid), erlang, is_process_alive, [Pid]).
