-module(cache_event).

-export([
         add_handler/2,
         delete_handler/2,
         start_link/0,
         lookup/1,
         create/2,
         replace/2,
         delete/1
        ]).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?MODULE, Handler, Args).

lookup(Key) ->
    gen_event:notify(?MODULE, {lookup, Key}).

create(Key, Data) ->
    gen_event:notify(?MODULE, {create, Key, Data}).

replace(Key, Data) ->
    gen_event:notify(?MODULE, {replace, Key, Data}).

delete(Key) ->
    gen_event:notify(?MODULE, {delete, Key}).
