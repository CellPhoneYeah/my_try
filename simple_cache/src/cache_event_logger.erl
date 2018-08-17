-module(cache_event_logger).

-behaviour(gen_event).

-export([
         add_handler/0,
         delete_handler/0
        ]).

-export([
         init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         code_change/3,
         terminate/2
        ]).

init([]) ->
    {ok, []}.

add_handler() ->
    cache_event:add_handler(?MODULE, []).

delete_handler() ->
    cache_event:delete_handler(?MODULE, []).

handle_event({create, Id, Data}, State) ->
    error_logger:info_msg("create(~w, ~w)~n", [Id, Data]),
    {ok, State};
handle_event({replace, Id, Data}, State) ->
    error_logger:info_msg("replace(~w, ~w)~n", [Id, Data]),
    {ok, State};
handle_event({delete, Id}, State) ->
    error_logger:info_msg("delete(~w)~n", [Id]),
    {ok, State};
handle_event({lookup, Id}, State) ->
    error_logger:info_msg("lookup(~w)~n", [Id]),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_call(_Request, State) ->
    {reply, ok, State}.

handle_info(_Request, State) ->
    {noreply, State}.
