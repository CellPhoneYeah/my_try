-module(cache_app).

-behaviour(application).

-export([
         start/2,
         stop/1
        ]).

-define(WAIT_FOR_RESOURCE, 2500).

start(_, _) ->
    ok = ensure_contact(),
    resource_discovery:add_local_resource(simple_cache, node()),
    resource_discovery:add_target_type(simple_cache),
    resource_discovery:trade_resource(),
    timer:sleep(?WAIT_FOR_RESOURCE),
    cache_store:init(),
    {ok, SupPid} = root_sup:start_link(),
    cache_event_logger:add_handler(),
    {ok, SupPid}.

ensure_contact() ->
    DefaultNodes = ['contact1@localhost', 'contact2@localhost'],
    case get_env(cache, contact_nodes, DefaultNodes) of
        [] ->
            {error, no_contact_nodes};
        Nodes ->
            ensure_contact(Nodes)
    end.

ensure_contact(Nodes) ->
    Answering = [N || N <- Nodes, net_adm:ping(N) =:= pong],
    case Answering of
        [] ->
            {error, no_contact_nodes_reachable};
        _ ->
            DefaultTime = 6000,
            WaitTime = get_env(cache, wait_time, DefaultTime),
            wait_for_nodes(length(Answering), WaitTime)
    end.

wait_for_nodes(MinNodes, WaitTime) ->
    Slices = 10,
    SliceTime = round(WaitTime/Slices),
    wait_for_nodes(MinNodes, SliceTime, Slices).

wait_for_nodes(_MinNodes, _SliceTime, 0) ->
    ok;
wait_for_nodes(MinNodes, SliceTime, Iterations) ->
    case length(nodes()) > MinNodes of
        true ->
            ok;
        false ->
            timer:sleep(SliceTime),
            wait_for_nodes(MinNodes, SliceTime, Iterations - 1)
    end.

get_env(AppName, Key, Default) ->
    case application:get_env(AppName, Key) of
        undefined -> Default;
        {ok, Value} -> Value
    end.

stop(_) ->
    ok.
