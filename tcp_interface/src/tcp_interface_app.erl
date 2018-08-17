-module(tcp_interface_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_PORT, 1088).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ListenPort = case application:get_env(tcp_interface, listen_port) of
                     {ok, Port} ->
                         Port;
                     _ ->
                         ?DEFAULT_PORT
                 end,
    io:format("listen port ~p~n", [ListenPort]),
    {ok, LSocket} = gen_tcp:listen(ListenPort, [binary, {packet, 0}, {active, true}]),
    case tcp_interface_sup:start_link(LSocket) of
        {ok, Pid} ->
            {ok, _} = tcp_interface_sup:start_child(),
            {ok, Pid};
        Error ->
            {error, Error}
    end.

stop(_State) ->
    ok.
