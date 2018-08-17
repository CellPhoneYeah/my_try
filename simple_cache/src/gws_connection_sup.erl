-module(gws_connection_sup).

-behaviour(supervisor).

-export([
         start_link/4,
         start_child/1
        ]).

-export([
         init/1
        ]).

%%% =====
%%% API
%%% =====
start_link(Callback, IP, Port, UserArgs) ->
    {ok, Pid} = supervisor:start_link(?MODULE, [Callback, IP, Port, UserArgs]),
    start_child(Pid),
    {ok, Pid}.

start_child(Pid) ->
    supervisor:start_child(Pid, []).

%%% =====
%%% internal
%%% =====
init([Callback, IP, Port, UserArgs]) ->
    BasicSockOpts = [binary,
                     {active, false},
                     {packet, http_bin},
                     {reuseaddr, true}],
    SockOpts = case IP of
                   undefined ->
                       BasicSockOpts;
                   _ ->
                       [{ip, IP} | BasicSockOpts]
               end,
    {ok, LSock} = gen_tcp:listen(Port, SockOpts),
    Server = #{id => gws_server,
               start => {gws_server, start_link, [Callback, LSock, UserArgs]},
               restart => termporary,
               shutdown => brutal,
               type => worker,
               modules => [gws_server]},
    Strategy = #{strategy => simple_one_for_one,
                 intensity => 1000,
                 period => 3600},
    {ok, {Strategy, [Server]}}.
