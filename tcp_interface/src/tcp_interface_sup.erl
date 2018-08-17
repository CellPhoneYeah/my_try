-module(tcp_interface_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/1,
         start_child/0
        ]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args), {I, {I, start_link, Args}, permanent, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(LSocket) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [LSocket]).

start_child() ->
    supervisor:start_child(?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([LSocket]) ->
    ListenSup = ?CHILD(acceptor, [LSocket]),
    ChildSpec = [ListenSup],
    {ok, { {simple_one_for_one, 5, 10}, ChildSpec} }.
