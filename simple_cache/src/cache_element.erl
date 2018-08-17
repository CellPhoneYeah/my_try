-module(cache_element).

-behaviour(gen_server).

-export([
         init/1,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         terminate/2,
         code_change/3
        ]).

-export([
         start_link/1
        ]).

-export([
         get_data/1,
         delete_data/1,
         add_data/1,
         replace_data/2
        ]).

-record(state,
        {
         data = undefined % 数据
        }).

-define(CACHE_LEASE, (3000 * 1000)).


%%% ======
%%% API
%%% ======
start_link(Data) ->
    gen_server:start_link(?MODULE, [Data], []).

get_data(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            gen_server:call(Pid, get_data);
        false ->
            error_cache_has_been_gc
    end.

delete_data(Pid) ->
    gen_server:cast(Pid, delete_data).

add_data(Data) ->
    cache_sup:start_cache(Data).

replace_data(Pid, Data) when is_pid(Pid) ->
    gen_server:cast(Pid, {replace_data, Data}).

%%% ======
%%% call back
%%% ======
init([Data]) ->
    {ok, #state{data = Data}, ?CACHE_LEASE}.

handle_cast(delete, State) ->
    {stop, normal, State};
handle_cast({replace_data, Data}, State) ->
    {noreply, State#state{data = Data}, ?CACHE_LEASE};
handle_cast(Request, State) ->
    io:format("handle_cast nothing to do for request ~w~n", [Request]),
    {noreply, State, ?CACHE_LEASE}.

handle_info(timeout, State) ->
    io:format("auto gc ~w~n", [State]),
    {stop, normal, State};
handle_info(Request, State) ->
    io:format("handle_info nothing to do for request ~w~n", [Request]),
    {noreply, State, ?CACHE_LEASE}.

handle_call(get_data, _From, State) ->
    #state{data = Data} = State,
    {reply, {ok, Data}, State, ?CACHE_LEASE};
handle_call(Request, From, State) ->
    io:format("handle_call nothing to do for request ~w from ~w~n", [Request, From]),
    {reply, ok, State, ?CACHE_LEASE}.

terminate(_Reason, _State) ->
    ok.

code_change(_Oldvsn, State, _Extra) ->
    {ok, State}.

%%% =====
%%% internal
%%% =====
