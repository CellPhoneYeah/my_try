-module(acceptor).

-behaviour(gen_server).

-export([
         start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {
          listen_socket = undefined, % 监听socket
          client_socket = undefined % 客户端socket
         }).

start_link(LSocket) ->
    gen_server:start_link(?MODULE, [LSocket], []).

init([LSocket]) ->
    {ok, #state{listen_socket = LSocket}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
    do_handle_data(Socket, Data),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(timeout, State) ->
    #state{listen_socket = LSocket} = State,
    {ok, CSocket} = gen_tcp:accept(LSocket),
    tcp_interface_sup:start_child(),
    {noreply, #state{client_socket = CSocket}};
handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% =====
%%% internal
%%% =====
do_handle_data(Socket, Data) ->
    try
        String = binary_to_list(Data),
        {Function, ArgStr} = lists:splitwith(fun(E) -> E =/= $[ end, String),
        {ok, Toks, _Line} = erl_scan:string(ArgStr ++ ".", 1),
        {ok, Args} = erl_parse:parse_term(Toks),
        FunctionAtom = erlang:list_to_atom(Function),
        io:format("Function ~p Args ~p~n", [FunctionAtom, Args]),
        Result = apply(cache_manager, FunctionAtom, Args),
        gen_tcp:send(Socket, io_lib:fwrite("ok:~p.~n", [Result]))
    catch
        Err ->
            gen_tcp:send(Socket, io_lib:fwrite("Error:~p.~n", [Err]))
    end.
