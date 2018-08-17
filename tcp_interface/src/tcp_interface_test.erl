-module(tcp_interface_test).

-export([
         test1/0,
         test2/0
        ]).

test1() ->
    case gen_tcp:connect(localhost, 1088, [binary, {packet, 0}]) of
        {ok, Socket} ->
            gen_tcp:send(Socket, "insert_cache[1, hello]"),
            receive
                {tcp, _, Data} ->
                    io:format("return ~p~n", [Data])
            end;
        Error ->
            {error, Error}
    end.

test2() ->
    case gen_tcp:connect(localhost, 1088, [binary, {packet, 0}]) of
        {ok, Socket} ->
            gen_tcp:send(Socket, "get_cache[1]"),
            receive
                {tcp, _, Data} ->
                    io:format("return ~p~n", [Data])
            end;
        Error ->
            {error, Error}
    end.
