-module(gen_web_server).

-export([
         start_link/3,
         start_link/4,
         http_reply/1,
         http_reply/2,
         http_reply/3
        ]).

%%% =====
%%% Call back
%%% =====
-callback init() -> ok.

-callback web_head() -> ok.

-callback web_get() -> ok.

-callback web_delete() -> ok.

-callback web_options() -> ok.

-callback web_post() -> ok.

-callback web_put() -> ok.

-callback web_trace() -> ok.

-callback other_methods() -> ok.
%%% =====
%%% API
%%% =====
start_link(Callback, Port, UserArgs) ->
    start_link(Callback, undefined, Port, UserArgs).

start_link(Callback, IP, Port, UserArgs) ->
    gws_connect_sup:start_link(Callback, IP, Port, UserArgs).

http_reply(Code) ->
    http_reply(Code, <<>>).

http_reply(Code, Body) ->
    http_reply(Code, [{"Content-Type", "text/html"}], Body).

http_reply(Code, Header, Body) ->
    ContentBytes = iolist_to_binary(Body),
    Length = byte_size(ContentBytes),
    [io_lib:format("HTTP/1.1 ~s\s\n~sContent-Length: ~w\r\n\r\n",
                   [response(Code), headers(Header), Length]),
     ContentBytes].

%%% =====
%%% internal
%%% =====
headers([]) ->
    [];
headers([{Header, Text} | Hs]) ->
    [io_lib:format("~s: ~s\r\n", [Header, Text]) | headers(Hs)].

response(100) ->
    "100 continue";
response(200) ->
    "200 OK";
response(404) ->
    "404 Not Found";
response(501) ->
    "501 Not Implemented";
response(Code) ->
    integer_to_list(Code).
