-module(gws_server).

-behaviour(gen_server).

-export([
         start_link/3
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {lsock = undefined,      % 监听的socket
                socket = undefined,     % 客户端socket
                request_line = undefined,   % 请求内容
                headers = [],           % 请求头部
                body = <<>>,            % 主体
                content_remaining = 0,  % 内容
                callback = undefined,   % 回调模块
                user_data = undefined,  % 用户数据
                parent = undefined}).   % 父进程

%%% =====
%%% API
%%% =====
start_link(Callback, LSock, UserArgs) ->
    gen_server:start(?MODULE, [Callback, LSock, UserArgs, self()], []).

%%% =====
%%% callback
%%% =====
init([Callback, LSock, UserArgs, Parent]) ->
    {ok, UserData} = Callback:init(UserArgs),
    State = #state{
               callback = Callback,
               lsock = LSock,
               user_data = UserData,
               parent = Parent},
    {ok, State, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

%% http请求报文将分段按如下结构发送：
%% 1. 请求行
%% 2. 协议头(数量任意)，这里会说明正文内容长度
%% 3. 空行
%% 4. 消息正文

%% 初始化结束，立刻等待客户端连接，
%% 当有新连接进入，马上开启另一个子进程等待，
%% 并设置socket为{active, once}
handle_info(timeout, #state{lsock = LSock, parent = Parent} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    gws_connection_sup:start_child(Parent),
    inet:setopts(Socket, {active, once}),
    {noreply, State#state{socket = Socket}};
%% 请求行解析
handle_info({http, _Sock, {http_request, _RequestType, _Content, _HTTPVersion} = Request}, State) ->
    inet:setopts(State#state.socket, [{active, once}]),
    {noreply, State#state{request_line = Request}};
%% 协议头解析
handle_info({http, _Sock, {http_hander, _Length, Name, _ReservedField, Value}}, State) ->
    inet:setopts(State#state.socket, [{active, once}]),
    {noreply, header(Name, Value, State)};
%% 报文头部结束，出现空行，并且消息正文为空，则web进程直接处理请求然后停止进程
handle_info({http, _Sock, http_eoh}, #state{content_remaining = 0} = State) ->
    {stop, normal, handle_http_request(State)};
%% 协议头结束，出现空行，设置socket为{packet, raw}，准备接受消息正文
handle_info({http, _Sock, http_eoh}, State) ->
    inet:setopts(State#state.socket, [{active, once}, {packet, raw}]),
    {noreply, State};
%% 报文的消息正文
handle_info({tcp, _Sock, Data}, State) when is_binary(Data) ->
    %% 检查是否有没发送完的正文
    ContentRem = State#state.content_remaining - byte_size(Data),
    Body = list_to_binary([State#state.body, Data]),
    NewState = State#state{body = Body,
                           content_remaining = ContentRem},
    % 剩余正文为0则执行请求内容，否则继续接收内容
    if ContentRem > 0 ->
           inet:setopts(State#state.socket, [{active, once}]),
           {noreply, NewState};
       true ->
           {stop, normal, handle_http_request(NewState)}
    end;
%% socket连接中断，则进程也中断
handle_info({tcp_closed, _Sock}, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% =====
%%% internal
%%% =====
header('Content-Length' = Name, Value, State) ->
    ContentLength = list_to_integer(binary_to_list(Value)),
    State#state{content_remaining = ContentLength,
     headers = [{Name, Value} | State#state.headers]};
header(<<"Expect">> = Name, <<"100-continue">> = Value, State) ->
    gen_tcp:send(State#state.socket, gen_web_server:http_reply(100)),
    State#state{headers = [{Name, Value} | State#state.headers]};
header(Name, Value, State) ->
    State#state{headers = [{Name, Value} | State#state.headers]}.

handle_http_request(#state{
                       callback = Callback,
                       request_line = RequestLine,
                       headers = Headers,
                       body = Body,
                       user_data = UserData} = State) ->
    {http_request, Method, _, _} = RequestLine,
    Reply = dispatch(Method, RequestLine, Headers, Body, Callback, UserData),
    gen_tcp:send(State#state.socket, Reply),
    State.

dispatch('GET', Request, Headers, _Body, Callback, UserData) ->
    Callback:web_get(Request, Headers, UserData);
dispatch('DELETE', Request, Headers, _Body, Callback, UserData) ->
    Callback:web_delete(Request, Headers, UserData);
dispatch('HEAD', Request, Headers, _Body, Callback, UserData) ->
    Callback:web_head(Request, Headers, UserData);
dispatch('POST', Request, Headers, Body, Callback, UserData) ->
    Callback:web_post(Request, Headers, Body, UserData);
dispatch('PUT', Request, Headers, Body, Callback, UserData) ->
    Callback:web_put(Request, Headers, Body, UserData);
dispatch('TRACE', Request, Headers, Body, Callback, UserData) ->
    Callback:web_trace(Request, Headers, Body, UserData);
dispatch('OPTIONS', Request, Headers, Body, Callback, UserData) ->
    Callback:web_options(Request, Headers, Body, UserData);
dispatch(_Other, Request, Headers, Body, Callback, UserData) ->
    Callback:other_method(Request, Headers, Body, UserData).
