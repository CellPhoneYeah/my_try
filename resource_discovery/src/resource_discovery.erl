%%% =====
%%% @author yxf
%%% @doc
%%% 资源探测模块
%%% @end
%%% =====
-module(resource_discovery).

-behaviour(gen_server).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-export([
         start_link/0
        ]).

-export([
         find_resource/1, % 按类型获取资源
         add_local_resource/2, % 增加本地资源
         add_target_type/1, % 增加目标资源
         trade_resource/0 % 交易资源
        ]).

-define(UNDEF, undefined).

-record(state, {
          target_type = [], % 目标资源
          local_resource = ?UNDEF, % 本地资源
          found_resource = ?UNDEF % 找到的资源
         }).

%%% =====
%%% API
%%% =====
find_resource(Type) ->
    gen_server:call(?MODULE, {find_resource, Type}).

add_local_resource(Type, Resource) ->
    gen_server:cast(?MODULE, {add_local_resource, Type, Resource}).

add_target_type(Type) ->
    gen_server:cast(?MODULE, {add_target_type, Type}).

trade_resource() ->
    gen_server:cast(?MODULE, trade_resource).

%%% =====
%%% call back
%%% =====
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    State = #state{
               target_type = [],
               local_resource = dict:new(),
               found_resource = dict:new()},
    {ok, State}.

handle_call({find_resource, Type}, _From, State) ->
    #state{found_resource = FoundRes} = State,
    case dict:find(Type, FoundRes) of
        {ok, List} ->
            ok;
        error ->
            List = []
    end,
    {reply, {ok, List}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({add_local_resource, Type, Resource}, State) ->
    #state{local_resource = LocalRes} = State,
    case dict:find(Type, LocalRes) of
        {ok, List} ->
            NewLocalRes = dict:store(Type, [Resource | List], LocalRes);
        error ->
            NewLocalRes = dict:store(Type, [Resource], LocalRes)
    end,
    {noreply, State#state{local_resource = NewLocalRes}};
handle_cast({add_target_type, Type}, State) ->
    #state{target_type = TargetType} = State,
    {noreply, State#state{target_type = [Type | lists:delete(Type, TargetType)]}};
%% 向每个相连节点发送本地资源
handle_cast(trade_resource, State) ->
    #state{local_resource = LocalResource} = State,
    AllNodes = [node() | nodes()],
    io:format("AllNodes ~p~n", [AllNodes]),
    [{?MODULE, Node} ! {trade_resource, self(), LocalResource}
     || Node <- AllNodes],
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({trade_resource, noreply, Resource}, State) ->
    #state{target_type = TargetType,
           found_resource = FoundRes} = State,
    NewFoundRes = add_resource_by_type(TargetType, Resource, FoundRes),
    {noreply, State#state{found_resource = NewFoundRes}};
handle_info({trade_resource, FromPid, Resource}, State) ->
    #state{local_resource = LocalRes,
           target_type = TargetType,
           found_resource = FoundRes} = State,
    NewFoundRes = add_resource_by_type(TargetType, Resource, FoundRes),
    FromPid ! {trade_resource, noreply, LocalRes},
    {noreply, State#state{found_resource = NewFoundRes}};
handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% =====
%%% internal
%%% =====

%% Resource 远程资源
%% FoundRes 本地已找到资源
%% Type 所需的资源类型
add_resource_by_type([Type | LeftType], Resource, FoundRes) ->
    case dict:find(Type, Resource) of
        {ok, Target} ->
            case dict:find(Type, FoundRes) of
                {ok, OldTypeRes} ->
                    FilterTarget = lists:filter(fun(R) -> lists:member(R, OldTypeRes) =:= false end, Target),
                    NewFoundRes = dict:store(Type, FilterTarget ++ OldTypeRes, FoundRes);
                error ->
                    NewFoundRes = dict:store(Type, Target, FoundRes)
            end,
            NewResource = dict:erase(Type, Resource);
        error ->
            NewFoundRes = FoundRes,
            NewResource = Resource
    end,
    add_resource_by_type(LeftType, NewResource, NewFoundRes);
add_resource_by_type([], _, NewFoundRes) ->
    NewFoundRes.
