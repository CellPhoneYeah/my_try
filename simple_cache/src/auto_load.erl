-module(auto_load).

-behaviour(gen_server).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2
        ]).

-export([
         start_link/0
        ]).

-include("cache.hrl").
-include_lib("kernel/include/file.hrl").

-define(ONE_SECOND, 10000).

%%% =====
%%% API
%%% =====
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%% =====
%%% call back
%%% =====
init([]) ->
    [begin 
         {ok, #file_info{mtime = MTime}} = file:read_file_info(ModName),
         ets:insert(?ETS_LOADED_MODULES, {Module, MTime})
     end ||
    {Module, ModName} <- code:all_loaded(), is_list(ModName)],
    {ok, {}, ?ONE_SECOND}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    AllModules = code:all_loaded(),
    reload_modules(AllModules),
    {noreply, State, ?ONE_SECOND};
handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% =====
%%% Internal
%%% =====
reload_modules([]) ->
    ok;
reload_modules([{Module, ModName}  | Tail]) ->
    do_reload_module(Module, ModName),
    reload_modules(Tail).

do_reload_module(Module, ModName) ->
    case file:read_file_info(ModName) of
        {_, #file_info{mtime = _Mtime}} ->
            case ets:lookup(?ETS_LOADED_MODULES, Module) of
                [{Module, _Mtime}] ->
                    ok;
                [{Module, Mtime1}] when Mtime1 > _Mtime ->
                    reload(Module);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

reload(Module) ->
    c:l(Module),
    ets:insert(?ETS_LOADED_MODULES, {Module, erlang:localtime()}),
    io:format("reload module ~p success!~n", [Module]).
