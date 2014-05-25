-module(proxy_start_func).

-export([
         make_spawn_func/1,
         make_spawn_func/2,
         make_spawn_func/3,
         make_spawn_func/4,
         make_start_func/3,
         make_start_func/4,
         make_start_func/5,

         get_args/1,
         set_args/2,
         start_link/1
        ]).

make_spawn_func(Module, Function, Args) ->
    make_spawn_func(Module, Function, Args, []).

make_spawn_func(Module, Function, Args, Options) ->
    {spawn, Module, Function, Args, Options}.

make_spawn_func(Fun) ->
    make_spawn_func(Fun, []).

make_spawn_func(Fun, Options) ->
    {spawn, Fun, Options}.

make_start_func(Module, Function, Args) ->
    make_start_func(Module, Function, Args, 5000).

make_start_func(Module, Function, Args, Time) ->
    make_start_func(Module, Function, Args, Time, []).

make_start_func(Module, Function, Args, Time, Options) ->
    {start, Module, Function, Args, Time, Options}.

get_args({spawn, _, _, Args, _})    -> {ok, Args};
get_args({start, _, _, Args, _, _}) -> {ok, Args};
get_args(_)                         -> error.

set_args(Args, Func = {spawn, _, _, _, _})    -> setelement(4, Func, Args);
set_args(Args, Func = {start, _, _, _, _, _}) -> setelement(4, Func, Args);
set_args(_Args, Func)                         -> Func.

start_link({spawn, Module, Function, Args, Options}) ->
    {ok, spawn_opt(Module, Function, Args, [link | Options])};
start_link({spawn, Fun, Options}) ->
    {ok, spawn_opt(Fun, [link | Options])};
start_link({start, Module, Function, Args, _Time, _Options}) ->
    case apply(Module, Function, Args) of
        {ok, Pid} ->
            true = link(Pid),
            {ok, Pid};
        Other -> Other
    end.
