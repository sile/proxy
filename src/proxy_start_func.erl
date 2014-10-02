%% @doc 実行する関数を扱うモジュール
-module(proxy_start_func).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
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

-export_type([
              real_func/0,
              spawn_func/0, start_func/0,

              function/0, args/0, spawn_options/0,

              %% 以下はspawn_optのoption
              spawn_option/0, priority_level/0
             ]).

-type real_func() :: spawn_func() | start_func().

-type spawn_func() :: {spawn, module(), function(), args(), spawn_options()}
                    | {spawn, fun(), spawn_options()}.
-type start_func() :: {start, module(), function(), args(), time(), start_options()}.

-type function() :: atom().
-type args() :: [term()].
-type spawn_options() :: [spawn_option()].

-type spawn_option() :: link
                      | monitor
                      | {priority, priority_level()}
                      | {fullsweep_after, non_neg_integer()}
                      | {min_heap_size, non_neg_integer()}
                      | {min_bin_vheap_size, non_neg_integer()}.
%% @doc spawn_optのoption

-type priority_level() :: low | normal | high | max.

-type time() :: integer() | term(). % 現在不使用.
-type start_options() :: [term()] | term(). % 現在不使用.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc 別プロセスで実行する関数セットを作る.
-spec make_spawn_func(module(), function(), args()) -> spawn_func().
make_spawn_func(Module, Function, Args) ->
    make_spawn_func(Module, Function, Args, []).

-spec make_spawn_func(module(), function(), args(), spawn_options()) -> spawn_func().
make_spawn_func(Module, Function, Args, Options) ->
    {spawn, Module, Function, Args, Options}.

-spec make_spawn_func(fun()) -> spawn_func().
make_spawn_func(Fun) ->
    make_spawn_func(Fun, []).

-spec make_spawn_func(fun(), spawn_options()) -> spawn_func().
make_spawn_func(Fun, Options) ->
    {spawn, Fun, Options}.

%% @doc サーバを実行するための関数セットを作る.
-spec make_start_func(module(), function(), args()) -> start_func().
make_start_func(Module, Function, Args) ->
    make_start_func(Module, Function, Args, 5000).

-spec make_start_func(module(), function(), args(), time()) -> start_func().
make_start_func(Module, Function, Args, Time) ->
    make_start_func(Module, Function, Args, Time, []).

-spec make_start_func(module(), function(), args(), time(), start_options()) -> start_func().
make_start_func(Module, Function, Args, Time, Options) ->
    {start, Module, Function, Args, Time, Options}.

%% @doc 引数を取得する.
-spec get_args(real_func()) -> {ok, args()} | error.
get_args({spawn, _, _, Args, _})    -> {ok, Args};
get_args({start, _, _, Args, _, _}) -> {ok, Args};
get_args(_)                         -> error.

%% @doc 引数をセットする.
-spec set_args(args(), real_func()) -> real_func().
set_args(Args, Func = {spawn, _, _, _, _})    -> setelement(4, Func, Args);
set_args(Args, Func = {start, _, _, _, _, _}) -> setelement(4, Func, Args);
set_args(_Args, Func)                         -> Func.

%% @doc 関数を開始する.
-spec start_link(real_func()) -> {ok, pid() | {pid(), reference()}} | term().
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
