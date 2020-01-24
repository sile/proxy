%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc proxyサーバを扱うためのインターフェースモジュール
-module(proxy).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         %% TODO: call, notify, add_proxy, swap_proxy, delete_proxy, which_proxies
         spawn/2, spawn/4,
         spawn_opt/3, spawn_opt/5,
         start/4, start/5,
         start_link/4, start_link/5,
         get_real_pid/1,
         get_state/1
        ]).

-export([
         call/2
        ]).

-export_type([
              proxy_spec/0,
              proxy_arg/0,
              proxy_state/0,

              function/0,
              args/0,
              spawn_options/0, spawn_option/0,

              proxy_name/0,
              proxy_ref/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback init(proxy_arg()) ->
    {stop, term()} | ignore | {ok, proxy_state()}.

%% proxy_server で実行する関数の引数を処理する.
-callback handle_arg([term()], proxy_state()) ->
    {stop, term(), proxy_state()} | {ok, [term()], proxy_state()} | {remove_proxy, [term()], proxy_state()} |
    {swap_proxy, [term()], term(), proxy_state(), module(), proxy_arg()} |
    {hibernate, [term()], proxy_state()}.

%% proxy_server が起動した時にする処理.
-callback handle_up(pid(), proxy_state()) ->
    {stop, term(), proxy_state()} | {ok, proxy_state()} | {remove_proxy, proxy_state()} |
    {swap_proxy, term(), proxy_state(), module(), proxy_arg()}.

%% Message が飛んできたときの処理.
-callback handle_message(term(), proxy_state()) ->
    {stop, term(), proxy_state()} | {ok, term(), proxy_state()} |
    {remove_proxy, term(), proxy_state()} | {remove_proxy, proxy_state()} |
    {ignore, proxy_state()} |
    {swap_proxy, term(), term(), proxy_state(), module(), proxy_arg()} |
    {swap_proxy, term(), proxy_state(), module(), proxy_arg()}.

%% process 終了時の処理. 前にある proxy がrestartを返すと, restart が掛かる.
-callback handle_down(term(), proxy_state()) ->
    {stop, term(), proxy_state()} | {ok, proxy_state()} |
    {restart, proxy_state()} | {restart, non_neg_integer(), proxy_state()}.

-callback terminate(term(), proxy_state()) -> any().

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type proxy_spec() :: {module(), proxy_arg()}.
-type proxy_state() :: term().
-type proxy_arg() :: term().

-type spawn_options() :: [spawn_option()].

-type spawn_option() :: link
                      | monitor
                      | {priority, priority_level()}
                      | {fullsweep_after, non_neg_integer()}
                      | {min_heap_size, non_neg_integer()}
                      | {min_bin_vheap_size, non_neg_integer()}.
%% spawn_optのoption

-type priority_level() :: low | normal | high | max.

-type fun_name() :: atom().
-type args() :: [term()].

-type proxy_name() :: {local, Name :: atom()}
                    | {global, Name :: term()}
                    | {via, module(), Name :: term()}.

-type proxy_ref() :: (Name :: atom())
                   | {Name :: atom(), node()}
                   | {global, Name :: term()}
                   | {via, module(), Name :: term()}
                   | pid().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc proxy_server モジュールにメッセージを送る.
%%
%% 'get_real_process' メッセージを送ると, proxy_server の Process ID を取得できる.
%% (それ以外のメッセージには、現在非対応)
-spec call(pid(), term()) -> ProxyServerPid::pid() | error.
call(ProxyPid, Msg) ->
    Tag = make_ref(),
    From = {self(), Tag},
    _ = ProxyPid ! {'$proxy_call', From, Msg},
    receive
        {Tag, Response} -> Response
    after 50 ->
            error %exit({timeout, call, [ProxyPid, Msg]})
    end.

%% @doc proxy を挟んで process を生成する.
-spec spawn(fun(), [proxy_spec()]) -> pid() | {pid(), reference()}.
spawn(Fun, ProxySpecs) ->
    ?MODULE:spawn_opt(Fun, ProxySpecs, []).

%% @doc proxy を挟んで process を生成する.
-spec spawn(module(), fun_name(), args(), [proxy_spec()]) -> pid() | {pid(), reference()}.
spawn(Module, Function, Args, ProxySpecs) ->
    ?MODULE:spawn_opt(Module, Function, Args, ProxySpecs, []).

%% @doc proxy を挟み option を指定して process を生成する.
-spec spawn_opt(fun(), [proxy_spec()], spawn_options()) -> pid() | {pid(), reference()}.
spawn_opt(Fun, ProxySpecs, SpawnOpts) ->
    StartFunc = proxy_start_func:make_spawn_func(Fun, SpawnOpts -- [monitor]),
    erlang:spawn_opt(proxy_server, start_loop, [StartFunc, ProxySpecs], SpawnOpts). % TODO: 一部のオプション以外は切り取る

%% @doc proxy を挟み option を指定して process を生成する.
-spec spawn_opt(module(), fun_name(), args(), [proxy_spec()], spawn_options()) -> pid() | {pid(), reference()}.
spawn_opt(Module, Function, Args, ProxySpecs, SpawnOpts) ->
    StartFunc = proxy_start_func:make_spawn_func(Module, Function, Args, SpawnOpts -- [monitor]),
    erlang:spawn_opt(proxy_server, start_loop, [StartFunc, ProxySpecs], SpawnOpts).

%% @doc proxy を挟んで 名前付きserver を起動する.
-spec start(proxy_name(), module(), fun_name(), args(), [proxy_spec()]) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start(Name, Module, Function, Args, ProxySpecs) ->
    start_impl(Name, false, Module, Function, Args, ProxySpecs).

%% @doc proxy を挟んで 名前付きserver を起動する.
-spec start_link(proxy_name(), module(), fun_name(), args(), [proxy_spec()]) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_link(Name, Module, Function, Args, ProxySpecs) ->
    start_impl(Name, true, Module, Function, Args, ProxySpecs).

%% @doc proxy を挟んで server を起動する.
-spec start(module(), fun_name(), args(), [proxy_spec()]) -> {ok, pid()} | {error, Reason::term()}.
start(Module, Function, Args, ProxySpecs) ->
    start_impl(undefined, false, Module, Function, Args, ProxySpecs).

%% @doc proxy を挟んで server を起動する.
-spec start_link(module(), fun_name(), args(), [proxy_spec()]) -> {ok, pid()} | {error, Reason::term()}.
start_link(Module, Function, Args, ProxySpecs) ->
    start_impl(undefined, true, Module, Function, Args, ProxySpecs).

%% @doc 実プロセスのPIDを取得する
%%
%% NOTE: 基本的に実プロセスを直接操作することは望ましくないので、この関数はデバッグやテスト目的のために提供されている
-spec get_real_pid(pid()) -> pid() | hibernate.
get_real_pid(ProxyPid) ->
    case call(ProxyPid, get_real_process) of
        error   -> error(timeout, [ProxyPid]);
        RealPid -> RealPid
    end.

%% @doc プロキシプロセスの状態を取得する
%%
%% デバッグ用.
-spec get_state(pid()) -> proxy_server:state().
get_state(ProxyPid) ->
    case call(ProxyPid, get_proxy_server_state) of
        error   -> error(timeout, [ProxyPid]);
        State   -> State
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec start_impl(proxy_name() | undefined, boolean(), module(), fun_name(), args(), [proxy_spec()]) -> {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_impl(Name, DoLink, Module, Function, Args, ProxySpecs) ->
    case Name =:= undefined orelse proxy_lib:where(Name) of
        Pid when is_pid(Pid) -> {error, {already_started, Pid}};
        _ ->
            Ref  = make_ref(),
            From = {self(), Ref},
            StartFunc = proxy_start_func:make_start_func(Module, Function, Args),
            Spawn = case DoLink of true -> spawn_link; false -> spawn end,
            Pid = erlang:Spawn(proxy_server, start_loop, [Name, From, StartFunc, ProxySpecs]),
            Monitor = erlang:monitor(process, Pid),
            receive
                {'DOWN', _, _, Pid, Reason} -> {error, Reason};
                {Ref, Result}               ->
                    _ = erlang:demonitor(Monitor, [flush]),
                    Result
            end
    end.
