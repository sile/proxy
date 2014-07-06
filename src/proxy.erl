%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc proxyサーバを扱うためのインターフェースモジュール
%%
%% TODO: OTP監視ツリーとの統合
-module(proxy).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         spawn/2, spawn/3, spawn/4, spawn/5,
         start/4, start/5,

         reply/2,
         try_call/3, try_call/4,
         call/3, call/4,
         cast/3,

         add/2,
         remove/2, remove/3,
         swap/3, swap/4,

         get_real_process/1,
         which_proxies/1
        ]).

-export_type([
              proxy_specs/0,
              proxy_spec/0,
              proxy_id/0,
              proxy_arg/0,
              proxy_state/0,

              spawn_options/0,
              start_options/0,
              spawn_option/0,
              start_option/0,
              spawn_proxy_option/0,
              spawn_real_option/0,
              priority_level/0,

              function_name/0,
              mfargs/0,

              context/0,

              stop_reason/0,

              message/0,
              request/0,
              from/0,

              proxy_process_ref/0,
              proxy_process_pid/0,
              real_process_pid/0,
              real_process_args/0,

              non_neg_milliseconds/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback init(proxy_arg(), context()) ->
    {ok, proxy_state(), context()} |
    {stop, stop_reason(), context()} |
    {ignore, context()}.

-callback handle_args(real_process_args(), proxy_state(), context()) ->
    {ok, real_process_args(), proxy_state(), context()} |
    {stop, stop_reason(), context()}.

-callback handle_up(real_process_pid(), proxy_state(), context()) ->
    {ok, proxy_state(), context()} |
    {stop, stop_reason(), proxy_state(), context()}.

-callback handle_message(message(), proxy_state(), context()) ->
    {ok, message(), proxy_state(), context()} |
    {stop, stop_reason(), proxy_state(), context()} |
    {ignore, proxy_state(), context()}.

-callback handle_request(request(), from(), proxy_state(), context()) ->
    {ok, proxy_state(), context()} |
    {stop, stop_reason(), proxy_state(), context()}.

-callback handle_down(stop_reason(), proxy_state(), context()) ->
    {ok, stop_reason(), proxy_state(), context()} |
    {restart, non_neg_milliseconds(), proxy_state(), context()}.

-callback terminate(stop_reason(), proxy_state(), context()) ->
    context().

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type proxy_specs() :: [proxy_spec()].
-type proxy_spec()  :: {proxy_id(), proxy_arg()}.
-type proxy_id()    :: module() | {module(), Name::term()}.
-type proxy_state() :: term().
-type proxy_arg()   :: term().

-type function_name() :: atom().
-type mfargs() :: {module(), function_name(), [term()]}.

-type context() :: proxy_context:context().

-type spawn_options() :: [spawn_option()].
-type start_options() :: [start_option()].

-type spawn_option() :: {logger, logi:context_ref()} % default: logi:default_logger()
                      | link
                      | {proxy_options, [spawn_proxy_option()]}
                      | {real_options,  [spawn_real_option()]}.

-type spawn_proxy_option() :: {node, node()}
                            | {process, priority_level()}
                            | {fullsweep_after, non_neg_milliseconds()}
                            | {min_heap_size, Size::non_neg_integer()}
                            | {min_bin_vheap_size, VSize::non_neg_integer()}.

-type start_option() :: {logger, logi:context_ref()} % default: logi:default_logger()
                      | link
                      | {timeout, timeout()}
                      | {process, priority_level()}
                      | {fullsweep_after, non_neg_milliseconds()}
                      | {min_heap_size, Size::non_neg_integer()}
                      | {min_bin_vheap_size, VSize::non_neg_integer()}.

-type priority_level() :: low | normal | high | max.

-type stop_reason() :: term().
-type swap_reason() :: term().

-type message() :: term().
-type request() :: term().
-type from()    :: {Tag::reference(), pid()} | undefined.

-type proxy_process_ref() :: atom() | {atom(), node()} | proxy_process_pid().
-type proxy_process_pid() :: pid().
-type real_process_pid()  :: pid().
-type real_process_args() :: [term()].

-type non_neg_milliseconds() :: non_neg_integer().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv spawn(Fun, ProxySpecs, [])
-spec spawn(fun (() -> any()), proxy_specs()) -> proxy_process_pid().
spawn(Fun, ProxySpecs) ->
    spawn(Fun, ProxySpecs, []).

-spec spawn(fun (() -> any()), proxy_spec(), spawn_options()) -> proxy_process_pid().
spawn(Fun, ProxySpecs, Options) ->
    error(not_implemented, [Fun, ProxySpecs, Options]).

%% @equiv spawn(Module, Function, Args, ProxySpecs, [])
-spec spawn(module(), function_name(), [term()], proxy_specs()) -> proxy_process_pid().
spawn(Module, Function, Args, ProxySpecs) ->
    spawn(Module, Function, Args, ProxySpecs).

-spec spawn(module(), function_name(), [term()], proxy_specs(), spawn_options()) -> proxy_process_pid().
spawn(Module, Function, Args, ProxySpecs, Options) ->
    error(not_implemented, [Module, Function, Args, ProxySpecs, Options]).

%% @equiv start(Module, Function, Args, ProxySpecs, [])
-spec start(module(), function_name(), [term()], proxy_specs()) -> term() | {error, term()}.
start(Module, Function, Args, ProxySpecs) ->
    start(Module, Function, Args, ProxySpecs, []).

-spec start(module(), function_name(), [term()], proxy_specs(), start_option()) -> term() | {error, term()}.
start(Module, Function, Args, ProxySpecs, Options) ->
    error(not_implemented, [Module, Function, Args, ProxySpecs, Options]).

%% @equiv try_call(ProxyProcess, ProxyId, Request, 5000)
-spec try_call(proxy_process_ref(), proxy_id(), request()) -> term() | {error, Reason} when
      Reason :: not_found.
try_call(ProxyProcess, ProxyId, Request) ->
    try_call(ProxyId, ProxyId, Request, 5000).

-spec try_call(proxy_process_ref(), proxy_id(), request(), timeout()) -> term() | {error, Reason} when
      Reason :: not_found.
try_call(ProxyProcess, ProxyId, Request, Timeout) ->
    error(not_implemented, [ProxyProcess, ProxyId, Request, Timeout]).

%% @equiv call(ProxyProcess, ProxyId, Request, 5000)
-spec call(proxy_process_ref(), proxy_id(), request()) -> term().
call(ProxyProcess, ProxyId, Request) ->
    call(ProxyId, ProxyId, Request, 5000).

-spec call(proxy_process_ref(), proxy_id(), request(), timeout()) -> term().
call(ProxyProcess, ProxyId, Request, Timeout) ->
    error(not_implemented, [ProxyProcess, ProxyId, Request, Timeout]).

-spec cast(proxy_process_ref(), proxy_id(), request()) -> ok.
cast(ProxyProcess, ProxyId, Request) ->
    error(not_implemented, [ProxyProcess, ProxyId, Request]).

-spec reply(from(), term()) -> ok | ignroe.
reply(undefined, _Reply)                                     -> ignore;
reply({Tag, Pid}, Reply) when is_reference(Tag), is_pid(Pid) -> _ = Pid ! {Tag, Reply}, ok;
reply(From, Reply)                                           -> error(badarg, [From, Reply]).

-spec add(proxy_process_ref(), proxy_spec()) -> ok | {error, Reason} when
      Reason :: already_present.
add(ProxyProcess, ProxySpec) ->
    error(not_implemented, [ProxyProcess, ProxySpec]).

-spec remove(term(), context()) -> context().
remove(Reason, Context) ->
    error(not_implemented, [Reason, Context]).

-spec remove(proxy_process_ref(), proxy_id(), term()) -> ok | {error, Reason} when
      Reason :: proxy_not_found.
remove(ProxyProcess, ProxyId, Reason) ->
    error(not_implemented, [ProxyProcess, ProxyId, Reason]).

-spec swap(term(), proxy_spec(), context()) -> {ok, context()} | {error, Reason} when
      Reason :: already_present.
swap(Reason, ProxySpec, Context) ->
    error(not_implemented, [Reason, ProxySpec, Context]).

-spec swap(proxy_process_ref(), proxy_id(), term(), proxy_spec()) -> ok | {error, Reason} when
      Reason :: not_found | already_present.
swap(ProxyProcess, ProxyId, Reason, ProxySpec) ->
    error(not_implemented, [ProxyProcess, ProxyId, Reason, ProxySpec]).

-spec get_real_process(proxy_process_ref()) -> real_process_pid() | {restarting, non_neg_milliseconds()}.
get_real_process(ProxyProcess) ->
    error(not_implemented, [ProxyProcess]).

-spec which_proxies(proxy_process_ref()) -> [proxy_id()].
which_proxies(ProxyProcess) ->
    error(not_implemented, [ProxyProcess]).
