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
         start/2, start/3,
         %% TODO: start_link/4, start_link/5,

         reply/2,
         try_call/3, try_call/4,
         call/3, call/4,
         cast/3,

         add/2,
         remove/2, remove/3,
         swap/3, swap/4,

         get_real_process/1,
         which_modules/1
        ]).

-export_type([
              start_opt/0,
              spawn_opt/0,
              debug_opt/0,

              priority_level/0,

              function_name/0,
              mfargs/0,

              context/0,

              message/0,
              request/0,
              from/0,

              proxy_process_ref/0,
              proxy_process_pid/0,
              real_process_pid/0,

              non_neg_milliseconds/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type function_name() :: atom().
-type mfargs() :: {module(), function_name(), [term()]}.

-type context() :: proxy_context:context().

-type start_opt() :: {spawn_opt, [spawn_opt()]}
                   | {timeout, timeout()}
                   | {debug, [debug_opt()]}
                   | {logger, logi:context_ref()}.

-type spawn_opt() :: link
                   | {priority, Level :: priority_level()}
                   | {fullsweep_after, Number :: non_neg_milliseconds()}
                   | {min_heap_size, Size :: non_neg_integer()}
                   | {min_bin_vheap_size, VSize :: non_neg_integer()}.

-type priority_level() :: low | normal | high | max.
                     
%% see: gen:debug_flag/0
-type debug_opt() :: trace
                   | log
                   | statistics
                   | debug
                   | {logfile, string()}.

-type message() :: term().
-type request() :: term().
-type from()    :: {Tag::reference(), pid()} | undefined.

-type proxy_process_ref() :: atom() | {atom(), node()} | proxy_process_pid().
-type proxy_process_pid() :: pid().
-type real_process_pid()  :: pid().

-type non_neg_milliseconds() :: non_neg_integer().

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv start(MFArgs, ProxySpecs, [])
-spec start(mfargs(), [proxy_module:spec()]) -> {ok, pid()} | ignore | {error, Reason} when
      Reason :: timeout | term().
start(MFArgs, ProxySpecs) ->
    start(MFArgs, ProxySpecs, []).

-spec start(mfargs(), [proxy_module:spec()], [start_opt()]) -> {ok, pid()} | ignore | {error, Reason} when
      Reason :: timeout | term().
start(MFArgs, ProxySpecs, Options) ->
    proxy_server:start(MFArgs, ProxySpecs, Options).

%% @equiv try_call(ProxyProcess, ProxyId, Request, 5000)
-spec try_call(proxy_process_ref(), proxy_module:id(), request()) -> term() | {error, Reason} when
      Reason :: not_found.
try_call(ProxyProcess, ProxyId, Request) ->
    try_call(ProxyProcess, ProxyId, Request, 5000).

-spec try_call(proxy_process_ref(), proxy_module:id(), request(), timeout()) -> term() | {error, Reason} when
      Reason :: not_found.
try_call(ProxyProcess, ProxyId, Request, Timeout) ->
    error(not_implemented, [ProxyProcess, ProxyId, Request, Timeout]).

%% @equiv call(ProxyProcess, ProxyId, Request, 5000)
-spec call(proxy_process_ref(), proxy_module:id(), request()) -> term().
call(ProxyProcess, ProxyId, Request) ->
    call(ProxyProcess, ProxyId, Request, 5000).

-spec call(proxy_process_ref(), proxy_module:id(), request(), timeout()) -> term().
call(ProxyProcess, ProxyId, Request, Timeout) ->
    error(not_implemented, [ProxyProcess, ProxyId, Request, Timeout]).

-spec cast(proxy_process_ref(), proxy_module:id(), request()) -> ok.
cast(ProxyProcess, ProxyId, Request) ->
    error(not_implemented, [ProxyProcess, ProxyId, Request]).

-spec reply(from(), term()) -> ok | ignroe.
reply(undefined, _Reply)                                     -> ignore;
reply({Tag, Pid}, Reply) when is_reference(Tag), is_pid(Pid) -> _ = Pid ! {Tag, Reply}, ok;
reply(From, Reply)                                           -> error(badarg, [From, Reply]).

-spec add(proxy_process_ref(), proxy_module:spec()) -> ok | {error, Reason} when
      Reason :: already_present.
add(ProxyProcess, ProxySpec) ->
    error(not_implemented, [ProxyProcess, ProxySpec]).

-spec remove(term(), context()) -> context().
remove(Reason, Context) ->
    error(not_implemented, [Reason, Context]).

-spec remove(proxy_process_ref(), proxy_module:id(), term()) -> ok | {error, Reason} when
      Reason :: proxy_not_found.
remove(ProxyProcess, ProxyId, Reason) ->
    error(not_implemented, [ProxyProcess, ProxyId, Reason]).

-spec swap(term(), proxy_module:spec(), context()) -> {ok, context()} | {error, Reason} when
      Reason :: already_present.
swap(Reason, ProxySpec, Context) ->
    error(not_implemented, [Reason, ProxySpec, Context]).

-spec swap(proxy_process_ref(), proxy_module:id(), term(), proxy_module:spec()) -> ok | {error, Reason} when
      Reason :: not_found | already_present.
swap(ProxyProcess, ProxyId, Reason, ProxySpec) ->
    error(not_implemented, [ProxyProcess, ProxyId, Reason, ProxySpec]).

-spec get_real_process(proxy_process_ref()) -> real_process_pid() | not_started.
get_real_process(ProxyProcess) ->
    proxy_server:get_real_process(ProxyProcess).

-spec which_modules(proxy_process_ref()) -> [proxy_module:id()].
which_modules(ProxyProcess) ->
    error(not_implemented, [ProxyProcess]).
