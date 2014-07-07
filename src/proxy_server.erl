%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc プロキシサーバプロセス
%% @private
-module(proxy_server).

-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         start/3,
         get_real_process/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(LOGGER(Context), proxy_context:get_logger(Context)).

-define(LOG_FUNCTION_FAILURE(Context, Reason, M, F, Arity),
        case is_normally_stop_reason(Reason) of
            false -> logi:notice(?LOGGER(Context), "~s:~s/~p is failed: reason=~p", [M, F, Arity, Reason]);
            true  -> logi:verbose(?LOGGER(Context), "~s:~s/~p is canceled: reason=~p", [M, F, Arity, Reason])
        end).

-type real_process() :: proxy:real_process_pid() | not_started.

-record(state,
        {
          driver  :: proxy_driver:driver(),
          context :: proxy_context:context()
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec start(proxy:mfargs(), [proxy_module:spec()], [proxy:start_opt()]) -> {ok, pid()} | ignore | {error, Reason} when
      Reason :: timeout | term().
start(MFArgs, ProxyModuleSpecs, Options) ->
    %% TODO: validate
    GenServerOptions = proxy_util:filter_gen_server_opts(Options),
    ProxyServerOptions = Options -- GenServerOptions, % XXX:
    gen_server:start(?MODULE, [MFArgs, ProxyModuleSpecs, ProxyServerOptions], GenServerOptions).

-spec get_real_process(proxy:proxy_process_ref()) -> real_process().
get_real_process(ServerRef) ->
    gen_server:call(ServerRef, get_real_process).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions 
%%----------------------------------------------------------------------------------------------------------------------
%% @hidden
init([MFArgs, ProxyModuleSpecs, Options]) ->
    Logger0 = proplists:get_value(logger, Options, logi:default_logger()),
    Logger1 = logi:debug(Logger0, "init: mfargs=~p, proxy_module_specs=~p, options=~p", [MFArgs, ProxyModuleSpecs, Options]),
    Logger2 = logi:verbose(Logger1, "init: proxy_modules=~p", [[Id || {Id, _} <- ProxyModuleSpecs]]),

    Context0 = proxy_context:make(Logger2),
    {Result0, Driver0, Context1} = ready_proxy_driver(MFArgs, ProxyModuleSpecs, Context0),
    case Result0 of
        {stop, Reason} ->
            _ = proxy_driver:terminate(Reason, Driver0, Context1),
            {stop, Reason};
        ok  ->
            _ = process_flag(trap_exit, true),
            {Result1, Driver1, Context2} = ready_real_process(Driver0, Context1),
            case Result1 of
                ignore ->
                    _ = proxy_driver:terminate({shutdown, ignore}, Driver1, Context2),
                    ignore;
                {stop, Reason} ->
                    _ = proxy_driver:terminate(Reason, Driver1, Context2),
                    {stop, Reason};
                {ok, Driver1, Context2} ->
                    Context2 =
                        proxy_context:with_logger(
                          fun (L0) ->
                                  RealProcess = proxy_context:get_real_process(Context1),
                                  L1 = logi:set_headers(L0, [{real, RealProcess}]),
                                  logi:verbose(L1, "proxy server is started")
                          end,
                          Context1),
                    State =
                        #state{
                           driver  = Driver1,
                           context = Context2
                          },
                    {ok, State}
            end
    end.

%% @hidden
handle_call(get_real_process, _From, State) ->
    {reply, proxy_context:get_real_process(State#state.context), State};
handle_call(Request, From, State) ->
    Context0 =
        proxy_context:with_logger(
          fun (Logger0) -> logi:warning_opt(Logger0, "unknown call: request=~p, from=~p", [Request, From],
                                            [{frequency, {interval, 5 * 60 * 1000}}])
          end,
          State#state.context),
    {noreply, State#state{context = Context0}}.

%% @hidden
handle_cast(Request, State) ->
    Context0 =
        proxy_context:with_logger(
          fun (Logger0) -> logi:warning_opt(Logger0, "unknown cast: request=~p", [Request],
                                            [{frequency, {interval, 5 * 60 * 1000}}])
          end,
          State#state.context),
    {noreply, State#state{context = Context0}}.
    
%% @hidden
handle_info({'EXIT', Pid, Reason}, State) ->
    #state{context = Context} = State,
    _ = case is_normally_stop_reason(Reason) of
            false -> logi:notice(?LOGGER(Context), "process ~p exited: reason=~p", [Pid, Reason]);
            true  -> logi:verbose(?LOGGER(Context), "process ~p exited: reason=~p", [Pid, Reason])
        end,
    case Pid =:= proxy_context:get_real_process(Context) of
        false -> {stop, Reason, State};
        true  ->
            case handle_down(Reason, State) of
                {stop, Reason1, State1} -> {stop, Reason1, State1};
                {ok, State1}            -> {noreply, State1}
            end
    end;
handle_info(Info, State) ->
    Context0 =
        proxy_context:with_logger(
          fun (Logger0) -> logi:warning_opt(Logger0, "unknown info: info=~p", [Info],
                                            [{frequency, {interval, 5 * 60 * 1000}}])
          end,
          State#state.context),
    {noreply, State#state{context = Context0}}.

%% @hidden
terminate(Reason, State) ->
    #state{context = Context0, driver = Driver0} = State,
    Context1 = proxy_driver:terminate(Reason, Driver0, Context0),
    _ = case is_normally_stop_reason(Reason) of
            false -> logi:notice(?LOGGER(Context1), "proxy server terminated: reason=~p", [Reason]);
            true  -> logi:verbose(?LOGGER(Context1), "proxy server terminated: reason=~p", [Reason])
        end,
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
%% TODO: move
-spec is_normally_stop_reason(proxy:stop_reason()) -> boolean().
is_normally_stop_reason(normal)        -> true;
is_normally_stop_reason(shutdown)      -> true;
is_normally_stop_reason({shutdown, _}) -> true;
is_normally_stop_reason(_)             -> false.

-spec ready_proxy_driver(proxy:mfargs(), [proxy_module:spec()], proxy_context:context()) ->
                                {Result, proxy_driver:driver(), proxy_context:context()} when
      Result :: ok | {stop, Reason::term()}.
ready_proxy_driver(MFArgs, ProxyModuleSpecs, Context0) ->
    {Result, Driver0, Context1} = proxy_driver:init(MFArgs, ProxyModuleSpecs, Context0),
    case Result of
        {stop, Reason} ->
            _ = ?LOG_FUNCTION_FAILURE(Context1, Reason, proxy_driver, init, 2),
            {{stop, Reason}, Driver0, Context1};
        ok ->
            {ok, Driver0, Context1}
    end.

-spec ready_real_process(proxy_driver:driver(), proxy_context:context()) ->
                                {Result, proxy_driver:driver(), proxy_context:context()} when
      Result :: ok | ignore | {stop, Reason::term()}.
ready_real_process(Driver0, Context0) ->
    {ArgsResult, Driver1, Context1} = handle_args(Driver0, Context0),
    case ArgsResult of
        {stop, Reason} -> {{stop, Reason}, Driver1, Context1};
        {hibernate, _} -> {ok, Driver1, Context1};
        ok             -> start_link_real_process(Driver1, Context1)
    end.

-spec handle_args(proxy_driver:driver(), proxy_context:context()) -> {Result, proxy_driver:driver(), proxy_context:context()} when
      Result :: ok | {hibernate, Reason} | {stop, Reason},
      Reason :: term().
handle_args(Driver0, Context0) ->
    {Result0, Driver1, Context1} = proxy_driver:handle_args(Driver0, Context0),
    case Result0 of
        {stop, Reason} ->
            _ = ?LOG_FUNCTION_FAILURE(Context1, Reason, proxy_driver, handle_args, 2),
            {{stop, Reason}, Driver1, Context1};
        ok ->
            {Result1, Driver2, Context2} = handle_context(Driver1, Context1),
            _ = case Result1 of
                    {stop, Reason}      -> ?LOG_FUNCTION_FAILURE(Context2, Reason, ?MODULE, handle_context, 2);
                    {hibernate, Reason} -> logi:verbose(?LOGGER(Context2), "real process goes in hibernation: reason=~p", [Reason]);
                    _                   -> ok
                end,
            {Result1, Driver2, Context2}
    end.
                            
-spec start_link_real_process(proxy_driver:driver(), proxy_context:context()) -> {Result, proxy_driver:driver(), proxy_context:context()} when
      Result :: ok | ignore | {stop, Reason::term()}.
start_link_real_process(Driver0, Context0) ->
    {Module, Function, Args} = MFArgs = proxy_driver:get_mfargs(Driver0),
    try apply(Module, Function, Args) of
        ignore ->
            _ = logi:verbose(?LOGGER(Context0), "~s:~s/~p returns 'ignore'", [Module, Function, length(Args)]),
            {ignore, Driver0, Context0};
        {error, Reason} ->
            _ = ?LOG_FUNCTION_FAILURE(Context0, Reason, Module, Function, length(Args)),
            handle_down(Reason, Driver0, Context0);
        {ok, Pid} ->
            true = link(Pid),
            _ = logi:verbose(?LOGGER(Context0), "real process is started: pid=~p, node=~s", [Pid, node(Pid)]),
            handle_up(Pid, Driver0, Context0);
        Other ->
            _ = logi:warning(?LOGGER(Context0), "~s:~s/~p returns unexpected result: ~p", [Module, Function, length(Args), Other]),
            {{stop, {unexpected_result, [{mfargs, MFArgs}, {result, Other}]}}, Driver0, Context0}
    catch
        ExClass:ExReason ->
            _ = logi:warning(?LOGGER(Context0), "exception raised: class=~p, reason=~p, trace=~p",
                             [ExClass, ExReason, erlang:get_stacktrace()]),
            {{stop, {exception, ExClass, ExReason, erlang:get_stacktrace()}}, Driver0, Context0}
    end.

-spec handle_up(pid(), proxy_driver:driver(), proxy_context:context()) -> {Result, proxy_driver:driver(), proxy_context:context()} when
      Result :: ok | {stop, Reason::term()}.
handle_up(Pid, Driver0, Context0) ->
    Context1 = proxy_context:set_real_process(Pid, Context0),
    {Result0, Driver1, Context2} = proxy_driver:handle_up(Driver0, Context1),
    case Result0 of
        {stop, Reason} ->
            _ = ?LOG_FUNCTION_FAILURE(Context2, Reason, proxy_driver, handle_up, 2),
            {Result0, Driver1, Context2};
        ok ->
            {Result1, Driver2, Context3} = handle_context(Driver1, Context2),
            case Result1 of
                {stop, Reason} ->
                    _ = ?LOG_FUNCTION_FAILURE(Context3, Reason, ?MODULE, handle_context, 2),
                    {{stop, Reason}, Driver2, Context3};
                {hibernate, Reason} ->
                    _ = logi:verbose(?LOGGER(Context3), "real process goes in hibernation: reason=~p", [Reason]),
                    ok = proxy_util:unlink_and_exit(Pid, Reason),
                    Context4 = proxy_context:set_real_process(not_started, Context3),
                    {ok, Driver2, Context4};
                ok ->
                    {ok, Driver2, Context3}
            end
    end.

-spec handle_down(term(), #state{}) -> {ok, #state{}} | {error, term(), #state{}}.
handle_down(Reason0, State) ->
    {Result, Driver, Context} = handle_down(Reason0, State#state.driver, State#state.context),
    case Result of
        {stop, Reason1} -> {stop, Reason1, State#state{driver = Driver, context = Context}};
        ok              -> {ok, State#state{driver = Driver, context = Context}}
    end.

%% TODO: handle_downはプロセスが確実に死んでから呼ばれるようにする
-spec handle_down(term(), proxy_driver:driver(), proxy_context:context()) -> {Result, proxy_driver:driver(), proxy_context:context()} when
      Result :: ok | {stop, Reason::term()}.
handle_down(Reason0, Driver0, Context0) ->
    {Result0, Driver1, Context1} = proxy_driver:handle_down(Reason0, Driver0, Context0),
    case Result0 of
        {stop, Reason1} ->
            _ = ?LOG_FUNCTION_FAILURE(Context1, Reason1, proxy_driver, handle_down, 3),
            {Reason0, Driver1, Context1};
        ok ->
            {Result1, Driver2, Context2} = handle_context(Driver1, Context1),
            case Result1 of
                {stop, Reason} ->
                    _ = ?LOG_FUNCTION_FAILURE(Context2, Reason, ?MODULE, handle_context, 2),
                    {{stop, Reason}, Driver2, Context2};
                {hibernate, Reason} ->
                    _ = logi:verbose(?LOGGER(Context2), "real process goes in hibernation: reason=~p", [Reason]),
                    ok = proxy_util:unlink_and_exit(Pid, Reason),
                    Context3 = proxy_context:set_real_process(not_started, Context2),
                    {ok, Driver2, Context3};
                ok ->
                    {ok, Driver2, Context2}
            end
    end.

-spec handle_context(proxy_driver:driver(), proxy_context:context()) -> {Result, proxy_driver:driver(), proxy_context:context()} when
      Result :: ok | {hibernate, Reason} | {stop, Reason},
      Reason :: term().
handle_context(Driver0, Context0) ->
    
      
