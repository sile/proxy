%% @doc proxyサーバモジュール
%% @private
-module(proxy_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         start_loop/2, start_loop/4,
         loop/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types & Records
%%----------------------------------------------------------------------------------------------------------------------
%-type proxy() :: {module(), proxy_state()}.
%-type proxy_state() :: term().

-record(state,
        {
          tag = make_ref() :: reference(),
          start_func :: proxy_start_func:real_func(),
          real_pid :: pid() | hibernate,
          driver :: proxy_driver:state()
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc proxy サーバを開始する.
-spec start_loop(proxy_start_func:real_func(), [proxy:proxy_spec()]) -> no_return(). % start_loop/3
start_loop(StartFunc, ProxySpecs) ->
    start_loop(undefined, undefined, StartFunc, ProxySpecs).

%% @doc proxy サーバを開始する.
-spec start_loop(proxy:proxy_name() | undefined, From, proxy_start_func:real_func(), [proxy:proxy_spec()]) -> no_return() when % loop/1
      From :: {pid(), Tag::term()} | undefined.
start_loop(Name, From, StartFunc, ProxySpecs) when Name =/= undefined ->
    case proxy_lib:name_register(Name) of
        {false, Pid} -> exit({already_started, Pid});
        true         -> start_loop(undefined, From, StartFunc, ProxySpecs)
    end;
start_loop(_Name, From, StartFunc0, ProxySpecs) ->
    {InitResult, Driver0} = proxy_driver:init(ProxySpecs),
    case InitResult of
        {stop, Reason} -> terminate(Reason, Driver0);
        ok ->
            _ = process_flag(trap_exit, true),
            {RealPid, StartFunc1, Driver1} = start_real_process(StartFunc0, Driver0),
            _ = case From of
                    undefined -> ok;
                    _         -> reply(From, {ok, self()})
                end,
            State = #state{
                       start_func = StartFunc1,
                       real_pid   = RealPid,
                       driver     = Driver1
                      },
            ?MODULE:loop(State)
    end.

%% @doc proxy サーバのメインループ.
-spec loop(#state{}) -> no_return(). % terminate/2
loop(State = #state{tag = Tag, real_pid = RealPid}) ->
    receive
        {'EXIT', RealPid, Reason} -> % process 終了時
            {Result, Driver0} = proxy_driver:handle_down(Reason, State#state.driver),
            State2 = State#state{driver = Driver0},
            case Result of
                {restart, After} ->
                    _ = erlang:send_after(After, self(), {Tag, restart}),
                    ?MODULE:loop(State2);
                ok ->
                    _ = terminate(Reason, Driver0)
            end;
        {Tag, restart} -> % 再起動
            State2 = restart_real_process(State),
            ?MODULE:loop(State2);
        {'$proxy_call', From, get_real_process} ->
            _ = reply(From, State#state.real_pid),
            ?MODULE:loop(State);
        {'__SYSTEM__', 'RESUME'} ->
            %% TODO: システムメッセージのハンドリングを仕組みはもっとちゃんとする
            case State#state.real_pid =:= hibernate of
                true ->
                    State2 = restart_real_process(State),
                    ?MODULE:loop(State2);
                false ->
                    _ = logi:warning("real process is already running"),
                    ?MODULE:loop(State)
            end;
        Message -> % TODO: {'$proxy_call', From, term()} が来た時にここで処理することは問題がない?
            {Result, Driver0} = proxy_driver:handle_message(Message, State#state.driver),
            State2 = State#state{driver = Driver0},
            case Result of
                ignore                    -> ?MODULE:loop(State2);
                {stop, Reason}            -> _ = terminate(Reason, Driver0);
                {ok, {'EXIT', _, Reason}} ->
                    true = exit(RealPid, Reason),
                    ?MODULE:loop(State2);
                {ok, Message1}            ->
                    _ = RealPid ! Message1,
                    ?MODULE:loop(State2)
            end
    end.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec terminate(term(), proxy_driver:state()) -> no_return(). % exit/1
terminate(Reason, ProxyDriver) ->
    _ = proxy_driver:terminate(Reason, ProxyDriver),
    exit(Reason).

-spec restart_real_process(#state{}) -> #state{}.
restart_real_process(State) ->
    {RealPid, StartFunc, Driver} = start_real_process(State#state.start_func, State#state.driver),
    State#state{real_pid = RealPid, driver = Driver, start_func = StartFunc}.

-spec start_real_process(StartFuncIn, DriverIn) -> {pid() | hibernate, StartFuncOut, DriverOut} when
      StartFuncIn :: proxy_start_func:real_func(),
      DriverIn :: proxy_driver:state(),
      StartFuncOut :: proxy_start_func:real_func(),
      DriverOut :: proxy_driver:state().
start_real_process(StartFunc0, Driver0) ->
    {DoStart, StartFunc1, Driver1} = ready_start_func(StartFunc0, Driver0),
    case DoStart of
        false -> {hibernate, StartFunc1, Driver1};
        true  ->
            case proxy_start_func:start_link(StartFunc1) of
                {error, Reason2} -> terminate(Reason2, Driver1);
                {ok, RealPid} when is_pid(RealPid) ->
                    {HandleUpResult, Driver2} = proxy_driver:handle_up(RealPid, Driver1),
                    case HandleUpResult of
                        {stop, Reason3} -> terminate(Reason3, Driver2);
                        ok              -> {RealPid, StartFunc1, Driver2}
                    end
            end
    end.

-spec ready_start_func(StartFuncIn, DriverIn) -> {DoStart, StartFuncOut, DriverOut} when
      StartFuncIn :: proxy_start_func:real_func(),
      DriverIn :: proxy_driver:state(),
      DoStart :: boolean(),
      StartFuncOut :: proxy_start_func:real_func(),
      DriverOut :: proxy_driver:state().
ready_start_func(StartFunc0, Driver0) ->
    case proxy_start_func:get_args(StartFunc0) of
        error       -> {true, StartFunc0, Driver0};
        {ok, Args0} ->
            {HandleArgResult, Driver1} = proxy_driver:handle_arg(Args0, Driver0),
            case HandleArgResult of
                {stop, Reason}   -> terminate(Reason, Driver1);
                {ok, Args1}      ->
                    StartFunc1 = proxy_start_func:set_args(Args1, StartFunc0),
                    {true, StartFunc1, Driver1};
                {hibernate, Args1} ->
                    %% TODO: arg無しの場合のでもここに来れるようにする (handle_argを拡張)
                    StartFunc1 = proxy_start_func:set_args(Args1, StartFunc0),
                    {false, StartFunc1, Driver1}
            end
    end.

-spec reply({pid(), term()}, term()) -> ok.
reply({Pid, Tag}, Message) ->
    _ = Pid ! {Tag, Message},
    ok.
