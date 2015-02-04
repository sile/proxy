%% @doc proxyサーバで用いるドライバ.
%% @private
-module(proxy_driver).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         init/1,
         handle_arg/2,
         handle_up/2,
         handle_message/2,
         handle_down/2,
         terminate/2
        ]).

-export_type([
              state/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(STATE, ?MODULE).

-record(?STATE,
        {
          proxy_list = [] :: [proxy()]
        }).

-type proxy_behaiviour_module() :: module().
%% @doc proxy を behaviour として宣言しているモジュール.

-type proxy() :: {proxy_behaiviour_module(), proxy:proxy_state()}.

-opaque state() :: #?STATE{}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec init([proxy:proxy_spec()]) -> {Result, state()} when
      Result :: ok | {stop, Reason::term()}.
init(ProxySpecs) ->
    {Result, ProxyList} =
        lists:foldl(
          fun (_Spec, {{stop, Reason}, Acc}) -> {{stop, Reason}, Acc};
              (Spec,  {ok, Acc})             ->
                  try invoke_init(Spec) of
                      {stop, Reason} -> {{stop, Reason}, Acc};
                      ignore         -> {ok, Acc};
                      {ok, Proxy}    -> {ok, [Proxy | Acc]}
                  catch
                      Class:Reason ->
                          {{stop, make_exception_reason(Class, Reason)}, Acc}
                  end
          end,
          {ok, []},
          ProxySpecs),
    State = #?STATE{proxy_list = lists:reverse(ProxyList)},
    {Result, State}.

-spec terminate(term(), state()) -> state().
terminate(Reason, State) ->
    ok = lists:foreach(
           fun (Proxy) ->
                   _ = (catch invoke_terminate(Reason, Proxy)) % TODO: log
           end,
           State#?STATE.proxy_list),
    #?STATE{proxy_list = []}.

-spec handle_arg(term(), state()) -> {Result, state()} when
      Result :: {ok, term()} | {stop, Reason::term()} | {hibernate, Arg::[term()]}.
handle_arg(Arg, State) ->
    invoke_proxy_list(fun invoke_handle_arg/3, Arg, State).

-spec handle_up(pid(), state()) -> {Result, state()} when
      Result :: ok | {stop, Reason::term()}.
handle_up(RealPid, State) ->
    {Result, State2} = invoke_proxy_list(fun invoke_handle_up/3, RealPid, State),
    case Result of
        {ok, _} -> {ok, State2};
        _       -> {Result, State2}
    end.

-spec handle_message(term(), state()) -> {Result, state()} when
      Result :: {ok, term()} | ignore | {stop, Reason::term()}.
handle_message(Message, State) ->
    case invoke_proxy_list(fun invoke_handle_message/3, Message, State) of
        {{ignore, _}, State2} -> {ignore, State2};
        Other                 -> Other
    end.

-spec handle_down(term(), state()) -> {Result, state()} when
      Result :: ok
              | {restart, After::non_neg_integer()}.
handle_down(Reason, State) ->
    {Result, State2} = invoke_proxy_list(fun invoke_handle_down/3, Reason, State),
    case Result of
        {restart, _} -> {Result, State2};
        _            -> {ok, State2}
    end.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec invoke_proxy_list(Fun, term(), state()) -> {Result, state()} when
      Fun :: fun ((term(), term(), term()) -> {term(), term()} | {{term(), term()}, proxy()}),
      Result :: {ok, term()} | {stop, Reason} | {hibernate, Arg} | {ignore, term()} | {restart, After},
      Reason :: term(),
      Arg :: [term()],
      After :: term().
invoke_proxy_list(Fun, Arg, State) ->
    {Result, ProxyList} =
        lists:foldl(
          fun (Proxy0, {{PrevResult, Arg0}, Acc}) ->
                  try Fun(PrevResult, Arg0, Proxy0) of
                      {Result}         -> {Result, Acc};
                      {Result, Proxy1} -> {Result, [Proxy1 | Acc]}
                  catch
                      Class:Reason -> {{stop, make_exception_reason(Class, Reason)}, [Proxy0 | Acc]}
                  end
          end,
          {{ok, Arg}, []},
          State#?STATE.proxy_list),
    {Result, State#?STATE{proxy_list = lists:reverse(ProxyList)}}.

-spec invoke_init(proxy:proxy_spec()) -> {ok, proxy()} | ignore | {stop, Reason::term()}.
invoke_init({Module, Arg}) ->
    case Module:init(Arg) of
        {stop, Reason} -> {stop, Reason};
        ignore         -> ignore;
        {ok, State}    -> {ok, {Module, State}};
        Other          -> error({unexpected_return, {Module, init, [Arg]}, Other}, [Module, Arg])
    end.

-spec invoke_handle_arg(PrevResult, Arg, proxy()) -> Result | {Result, proxy()} when
      PrevResult :: term(),
      Result :: {ok, Arg} | {stop, Reason} | {hibernate, Arg},
      Arg :: [term()],
      Reason :: term().
invoke_handle_arg(_Prev, Arg0, {Module, State0}) ->
    case Module:handle_arg(Arg0, State0) of
        {stop, Reason, State1}       -> {{stop, Reason}, {Module, State1}};
        {hibernate, Arg1, State1}    -> {{hibernate, Arg1}, {Module, State1}};
        {ok, Arg1, State1}           -> {{ok, Arg1}, {Module, State1}};
        {remove_proxy, Arg1, State1} -> _ = remove_proxy({Module, State1}), {{ignore, Arg1}};
        {swap_proxy, Arg1, SwapReason, State1, NewModule, NewArg} ->
            case swap_proxy(SwapReason, {Module, State1}, NewModule, NewArg) of
                {stop, Reason} -> {{stop, Reason}};
                ignore         -> {{ok, Arg1}};
                {ok, NewProxy} -> {{ok, Arg1}, NewProxy}
            end;
        Other -> error({unexpected_return, {Module, handle_arg, [Arg0, State0]}, Other}, [Arg0, {Module, State0}])
    end.

-spec invoke_handle_up(PrevResult, pid(), proxy()) -> {Result, proxy()} when
      PrevResult :: term(),
      Result :: {ok, pid()} | {stop, Reason},
      Reason :: term().
invoke_handle_up(_Prev, RealPid, {Module, State0}) ->
    case Module:handle_up(RealPid, State0) of
        {stop, Reason, State1} -> {{stop, Reason}, {Module, State1}};
        {ok, State1}           -> {{ok, RealPid}, {Module, State1}};
        {remove_proxy, State1} -> _ = remove_proxy({Module, State1}), {{ignore, RealPid}};
        {swap_proxy, SwapReason, State1, NewModule, NewArg} ->
            case swap_proxy(SwapReason, {Module, State1}, NewModule, NewArg) of
                {stop, Reason} -> {{stop, Reason}};
                ignore         -> {{ok, RealPid}};
                {ok, NewProxy} -> {{ok, RealPid}, NewProxy}
            end;
        Other -> error({unexpected_return, {Module, handle_up, [RealPid, State0]}, Other}, [RealPid, {Module, State0}])
    end.

-spec invoke_handle_down(Arg1, Arg2, Arg3) -> Return when
      %% 縦で1組.
      %% 関数のパターンマッチのように書こうとすると overlap で error が出るため, このように記述.
      Arg1   :: restart                     | PrevResult,
      Arg2   :: After                       | ExitReason,
      Arg3   :: state()                     | proxy(),
      Return :: {{restart, After}, state()} | {Result, proxy()},

      After :: non_neg_integer(),

      PrevResult :: term(),
      ExitReason :: term(),
      Result :: {ok, ExitReason} | {stop, Reason} | {restart, After},
      Reason :: term().
invoke_handle_down(restart, After, State) ->
    {{restart, After}, State};
invoke_handle_down(_Prev, ExitReason, {Module, State0}) ->
    case Module:handle_down(ExitReason, State0) of
        {stop, Reason, State1}   -> {{stop, Reason}, {Module, State1}};
        {ok, State1}             -> {{ok, ExitReason}, {Module, State1}};
        {restart, State1}        -> {{restart, 0}, {Module, State1}};
        {restart, After, State1} -> {{restart, After}, {Module, State1}};
        Other -> error({unexpected_return, {Module, handle_down, [ExitReason, State0]}, Other}, [ExitReason, {Module, State0}])
    end.

-spec invoke_handle_message(Arg1, Arg2, proxy()) -> Return when
      Arg1   :: stop   | PrevResult,
      Arg2   :: Reason | Msg,
      Return :: term() | {Result, proxy()},

      Reason :: term(),

      PrevResult :: term(),
      Msg :: term(),
      Result :: {ok, Msg} | {stop, Reason} | ignore.
invoke_handle_message(stop, Reason, {Module, State}) ->
    {{stop, Reason}, {Module, State}};
invoke_handle_message(ignore, Msg0, {Module, State}) ->
    Result = invoke_handle_message(ok, Msg0, {Module, State}),
    case element(1, element(1, Result)) of
        stop -> Result;
        _    -> setelement(1, Result, setelement(1, element(1, Result), ignore))
    end;
invoke_handle_message(_Prev, Msg0, {Module, State0}) ->
    case Module:handle_message(Msg0, State0) of
        {stop, Reason, State1}       -> {{stop, Reason}, {Module, State1}};
        {ok, Msg1, State1}           -> {{ok, Msg1}, {Module, State1}};
        {remove_proxy, Msg1, State1} -> _ = remove_proxy({Module, State1}), {{ok, Msg1}};
        {remove_proxy, State1}       -> _ = remove_proxy({Module, State1}), {{ok, Msg0}};
        {ignore, State1}             -> {{ignore, Msg0}, {Module, State1}};
        Other                        ->
            {InvokeResult, SwapResult} =
                case Other of
                    {swap_proxy, Msg1, SwapReason, State1, NewModule, NewArg} ->
                        {{ok, Msg1}, swap_proxy(SwapReason, {Module, State1}, NewModule, NewArg)};
                    {swap_proxy, SwapReason, State1, NewModule, NewArg}       ->
                        {{ignore, Msg0}, swap_proxy(SwapReason, {Module, State1}, NewModule, NewArg)};
                    _ -> error({unexpected_return, {Module, handle_message, [Msg0, State0]}, Other}, [Msg0, {Module, State0}])
                end,
            case SwapResult of
                {stop, Reason} -> {{stop, Reason}};
                ignore         -> {InvokeResult};
                {ok, NewProxy} -> {InvokeResult, NewProxy}
            end
    end.

-spec remove_proxy(proxy()) -> any().
remove_proxy({Module, State}) ->
    (catch invoke_terminate(remove_proxy, {Module, State})). % TODO: log

-spec swap_proxy(term(), proxy(), proxy_behaiviour_module(), term()) -> {stop, term()} | ignore | {ok, proxy()}.
swap_proxy(SwapReason, Proxy, NewModule, NewArg) ->
    try
        TerminateValue = invoke_terminate(SwapReason, Proxy),
        invoke_init({NewModule, {NewArg, TerminateValue}})
    catch
        ExClass:ExReason -> {stop, make_exception_reason(ExClass, ExReason)}
    end.

-spec invoke_terminate(term(), proxy()) -> term().
invoke_terminate(Reason, {Module, State}) ->
    Module:terminate(Reason, State).

-spec make_exception_reason(atom(), term()) -> term().
%% -spec make_exception_reason(atom(), term()) -> proxy:exception_reason().
make_exception_reason(Class, Reason) ->
    {'EXCEPTION', {Class, Reason, erlang:get_stacktrace()}}.
