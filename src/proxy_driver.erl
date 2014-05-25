-module(proxy_driver).

-export([
         init/1,
         handle_arg/2,
         handle_message/2
        ]).

-export_type([
              state/0
             ]).

-define(STATE, ?MODULE).

-record(?STATE,
        {
          proxy_list = [] :: [proxy()]
        }).

-type proxy() :: {module(), proxy_state()}.
-type proxy_state() :: term().

-opaque state() :: #?STATE{}.

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

-spec invoke_proxy_list(Fun, term(), state()) -> {Result, state()} when
      Fun    :: fun ((term(), proxy()) -> {term()} | {term(), proxy()}),
      Result :: ok | {stop, Reason},
      Reason :: term().
invoke_proxy_list(Fun, Arg, State) ->
    {Result, ProxyList} =
        lists:foldl(
          fun (Proxy0, {{ok, Arg0}, Acc}) ->
                  try Fun(Arg0, Proxy0) of
                      {Result}         -> {Result, Acc};
                      {Result, Proxy1} -> {Result, [Proxy1 | Acc]}
                  catch
                      Class:Reason -> {{stop, make_exception_reason(Class, Reason)}, [Proxy0 | Acc]}
                  end
          end,
          {{ok, Arg}, []},
          State#?STATE.proxy_list),
    {Result, State#?STATE{proxy_list = lists:reverse(ProxyList)}}.

-spec handle_arg(term(), state()) -> {Result, state()} when
      Result :: {ok, term()} | {stop, Reason::term()}.
handle_arg(Arg, State) ->
    invoke_proxy_list(fun invoke_handle_arg/2, Arg, State).

-spec handle_message(term(), state()) -> {Result, state()} when
      Result :: {ok, term()} | ignore | {stop, Reason::term()}.
handle_message(Message, State) ->
    invoke_proxy_list(fun invoke_handle_message/2, Message, State).

-spec invoke_init(proxy:proxy_spec()) -> {ok, proxy()} | ignore | {stop, Reason::term()}.
invoke_init({Module, Arg}) ->
    case Module:init(Arg) of
        {stop, Reason} -> {stop, Reason};
        ignore         -> ignore;
        {ok, State}    -> {ok, {Module, State}};
        Other          -> error({unexpected_return, {Module, init, [Arg]}, Other}, [Module, Arg])
    end.

%%-spec invoke_handle_arg(term(), proxy()) -> {ok, term(), proxy()} | {stop, term()} | {stop, term(), proxy()} | {remove_proxy, term()}.
invoke_handle_arg(Arg0, {Module, State0}) ->
    Result = Module:handle_arg(Arg0, State0),
    case handle_invoke_result_common(Result) of
        {delegate, Other} -> error({unexpected_return, {Module, handle_arg, [Arg0, State0]}, Other}, [Arg0, {Module, State0}]);
        _                 -> Result
    end.

%% -spec handle_invoke_result_common() -> todo.
handle_invoke_result_common(Module, Result) ->
    case Result of
        {stop, Reason, State}        -> {{stop, Reason}, {Module, State}};
        {ok, Value, State}           -> {{ok, Value}, {Module, State}};
        {remove_proxy, Value, State} ->
            _ = (catch invoke_terminate(remove_proxy, {Module, State})), % TODO: log
            {{ok, Value}};
        {swap_proxy, Value, SwapReason, State, NewModule, NewArg} ->
            try
                TerminateValue = invoke_terminate(SwapReason, {Module, State}),
                case invoke_init({NewModule, {NewArg, TerminateValue}}) of
                    {stop, Reason} -> {{stop, Reason}};
                    ignore         -> {{ok, Value}};
                    {ok, NewState} -> {{ok, Value}, {NewModule, NewState}}
                end
            catch
                ExClass:ExReason -> {{stop, make_exception_reason(ExClass, ExReason)}}
            end;
        Other -> {delegate, Other}
    end.

%%-spec invoke_handle_message(term(), proxy()) -> todo.
invoke_handle_message(Msg0, {Module, State0}) ->
    Result = Module:handle_message(Msg0, State0),
    case Result of
        {delegate, Delegated} ->
            case Delegated of
                {ignore, State1} -> {{ok, Msg0}, {Module, State1}};
                
        _                     -> Result
    end.

-spec invoke_terminate(term(), proxy()) -> term().
invoke_terminate(Reason, {Module, State}) ->
    Module:terminate(Reason, State).

-spec make_exception_reason(atom(), term()) -> proxy:exception_reason().
make_exception_reason(Class, Reason) ->
    {'EXCEPTION', {Class, Reason, erlang:get_stacktrace()}}.
