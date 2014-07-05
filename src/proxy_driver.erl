-module(proxy_driver).

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

-spec terminate(term(), state()) -> state().
terminate(Reason, State) ->
    ok = lists:foreach(
           fun (Proxy) ->
                   _ = (catch invoke_terminate(Reason, Proxy)) % TODO: log
           end,
           State#?STATE.proxy_list),
    #?STATE{proxy_list = []}.

-spec invoke_proxy_list(Fun, term(), state()) -> {Result, state()} when
      Fun    :: fun ((term(), proxy()) -> {term()} | {term(), proxy()}),
      Result :: {ok, term()} | {stop, Reason},
      Reason :: term().
invoke_proxy_list(Fun, Arg, State) ->
    {Result, ProxyList} =
        lists:foldl(
          fun (Proxy0, {{_Result, Arg0}, Acc}) ->
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

-spec handle_up(pid(), state()) -> {Result, state()} when
      Result :: ok | {stop, Reason::term()}.
handle_up(RealPid, State) ->
    {Result, State2} = invoke_proxy_list(fun invoke_handle_up/2, RealPid, State),
    case Result of
        {ok, _} -> {ok, State2};
        _       -> {Result, State2}
    end.
      
-spec handle_message(term(), state()) -> {Result, state()} when
      Result :: {ok, term()} | ignore | {stop, Reason::term()}.
handle_message(Message, State) ->
    invoke_proxy_list(fun invoke_handle_message/2, Message, State).

-spec handle_down(term(), state()) -> {Result, state()} when
      Result :: ok
              | {restart, After::non_neg_integer()}.
handle_down(Reason, State) ->
    {Result, State2} = invoke_proxy_list(fun invoke_handle_down/2, Reason, State),
    case Result of
        {restart, _} -> {Result, State2};
        _            -> {ok, State2}
    end.

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
    case Module:handle_arg(Arg0, State0) of
        {stop, Reason, State1}       -> {{stop, Reason}, {Module, State1}};
        {hibernate, Arg1, State1}    -> {{hibernate, Arg1}, {Module, State1}};
        {ok, Arg1, State1}           -> {{ok, Arg1}, {Module, State1}};
        {remove_proxy, Arg1, State1} -> _ = remove_proxy({Module, State1}), {{ok, Arg1}};
        {swap_proxy, Arg1, SwapReason, State1, NewModule, NewArg} ->
            case swap_proxy(SwapReason, {Module, State1}, NewModule, NewArg) of
                {stop, Reason} -> {{stop, Reason}};
                ignore         -> {{ok, Arg1}};
                {ok, NewProxy} -> {{ok, Arg1}, NewProxy}
            end;
        Other -> error({unexpected_return, {Module, handle_arg, [Arg0, State0]}, Other}, [Arg0, {Module, State0}])
    end.

invoke_handle_up(RealPid, {Module, State0}) ->
    case Module:handle_up(RealPid, State0) of
        {stop, Reason, State1} -> {{stop, Reason}, {Module, State1}};
        {ok, State1}           -> {{ok, RealPid}, {Module, State1}};
        {remove_proxy, State1} -> _ = remove_proxy({Module, State1}), {{ok, RealPid}};
        {swap_proxy, SwapReason, State1, NewModule, NewArg} ->
            case swap_proxy(SwapReason, {Module, State1}, NewModule, NewArg) of
                {stop, Reason} -> {{stop, Reason}};
                ignore         -> {{ok, RealPid}};
                {ok, NewProxy} -> {{ok, RealPid}, NewProxy}
            end;
        Other -> error({unexpected_return, {Module, handle_up, [RealPid, State0]}, Other}, [RealPid, {Module, State0}])
    end.

invoke_handle_down(ExitReason, {Module, State0}) ->
    case Module:handle_down(ExitReason, State0) of
        {stop, Reason, State1}   -> {{stop, Reason}, {Module, State1}};
        {ok, State1}             -> {{ok, ExitReason}, {Module, State1}};
        {restart, State1}        -> {{restart, 0}, {Module, State1}};
        {restart, After, State1} -> {{restart, After}, {Module, State1}};
        Other -> error({unexpected_return, {Module, handle_down, [ExitReason, State0]}, Other}, [ExitReason, {Module, State0}])
    end.

%%-spec invoke_handle_message(term(), proxy()) -> todo.
invoke_handle_message(Msg0, {Module, State0}) ->
    case Module:handle_message(Msg0, State0) of
        {stop, Reason, State1}       -> {{stop, Reason}, {Module, State1}};
        {ok, Msg1, State1}           -> {{ok, Msg1}, {Module, State1}};
        {remove_proxy, Msg1, State1} -> _ = remove_proxy({Module, State1}), {{ok, Msg1}};
        {remove_proxy, State1}       -> _ = remove_proxy({Module, State1}), {ignore};
        {ignore, State1}             -> {ignore, {Module, State1}};
        Other                        ->
            {InvokeResult, SwapResult} =
                case Other of
                    {swap_proxy, Msg1, SwapReason, State1, NewModule, NewArg} ->
                        {{ok, Msg1}, swap_proxy(SwapReason, {Module, State1}, NewModule, NewArg)};
                    {swap_proxy, SwapReason, State1, NewModule, NewArg}       ->
                        {ignore, swap_proxy(SwapReason, {Module, State1}, NewModule, NewArg)};
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

-spec swap_proxy(term(), proxy(), module(), term()) -> {stop, term()} | ignore | {ok, proxy()}.
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

-spec make_exception_reason(atom(), term()) -> proxy:exception_reason().
make_exception_reason(Class, Reason) ->
    {'EXCEPTION', {Class, Reason, erlang:get_stacktrace()}}.
