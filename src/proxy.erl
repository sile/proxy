-module(proxy).

-export([
         spawn/2,
         spawn/4,
         spawn_opt/3,
         spawn_opt/5,
         start/4,
         start_link/4
        ]).

-export_type([
              proxy_spec/0,
              proxy_arg/0,
              proxy_state/0
             ]).

-callback init(proxy_arg()) ->
    {stop, term()} | ignore | {ok, proxy_state()}.

-callback handle_arg([term()], proxy_state()) ->
    {stop, term()} | {ok, [term()], proxy_state()} | {remove_proxy, [term()], proxy_state()} |
    {swap_proxy, [term()], term(), proxy_state(), module(), proxy_arg()}.

-callback handle_up(pid(), proxy_state()) ->
    {stop, term(), proxy_state()} | {ok, proxy_state()} | {remove_proxy, proxy_state()} |
    {swap_proxy, term(), proxy_state(), module(), proxy_arg()}.

-callback handle_message(term(), proxy_state()) ->
    {stop, term(), proxy_state()} | {ok, term(), proxy_state()} |
    {remove_proxy, term(), proxy_state()} | {remove_proxy, proxy_state()} |
    {ignore, proxy_state()} |
    {swap_proxy, term(), term(), proxy_state(), module(), proxy_arg()} |
    {swap_proxy, term(), proxy_state(), module(), proxy_arg()}.

-callback handle_down(term(), proxy_state()) ->
    {stop, term(), proxy_state()} | {ok, proxy_state()} |
    {restart, proxy_state()} | {restart, non_neg_integer(), proxy_state()}.

-callback terminate(term(), proxy_state()) -> any().

-type proxy_spec() :: {module(), proxy_arg()}.
-type proxy_state() :: term().
-type proxy_arg() :: term().

spawn(Fun, ProxySpecs) ->
    ?MODULE:spawn_opt(Fun, ProxySpecs, []).

spawn(Module, Function, Args, ProxySpecs) ->
    ?MODULE:spawn_opt(Module, Function, Args, ProxySpecs, []).

spawn_opt(Fun, ProxySpecs, SpawnOpts) ->
    StartFunc = proxy_start_func:make_spawn_func(Fun, SpawnOpts),
    erlang:spawn_opt(proxy_server, start_loop, [StartFunc, ProxySpecs], SpawnOpts). % TODO: 一部のオプション以外は切り取る

spawn_opt(Module, Function, Args, ProxySpecs, SpawnOpts) ->
    StartFunc = proxy_start_func:make_spawn_func(Module, Function, Args, SpawnOpts),
    erlang:spawn_opt(proxy_server, start_loop, [StartFunc, ProxySpecs], SpawnOpts).

start(Module, Function, Args, ProxySpecs) ->
    Ref  = make_ref(),
    From = {self(), Ref},
    StartFunc = proxy_start_func:make_start_fun(Module, Module, Function, Args),
    Pid = erlang:spawn(proxy_server, start_loop, [From, StartFunc, ProxySpecs]),
    Monitor = erlang:monitor(process, Pid),
    receive
        {'DOWN', _, _, Pid, Reason} -> {error, Reason};
        {Ref, Result}               ->
            _ = erlang:demonitor(Monitor, [flush]),
            Result
    end.

start_link(Module, Function, Args, ProxySpecs) ->
    Ref  = make_ref(),
    From = {self(), Ref},
    StartFunc = proxy_start_func:make_start_fun(Module, Module, Function, Args),
    Pid = erlang:spawn_link(proxy_server, start_loop, [From, StartFunc, ProxySpecs]),
    Monitor = erlang:monitor(process, Pid),
    receive
        {'DOWN', _, _, Pid, Reason} -> {error, Reason};
        {Ref, Result}               ->
            _ = erlang:demonitor(Monitor, [flush]),
            Result
    end.
