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
              proxy_spec/0
             ]).

-type proxy_spec() :: todo.


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
