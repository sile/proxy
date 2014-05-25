-module(proxy).

-export([
         spawn/2
        ]).

-export_type([
              proxy_spec/0
             ]).

-type proxy_spec() :: todo.

-spec spawn(function(), [proxy_spec()]) -> pid().
spawn(Fun, ProxySpecs) ->
    spawn(proxy_driver, start_loop, [Fun, ProxySpecs]).
