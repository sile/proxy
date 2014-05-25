-module(proxy_restart_tests).

-include_lib("eunit/include/eunit.hrl").

restart_test_() ->
    [
     {"指定回数だけ実プロセスの再起動が行われる",
      fun () ->
              Count = 3,
              Parent = self(),
              ProxyPid =
                  proxy:spawn(fun () ->
                                      Parent ! hello
                              end,
                              [{proxy_restart, [{max_restart, Count}]}]),
              link(ProxyPid),
              lists:foreach(
                fun (_) ->
                        receive
                            hello -> ?assert(true)
                        end
                end,
                lists:seq(1, Count))
      end}
    ].
