%% coding: latin-1
-module(proxy_tests).

-include_lib("eunit/include/eunit.hrl").

spawn_test_() ->
    [
     {"無名関数がプロキシ経由で起動できる",
      fun () ->
              Parent = self(),
              ProxyPid = proxy:spawn(fun () -> Parent ! {pid, self()} end, []),
              ?assert(is_pid(ProxyPid)),

              receive
                  {pid, RealPid} ->
                      ?assert(is_pid(RealPid)),
                      ?assertNot(ProxyPid =:= RealPid) % プロキシプロセス と 実プロセス のPIDは異なる
              end
      end},
     {"プロキシに送信したメッセージは、実プロセスに伝達される",
      fun () ->
              Parent = self(),
              ProxyPid = proxy:spawn(fun () -> receive ping -> Parent ! pong end end, []),
              ProxyPid ! ping,
              receive
                  pong -> ?assert(true)
              end
      end},
     {"実プロセスが死ねば、プロキシプロセスも死ぬ",
      fun () ->
              ProxyPid = proxy:spawn(fun () -> exit(hoge) end, []),
              monitor(process, ProxyPid),
              receive
                  {'DOWN', _, _, ProxyPid, Reason} ->
                      ?assertEqual(hoge, Reason)
              end
      end},
     {"プロキシプロセスが死ねば、実プロセスも死ぬ",
      fun () ->
              Parent = self(),
              ProxyPid = proxy:spawn(fun () -> Parent ! {pid, self()}, timer:sleep(infinity) end, []),
              RealPid = receive {pid, Pid} -> Pid end,

              exit(ProxyPid, hoge),

              monitor(process, RealPid),
              receive
                  {'DOWN', _, _, RealPid, Reason} ->
                      ?assertEqual(hoge, Reason)
              end
      end}
    ].

multi_proxy_test_() ->
    [
     {"複数のプロキシが登録できる",
      fun () ->
              Retry    = {proxy_restart, [{max_restart, 1000}]},
              LifeTime = {proxy_lifetime, [{start_time, now()}]},
              ProxyList = [Retry, LifeTime],

              %% このテストでは、複数プロキシを登録しても正常に起動できるかどうかだけを確認
              %% (それぞれがちゃんと機能しているかは確認しない)
              Parent = self(),
              ProxyPid = proxy:spawn(fun () -> receive ping -> Parent ! pong end end, ProxyList),
              ProxyPid ! ping,
              receive
                  pong -> ?assert(true)
              end
      end}
    ].
