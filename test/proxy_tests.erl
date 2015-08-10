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

spawn_opt_test_() ->
    [
     {"minitorオプションが指定可能",
      fun () ->
              Parent = self(),
              {ProxyPid, Monitor} = proxy:spawn_opt(fun () -> Parent ! {pid, self()} end, [], [monitor]),

              receive
                  {pid, RealPid} ->
                      exit(ProxyPid, kill),
                      exit(RealPid, kill),
                      receive
                          {'DOWN', Monitor, _, ProxyPid, _} ->
                              %% プロキシプロセスは監視している
                              receive
                                  {'DOWN', _, _, _, _}  -> ?assert(false)
                              after 20 ->
                                      %% 実プロセスは監視対象外
                                      ?assertNot(is_process_alive(RealPid))
                              end
                      end
              end
      end}
    ].

start_test_() ->
    [
     {"無名関数が名前なしプロキシ経由で起動できる",
      fun () ->
              Parent = self(),
              {ok, ProxyPid} = proxy:start(erlang, apply, [fun () -> {ok, spawn(fun () -> Parent ! {pid, self()} end)} end, []], []),
              ?assert(is_pid(ProxyPid)),

              receive
                  {pid, RealPid} ->
                      ?assert(is_pid(RealPid)),
                      ?assertNot(ProxyPid =:= RealPid) % プロキシプロセス と 実プロセス のPIDは異なる
              end
      end},
     {"無名関数が名前付きプロキシ経由で起動できる",
      fun () ->
              Parent = self(),

              ?assertEqual(undefined, whereis(hoge_proxy)), % 未登録
              {ok, ProxyPid} = proxy:start({local, hoge_proxy}, erlang, apply, [fun () -> {ok, spawn(fun () -> Parent ! {pid, self()} end)} end, []], []),
              ?assertEqual(ProxyPid, whereis(hoge_proxy)), % 登録
              ?assert(is_pid(ProxyPid)),

              receive
                  {pid, RealPid} ->
                      ?assert(is_pid(RealPid)),
                      ?assertNot(ProxyPid =:= RealPid) % プロキシプロセス と 実プロセス のPIDは異なる
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

get_real_pid_test_() ->
    [
     {"実プロセスのPIDが取得できる",
      fun () ->
              Parent = self(),
              ProxyPid = proxy:spawn_opt(fun () -> Parent ! {pid, self()}, timer:sleep(infinity) end, [], [link]),

              receive
                  {pid, RealPid} ->
                      ?assertEqual(RealPid, proxy:get_real_pid(ProxyPid))
              end
      end}
    ].
