-module(proxy_lifetime_tests).

-include_lib("eunit/include/eunit.hrl").

-export([sleep_infinity/0]).

restart_test_() ->
    [
     {"実プロセスの開始時刻と終了時刻を指定できる",
      fun () ->
              StartTime = now_after(100),  % 100ms後
              StopTime  = now_after(200), % 200ms後

              ProxyPid =
                  proxy:spawn(?MODULE, sleep_infinity, [],
                              [{proxy_lifetime, [{start_time, StartTime}, {stop_time, StopTime}]}]),
              ?assertEqual(hibernate, proxy:call(ProxyPid, get_real_process)),
              timer:sleep(110),

              ?assertMatch(Pid when is_pid(Pid), proxy:call(ProxyPid, get_real_process)),
              timer:sleep(110),

              monitor(process, ProxyPid),
              receive
                  {'DOWN', _, _, ProxyPid, _} ->
                      ?assert(true)
              after 0 ->
                      ?assert(false)
              end
      end}
    ].

sleep_infinity() ->
    timer:sleep(infinity).

now_after(Ms) ->
    {MegaSecs, Secs, MicroSecs} = now(),
    {MegaSecs, Secs, MicroSecs + Ms * 1000}.
