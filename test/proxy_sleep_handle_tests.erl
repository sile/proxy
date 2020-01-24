-module(proxy_sleep_handle_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ensureExited(Pid),
        (fun() ->
                 __Old = process_flag(trap_exit, true),
                 __Ref = monitor(process, Pid),
                 exit(Pid, normal),
                 receive
                     {'DOWN', __Ref, process, _, _} -> ?assert(true)
                 after 50 ->
                         ?assert(timeout)
                 end,
                 process_flag(trap_exit, __Old)
         end)()).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%---------------------------------------------------------------------------------------------------------------------

lifetime_test_() ->
    {setup,
     fun() ->
             Call = fun(a) -> {reply, a};
                       (_) -> noreply
                    end,
             Cast = fun({Pid, X}) -> Pid ! {cast, X}, ok end,
             Info = fun({Pid, X}) -> Pid ! {info, X}, ok end,
             StartTime = now_after(100, 0, 0),
             proxy:spawn(timer, sleep, [infinity],
                         [
                          {proxy_sleep_handle, [{call, Call}, {cast, Cast}, {info, Info}]},
                          {proxy_lifetime, [{start_time, StartTime}]}
                         ])
     end,
     fun(Pid) ->
             ?ensureExited(Pid)
     end,
     fun(Pid) ->
             [
              {"RealProcessが起動する前にcallがハンドリングできる",
               fun() ->
                       ?assertEqual(a, gen_server:call(Pid, a)),
                       ?assertExit(_, gen_server:call(Pid, b, 10)),
                       ?assert(is_process_alive(Pid))
               end},
              {"RealProcessが起動する前にcastがハンドリングできる",
               fun() ->
                       ?assertEqual(ok, gen_server:cast(Pid, {self(), a})),
                       receive
                           {cast, a} -> ?assert(true)
                       after 1000 ->
                               ?assert(timeout)
                       end
               end},
              {"RealProcessが起動する前にinfoがハンドリングできる",
               fun() ->
                       Pid ! {self(), a},
                       receive
                           {info, a} -> ?assert(true)
                       after 1000 ->
                               ?assert(timeout)
                       end
               end}
             ]
     end}.

%% @doc 現在時刻から指定時間後を返す.
-spec now_after(integer(), integer(), integer()) -> erlang:timestamp().
now_after(AddMega, AddSec, AddMicro) ->
    {Mega, Sec, Micro} = os:timestamp(),
    {Mega + AddMega, Sec + AddSec, Micro + AddMicro}.
