%% coding: latin-1
%%
%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
-module(proxy_tests).

-include_lib("eunit/include/eunit.hrl").

-export([echo_loop/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
spawn_test_() ->
    [
     {"プロキシ付きでプロセスを起動できる",
      fun () ->
              Modules  = [{proxy_module_message_log, []}],
              ProxyPid = proxy:start({?MODULE, echo_loop, []}, Modules),
              RealPid  = proxy:get_real_process(ProxyPid),
              ?assertNot(ProxyPid =:= RealPid),

              ProxyPid ! {self(), hello}, % メッセージはプロキシプロセスに送信する
              receive
                  {echo, PeerPid, hello} ->
                      ?assertEqual(RealPid, PeerPid), % 応答は実プロセスから返ってくる
                      ?assertEqual({self(), hello}, proxy_module_message_log:get_history(RealPid)) % プロキシを経由している
              end
      end}
    ].

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec echo_loop() -> no_return().
echo_loop() ->
    receive
        {From, Msg} ->
            From ! {echo, self(), Msg},
            echo_loop()
    end.
