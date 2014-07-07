%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc ユーティリティ関数をまとめたモジュール
-module(proxy_util).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         filter_native_spawn_options/1,
         filter_gen_server_opts/1
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec filter_native_spawn_options([term()]) -> [proxy:native_spawn_option()].
filter_native_spawn_options(Options) ->
    lists:filter(
      fun ({K, _}) -> lists:member(K, [priority, fullsweep_after, min_heap_size, min_bin_vheap_size]);
          (X)      -> lists:member(X, [link])
      end,
      Options).

-spec filter_gen_server_opts([term()]) -> [term()].
filter_gen_server_opts(Options) ->
    [Opt || Opt = {K, _} <- Options, lists:member(K, [spawn_opt, timeout, debug])].
