%% @doc プロキシプロセスと任意のプロセスをアトミックにリンクするためのプロキシモジュール
%%
-module(proxy_link).

-behaviour(proxy).

-export_type([
              arg/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'proxy' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_arg/2, handle_up/2, handle_message/2, handle_down/2, terminate/2]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(STATE, ?MODULE).

-record(?STATE,
        {
          linked_pids = [] :: [pid()]
        }).

-type arg() :: [pid()].

%%----------------------------------------------------------------------------------------------------------------------
%% 'proxy' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
-spec init(arg()) -> {ok, #?STATE{}}.
init(Pids) ->
    %% TODO: validation
    _ = lists:foreach(fun erlang:link/1, Pids),
    State =
        #?STATE{
            linked_pids = Pids
           },
    {ok, State}.

%% @private
-spec handle_arg(Args::[term()], #?STATE{}) -> {ok, Args::[term()], #?STATE{}}.
handle_arg(Args, State) ->
    {ok, Args, State}.

%% @private
-spec handle_up(pid(), #?STATE{}) -> {ok, #?STATE{}}.
handle_up(_Pid, State) ->
    {ok, State}.

%% @private
-spec handle_message(term(), #?STATE{}) -> {ok, term(), #?STATE{}}.
handle_message(Message, State) ->
    {ok, Message, State}.

%% @private
-spec handle_down(term(), #?STATE{}) -> {ok, #?STATE{}}.
handle_down(_Reason, State) ->
    {ok, State}.

%% @private
-spec terminate(term(), #?STATE{}) -> ok.
terminate(_Reason, _State) ->
    ok.
