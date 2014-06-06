%% @doc 実プロセスの生存寿命管理を行うためのプロキシ
%%
%% TODO: 開始時間および終了時間は途中で変更可能にする
-module(proxy_lifetime).

-behaviour(proxy).

-export_type([
              option/0,
              time_spec/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'proxy' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         init/1, handle_arg/2, handle_up/2, handle_message/2, handle_down/2, terminate/2
        ]).

-define(STATE, ?MODULE).

-record(?STATE,
        {
          tag = make_ref() :: reference(),
          start_time :: time_spec(),
          stop_time  :: time_spec() | infinity,
          start_ref  :: reference() | undefined,
          stop_ref   :: reference() | undefined
        }).

-type option() :: {start_time, time_spec()}             % default: now()
                | {stop_time, time_spec() | infinity}.  % default: infinity

-type time_spec() :: erlang:timestamp().

%%----------------------------------------------------------------------------------------------------------------------
%% 'proxy' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init(Options) ->
    %% TODO: validation
    State =
        #?STATE{
            start_time = proplists:get_value(start_time, Options, os:timestamp()),
            stop_time  = proplists:get_value(stop_time, Options, infinity)
           },
    {ok, State}.

%% @private
handle_arg(Args, State) ->
    Delta = timer:now_diff(State#?STATE.start_time, now()),
    case Delta =< 0 of
        true  ->
            StopAfter = max(0, timer:now_diff(State#?STATE.stop_time, os:timestamp()) div 1000),
            TimerRef = erlang:send_after(StopAfter, self(), {?MODULE, State#?STATE.tag, stop}),
            _ = logi:info("stop after ~p ms", [StopAfter]),
            {ok, Args, State#?STATE{stop_ref = TimerRef}};
        false ->
            %% XXX: Delta は erlang:send_afterの制限時間以内だと仮定している
            TimerRef = erlang:send_after(Delta div 1000, self(), {?MODULE, State#?STATE.tag, start}),
            _ = logi:info("start after ~p ms", [Delta div 1000]),
            {hibernate, Args, State#?STATE{start_ref = TimerRef}}
    end.

%% @private
handle_up(_Pid, State) ->
    {ok, State}.

%% @private
handle_message({?MODULE, Tag, start}, State = #?STATE{tag = Tag}) ->
    _ = self() ! {'__SYSTEM__', 'RESUME'}, % 起動を依頼する
    case State#?STATE.stop_time of
        infinity -> {ignore, State#?STATE{stop_ref = undefined}};
        _        ->
            StopAfter = max(0, timer:now_diff(State#?STATE.stop_time, os:timestamp()) div 1000),
            TimerRef = erlang:send_after(StopAfter, self(), {?MODULE, State#?STATE.tag, stop}),
            _ = logi:info("stop after ~p ms", [StopAfter]),
            {ignore, State#?STATE{start_ref = undefined, stop_ref = TimerRef}}
    end;
handle_message({?MODULE, Tag, stop}, State = #?STATE{tag = Tag}) ->
    {stop, {shutdown, timeout}, State#?STATE{stop_ref = undefined}};
handle_message(Message, State) ->
    {ok, Message, State}.

%% @private
handle_down(_Reason, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.