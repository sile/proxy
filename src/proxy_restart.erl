%% @doc proxyが停止した時に再起動するためのモジュール.
-module(proxy_restart).

%%----------------------------------------------------------------------------------------------------------------------
%% Behaiviour
%%----------------------------------------------------------------------------------------------------------------------
-behaviour(proxy).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export_type([
              option/0
             ]).

-export([
         init/1, handle_arg/2, handle_up/2, handle_message/2, handle_down/2, terminate/2
        ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types & Macros & Records
%%----------------------------------------------------------------------------------------------------------------------
-type option() :: {max_restart, non_neg_integer()}  % default: 5
                | {max_time, timeout()}             % default: infinity
                | {interval, non_neg_integer()}     % default: 0
                | {max_interval, non_neg_integer()} % default: 60 * 1000
                | {only_error, boolean()}.          % default: false
%% TODO: only_error以外にもっと細かい粒度で再起動ポリシーを制御する方法を提供する

-define(STATE, ?MODULE).

-record(?STATE,
        {
          max_restart                    :: non_neg_integer(),
          max_time                       :: timeout(),
          interval                       :: non_neg_integer(),
          max_interval                   :: non_neg_integer(),
          only_error                     :: boolean(),
          start_timestamps = queue:new() :: queue:queue(erlang:timestamp())
        }).

%%----------------------------------------------------------------------------------------------------------------------
%% 'proxy' Callback API
%%----------------------------------------------------------------------------------------------------------------------
%% @private
-spec init([option()]) -> {ok, #?STATE{}}.
init(Options) ->
    %% TODO: validation
    State =
        #?STATE{
            max_restart = proplists:get_value(max_restart, Options, 5),
            max_time    = proplists:get_value(max_time, Options, infinity),
            max_interval= proplists:get_value(max_interval, Options, 60 * 1000),
            interval    = proplists:get_value(interval, Options, 0),
            only_error  = proplists:get_value(only_error, Options, false)
           },
    {ok, State}.

%% @private
-spec handle_arg([term()], #?STATE{}) -> {ok, [term()], #?STATE{}}.
handle_arg(Args, State) ->
    {ok, Args, State}.

%% @private
-spec handle_up(pid(), #?STATE{}) -> {ok, #?STATE{}}.
handle_up(_Pid, State) ->
    Now = os:timestamp(),
    Timestamps0 = queue:in(Now, State#?STATE.start_timestamps),
    Timestamps1 = delete_old_timestamp(Now, State#?STATE.max_time, Timestamps0),
    {ok, State#?STATE{start_timestamps = Timestamps1}}.

%% @private
-spec handle_message(MessageIn, #?STATE{}) -> {stop, Reason, #?STATE{}} | {ok, MessageOut, #?STATE{}} when
      MessageIn :: {'EXIT', pid(), Reason} | term(),
      Reason :: term(),
      MessageOut :: term().
handle_message({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};
handle_message(Message, State) ->
    {ok, Message, State}.

%% @private
-spec handle_down(term(), #?STATE{}) -> {ok, #?STATE{}} | {restart, Interval::non_neg_integer(), #?STATE{}}.
handle_down(Reason, State) ->
    #?STATE{interval = Interval} = State,
    NextInterval = min(max(1, Interval) * 2, State#?STATE.max_interval),
    {Result, State2} = is_restarting_needed(Reason, State),
    case Result of
        false -> {ok, State2};
        true  -> {restart, Interval, State2#?STATE{interval = NextInterval}}
    end.

%% @private
-spec terminate(term(), #?STATE{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @hidden
-spec is_error_exit(term()) -> boolean().
is_error_exit(normal)        -> false;
is_error_exit(shutdown)      -> false;
is_error_exit({shutdown, _}) -> false;
is_error_exit(_)             -> true.

%% @hidden
-spec is_restarting_needed(term(), #?STATE{}) -> {IsNeeded::boolean(), #?STATE{}}.
is_restarting_needed(Reason, State) ->
    #?STATE{start_timestamps = Timestamps0} = State,
    Now = os:timestamp(),
    Timestamps1 = delete_old_timestamp(Now, State#?STATE.max_time, Timestamps0),
    Count = queue:len(Timestamps1),
    IsNeeded =
        ((not State#?STATE.only_error orelse is_error_exit(Reason)) andalso
         Count < State#?STATE.max_restart),
    {IsNeeded, State#?STATE{start_timestamps = Timestamps1}}.

%% @hidden
-spec delete_old_timestamp(erlang:timestamp(), integer() | infinity, Timestamps) -> Timestamps when
      Timestamps :: queue:queue(erlang:timestamp()).
delete_old_timestamp(_Now, infinity, Timestamps0) ->
    Timestamps0;
delete_old_timestamp(Now, MaxTime, Timestamps0) ->
    case queue:out(Timestamps0) of
        {empty, Timestamps1}         -> Timestamps1;
        {{value, Head}, Timestamps1} ->
            case max(0, timer:now_diff(Now, Head)) div 1000 > MaxTime of
                true  -> delete_old_timestamp(Now, MaxTime, Timestamps1);
                false -> Timestamps0
            end
    end.
