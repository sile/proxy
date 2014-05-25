-module(proxy_restart).

-behaviour(proxy).

-export_type([
              option/0
             ]).

-export([
         init/1, handle_arg/2, handle_up/2, handle_message/2, handle_down/2, terminate/2
        ]).

-type option() :: {max_restart, non_neg_integer()}  % default: 5
                | {max_time, timeout()}             % default: infinity
                | {interval, non_neg_integer()}     % default: 0
                | {only_error, boolean()}.          % default: false

-define(STATE, ?MODULE).

-record(?STATE,
        {
          max_restart           :: non_neg_integer(),
          max_time              :: timeout(),
          interval              :: non_neg_integer(),
          only_error            :: boolean(),
          start_timestamps = queue:new() :: queue:queue(erlang:timestamp())
        }).
        
init(Options) ->
    %% TODO: validation
    State =
        #?STATE{
            max_restart = proplists:get_value(max_restart, Options, 5),
            max_time    = proplists:get_value(max_time, Options, infinity),
            interval    = proplists:get_value(interval, Options, 0),
            only_error  = proplists:get_value(only_error, Options, false)
           },
    {ok, State}.

handle_arg(Args, State) ->
    {ok, Args, State}.

handle_up(_Pid, State) ->
    Now = os:timestamp(),
    Timestamps0 = queue:in(Now, State#?STATE.start_timestamps),
    Timestamps1 = delete_old_timestamp(Now, State#?STATE.max_time, Timestamps0),
    {ok, State#?STATE{start_timestamps = Timestamps1}}.

handle_message({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};
handle_message(Message, State) ->
    {ok, Message, State}.

handle_down(Reason, State) ->
    {Result, State2} = is_restarting_needed(Reason, State),
    case Result of
        false -> {ok, State2};
        true  -> {restart, State2#?STATE.interval, State2}
    end.

terminate(_Reason, _State) ->
    ok.

is_error_exit(normal)        -> false;
is_error_exit(shutdown)      -> false;
is_error_exit({shutdown, _}) -> false;
is_error_exit(_)             -> true.

is_restarting_needed(Reason, State) ->
    #?STATE{start_timestamps = Timestamps0} = State,
    Now = os:timestamp(),
    Timestamps1 = delete_old_timestamp(Now, State#?STATE.max_time, Timestamps0),
    Count = queue:len(Timestamps1),
    IsNeeded = 
        ((not State#?STATE.only_error orelse is_error_exit(Reason)) andalso
         Count < State#?STATE.max_restart),
    {IsNeeded, State#?STATE{start_timestamps = Timestamps1}}.

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
