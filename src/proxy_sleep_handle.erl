%% @copyright 2015 Hinagiku Soranoba <soranoba@gmail.com>
%% @doc RealProcessが存在しない場合にgen_serverのメッセージをハンドルする
-module(proxy_sleep_handle).

-behaviour(proxy).

-export_type([
              option/0
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
          is_running = false :: boolean(),
          call               :: undefined | fun((term()) -> {reply, term()} | noreply),
          cast               :: undefined | fun((term()) -> ok),
          info               :: undefined | fun((term()) -> ok)
        }).
-type state()  :: #?STATE{}.

-type option() :: {call, fun((term()) -> {reply, term()} | noreply)} |
                  {cast, fun((term()) -> ok)}                        |
                  {info, fun((term()) -> ok)}.
                  %% RealProcessが存在しない場合に実行するMessage処理

%%----------------------------------------------------------------------------------------------------------------------
%% 'proxy' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
-spec init([option()]) -> {ok, state()}.
init(Options) ->
    Call = proplists:get_value(call, Options, undefined),
    Cast = proplists:get_value(cast, Options, undefined),
    Info = proplists:get_value(info, Options, undefined),

    State = #?STATE{call = Call, cast = Cast, info = Info},
    {ok, State}.

%% @private
-spec handle_arg(Args :: [term()], state()) -> {ok, Args :: [term()], state()}.
handle_arg(Args, State) ->
    {ok, Args, State}.

%% @private
-spec handle_up(RealProcess :: pid(), state()) -> {ok, state()}.
handle_up(_Pid, State) ->
    {ok, State#?STATE{is_running = true}}.

%% @private
-spec handle_message(Message :: term(), state()) -> {ok, MessageOut :: term(), state()} | {stop, Reason :: term(), state()}.
handle_message({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};
handle_message({'$gen_call', {From, Ref}, Request}, #?STATE{is_running = false, call = CallFun} = State) when CallFun =/= undefined ->
    _ = case CallFun(Request) of
            {reply, Response} -> gen_server:reply({From, Ref}, Response);
            noreply           -> ok
        end,
    {ignore, State};
handle_message({'$gen_cast', Request}, #?STATE{is_running = false, cast = CastFun} = State) when CastFun =/= undefined ->
    _ = CastFun(Request),
    {ignore, State};
handle_message(Request, #?STATE{is_running = false, info = InfoFun} = State) when InfoFun =/= undefined ->
    _ = InfoFun(Request),
    {ignore, State};
handle_message(Message, State) ->
    {ok, Message, State}.

%% @private
-spec handle_down(Reason :: term(), state()) -> {ok, state()}.
handle_down(_Reason, State) ->
    {ok, State#?STATE{is_running = false}}.

%% @private
-spec terminate(Reason :: term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.
