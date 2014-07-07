%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc プロキシモジュールのインターフェース定義モジュール
-module(proxy_module).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export_type([
              id/0,
              name/0,
              spec/0,
              arg/0,
              state/0,
              stop_reason/0,
              terminate_reason/0,
              real_process_args/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type id()                :: module() | {module(), name()}.
-type name()              :: term().
-type spec()              :: {id(), arg()}.
-type arg()               :: term().
-type state()             :: term().
-type stop_reason()       :: term().
-type terminate_reason()  :: {shutdown, ignore} | stop_reason().
-type real_process_args() :: [term()].

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback init(arg(), proxy_context:context()) ->
    {ok, state(), proxy_context:context()} |
    {stop, stop_reason(), proxy_context:context()} |
    {ignore, proxy_context:context()}.

-callback handle_args(real_process_args(), state(), proxy_context:context()) ->
    {ok, real_process_args(), state(), proxy_context:context()} |
    {stop, stop_reason(), proxy_context:context()}.

-callback handle_up(state(), proxy_context:context()) ->
    {ok, state(), proxy_context:context()} |
    {stop, stop_reason(), state(), proxy_context:context()}.

-callback handle_message(proxy:message(), state(), proxy_context:context()) ->
    {ok, [proxy:message()], state(), proxy_context:context()} |
    {stop, stop_reason(), state(), proxy_context:context()}.

-callback handle_request(proxy:request(), proxy:from(), state(), proxy_context:context()) ->
    {ok, state(), proxy_context:context()} |
    {stop, stop_reason(), state(), proxy_context:context()}.

-callback handle_down(stop_reason(), state(), proxy_context:context()) ->
    {ok, stop_reason(), state(), proxy_context:context()} |
    {stop, stop_reason(), state(), proxy_context:context()}.

-callback terminate(terminate_reason(), state(), proxy_context:context()) ->
    proxy_context:context().
