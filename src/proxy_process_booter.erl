%% @copyright 2014 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc プロセス起動用モジュールのインタフェース定義
-module(proxy_process_booter).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         make/3,
         start/1,
         get_mfargs/1,
         set_mfargs/2
        ]).

-export_type([
              booter/0,
              state/0,
              error_reason/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Types & Records
%%----------------------------------------------------------------------------------------------------------------------
-define(BOOTER, ?MODULE).

-record(?BOOTER,
        {
          module :: module(),
          state  :: state(),
          mfargs :: proxy:mfargs()
        }).

-opaque booter()     :: #?BOOTER{}.
-type state()        :: term().
-type error_reason() :: term().

%%----------------------------------------------------------------------------------------------------------------------
%% Behaviour Callbacks
%%----------------------------------------------------------------------------------------------------------------------
-callback start(proxy:mfargs(), state()) -> {ok, pid()} | {error, error_reason()}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec make(module(), proxy:mfargs(), state()) -> booter().
make(Module, MFArgs, State) ->
    %% TODO: validate
    #?BOOTER{module = Module, state = State, mfargs = MFArgs}.

-spec start(booter()) -> {ok, pid()} | {error, error_reason()}.
start(#?BOOTER{module = Module, state = State, mfargs = MFArgs}) -> Module:start(MFArgs, State).

-spec get_mfargs(booter()) -> proxy:mfargs().
get_mfargs(#?BOOTER{mfargs = MFArgs}) -> MFArgs.

-spec set_mfargs(proxy:mfargs(), booter()) -> booter().
set_mfargs(MFArgs, Booter) -> Booter#?BOOTER{mfargs = MFArgs}.
