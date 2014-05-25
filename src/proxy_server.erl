-module(proxy_server).

%% -export([
%%          start_loop/2,
%%          loop/2
%%         ]).

%% -type start_method() :: function().
%% -type proxy() :: {module(), proxy_state()}.
%% -type proxy_state() :: term().

%% -record(state,
%%         {
%%           tag = make_ref() :: reference(),
%%           start_method :: start_method(),
%%           real_pid     :: pid() | undefined,
%%           proxy_list   :: [proxy()]
%%         }).

%% -spec start_loop(function(), [proxy_spec()]) -> no_return().
%% start_loop(Fun, ProxySpecs) ->
%%     ProxyList = init_proxy_list(ProxySpecs),
%%     RealPid = spawn(Fun),
%%     State = #state{
%%                start_method = Fun,
%%                real_pid     = RealPid,
%%                proxy_list   = ProxyList
%%               },
%%     _ = process_flag(trap_exit, true),
%%     ?MODULE:loop(State).

%% -spec init_proxy_list([proxy_spec()]) -> [proxy()].
%% init_proxy_list([])   -> [];
%% init_proxy_list(Specs) ->
%%     [{Module, Arg} | Rest] = Specs,
%%     case Module:init(Arg) of
%%         {error, Reason}  -> error({function_failure, {Module, init, [Arg]}, {error, Reason}}, [Specs]);
%%         ignore           -> init_proxy_list(Rest);
%%         {ok, ProxyState} -> [{Module, ProxyState} | init_proxy_list(Rest)];
%%         Other            -> error({unexpected_return, {Module, init, [Arg]}, Other}, [Specs])
%%     end.

%% -spec loop(#state{}) -> no_return().
%% loop(State = #state{tag = Tag, real_pid = RealPid}) ->
%%     receive
%%         {'EXIT', RealPid, Reason} ->
%%             {Result, State2} = drive_handle_down(Reason, State),
%%             case Result of
%%                 {restart, After} ->
%%                     _ = erlang:send_after(After, self(), {Tag, restart}),
%%                     loop(State2);
%%                 {exit, ProxyExitReason} ->
%%                     exit(ProxyExitReason)
%%             end;
%%         {Tag, restart} ->
%%             State2 = restart_real_process(State),
%%             loop(State2);
%%         Message ->
%%             State2 = drive_handle_message(Message, State),
%%             loop(State2)
%%     end.
            
%% -spec drive_handle_down(term(), #state{}) -> {Result, #state{}} when
%%       Result :: {restart, non_neg_integer()}
%%               | {exit, term()}.
%% drive_handle_down(Reason, State) ->
    
      
