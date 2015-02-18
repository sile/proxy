%% @doc ユーティリティモジュール
%% @private
-module(proxy_lib).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([where/1, name_register/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec where(proxy:proxy_name()) -> pid() | undefined.
where({global, Name})      -> global:whereis_name(Name);
where({via, Module, Name}) -> Module:whereis_name(Name);
where({local, Name})       -> whereis(Name).

-spec name_register(proxy:proxy_name()) -> true | {false, pid()}.
name_register({local, Name} = LN) ->
    try register(Name, self()) of
        true -> true
    catch
        error:_ -> {false, where(LN)}
    end;
name_register({global, Name} = GN) ->
    case global:register_name(Name, self()) of
        yes -> true;
        no -> {false, where(GN)}
    end;
name_register({via, Module, Name} = GN) ->
    case Module:register_name(Name, self()) of
        yes -> true;
        no -> {false, where(GN)}
    end.
