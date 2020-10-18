proxy
=====

[![Build Status](https://travis-ci.org/sile/proxy.svg?branch=master)](https://travis-ci.org/sile/proxy)
[![Code Coverage](https://codecov.io/gh/sile/proxy/branch/master/graph/badge.svg)](https://codecov.io/gh/sile/proxy/branch/master)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

`proxy` は，Erlangのクライアントプロセスとサーバプロセスの間に中継役のプロセスを挟むための仕組みを提供するライブラリである．この中継役のプロセスを以後単に **中継プロセス** と呼ぶことにする．中継プロセスでは “状態を保持する，小さい `gen_server` コールバックのようなモジュール” がいくつか数珠つなぎになって1個のプロセスで稼働する．これらのモジュールを **中継モジュール** と呼ぶことにする．また，これと対比して中継される先にあるサーバプロセスを **本体プロセス** と呼ぶことにする．

```
  ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
  ┃                Proxy Process               ┃
  ┃ ┌──────────┐ ┌──────────┐     ┌──────────┐ ┃  ┏━━━━━━━━━━━━━━┓
──╂─┤  Proxy   ├─┤  Proxy   ├─ … ─┤  Proxy   ├─╂──┨ Real Process ┃
  ┃ │ Module 1 │ │ Module 2 │     │ Module N │ ┃  ┗━━━━━━━━━━━━━━┛
  ┃ └──────────┘ └──────────┘     └──────────┘ ┃
  ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
```

中継モジュールは **`proxy` ビヘイビア** を実装するモジュールとして中継プロセス起動時に与えられる．中継プロセス起動時の指定方法については，詳しくは以下の `proxy:proxy_spec()` 型と `proxy:start_link/4` を参照されたい．

中継プロセス（そのPIDを `ProxyPid` と書く）は，`ProxyPid ! Msg` でメッセージが飛んできたときに `Msg` を中継モジュールで順次前処理をしてから最終的に本体プロセスへその（適宜加工された）メッセージを中継する機能をもつ．つまり，“外界” には `ProxyPid` があたかも本体プロセスのPIDであるかのように見せることができる．

本体プロセスは自身と “外界” との間を仲介している中継プロセスの存在を必ずしも意識しなくてよいが，もし本体プロセスが自身のPIDを他のプロセスに伝えるような処理を行なっている場合は，そのメッセージが送られる相手は本体プロセスのPIDを直接知ってしまうので，送信相手は中継プロセスによる “遮蔽” が機能しないことに注意．


## `proxy` の主要なAPI

### `proxy:proxy_spec()`

```erlang
-type proxy_spec() :: {module(), term()}.
```

`{Module, Arg} :: proxy_spec()` に於いて `Module` は `proxy` ビヘイビアのコールバックモジュールの名前であることが想定される．また `Arg :: α` の形式は `Module` ごとに異なり，コールバック函数 `Module:init/1` に渡される初期設定用の引数である．

この `proxy_spec()` 型の値は，中継モジュールとその起動方法の指定として `proxy:start_link/4` の第4引数などに使われる．


### `proxy:proxy_name()`

```erlang
-type proxy_name() ::
    {local,  Name :: atom()}
  | {global, Name :: term()}
  | {via,    module(), Name :: term()}.
```

`proxy:start_link/5` 参照．


### `proxy:spawn_options()`

```erlang
-type spawn_options() :: [spawn_option()].

-type spawn_option() ::
    link
  | monitor
  | {priority,           priority_level()}
  | {fullsweep_after,    non_neg_integer()}
  | {min_heap_size,      non_neg_integer()}
  | {min_bin_vheap_size, non_neg_integer()}.

-type priority_level() :: low | normal | high | max.
```

`proxy:spawn_opt` などで使われるオプション指定．

* `link`
  - 呼び出したプロセスと中継プロセスの間にリンクを張る指定．
* `monitor`
  - 呼び出したプロセスから中継プロセスへのモニタを張ることを意図した指定だが，現在の実装ではこの指定は無視している．


### `proxy:start_link/4`

```erlang
start_link(
    M          :: module(),
    F          :: atom(),
    Args       :: [term()],
    ProxySpecs :: [proxy:proxy_spec()]
) ->
    {ok, ProxyPid :: pid()}
  | {error, Reason :: term()}.
```

中継プロセスを挟んでプロセスを起動する．`M`，`F`，`Args` は `apply(M, F, Args)` が新たにプロセスを生成してリンクを張り，その生成したプロセスのPIDを `{ok, RealPid :: pid()}` の形で返すか `{error, _}` で失敗を返す函数呼び出しになるようなものでなければならない．この `apply(M, F, Args)` によって生成されるプロセスが本体プロセスである．

また，`proxy:start_link/4` は本体プロセスとの間に実際にリンクを張れたかどうかをチェックする．リンクを張らずに本体プロセスを生成する函数を `M`，`F`，`Args` に与えてしまった場合は中継プロセスで内部的に失敗することに注意．

戻り値が `{ok, ProxyPid :: pid()}` の場合の `ProxyPid` は中継プロセスのPIDであり，本体プロセスのPIDではない（呼び出し側はこの `ProxyPid` を本体プロセスのPIDだと思い込んで使えるようになっている）．一応中継プロセスのPIDから本体プロセスのPIDを得る方法も後述する `proxy:call/2` というバックドアとして提供されている．

典型的には，本体プロセスとなる `gen_server` コールバックモジュールの `start_link` の実装で以下のように使う：

```erlang
start_link(…) ->
    ArgForInit = …,
      %% gen_server:start_link の第2引数となり，最終的に ?MODULE:init/1 に渡される．

    GenServerOptions = …,
      %% gen_server:start_link の第3引数に与えるオプション．

    ProxySpecs = …,
      %% 中継モジュール等の設定．これが生成する中継プロセスの機能の指定となる．

    proxy:start_link(gen_server, start_link, [?MODULE, ArgsForInit, GenServerOptions], ProxySpecs)`．
```

この `?MODULE:start_link` をプロセス `P_0` （例えば何らかのスーパーバイザ）が呼び出すと，まず中継プロセス `P_{proxy}` が中継モジュールの指定 `ProxySpecs :: [proxy:proxy_spec()]` に基づいて生成され，そこから `gen_server:start_link(?MODULE, ArgsForInit, GenServerOptions)` が呼び出されてこのモジュール `?MODULE` による本体プロセス `P_1` が生成される．

重要なのは `ProxySpecs :: [proxy:proxy_spec()]` による中継プロセスの挙動の指定で，ここには

```erlang
[ {Module1, Arg1},
  {Module2, Arg2},
  …
  {ModuleN, ArgN}
]
```

という具合に中継モジュール `ModuleI` とその初期設定 `ModuleI:init(ArgI)` のための引数 `ArgI` の組を羅列する．順番には意味があり，リストで前のものほど中継モジュールの数珠つなぎの前側にくる．


### `proxy:start_link/5`

```erlang
start_link(
    Name       :: proxy:proxy_name(),
    M          :: module(),
    F          :: atom(),
    Args       :: [term()],
    ProxySpecs :: [proxy:proxy_spec()]
) -> {ok, ProxyPid :: pid()}
   | {error, Reason}
when
    Reason :: {already_started, pid()} | term().
```

`start_link/4` の中継プロセスに名前をつける変種．


### `proxy:spawn_opt/2`

```erlang
proxy:spawn_opt(
    K          :: fun(),
    ProxySpecs :: [proxy:proxy_spec()],
    SpawnOpts  :: [proxy:spawn_option()]
) ->
    pid()
  | {pid(), reference()}.
```

処理 `K` （通常の `erlang:spawn_link/1` に引数として与えるような無引数函数）を実行する本体プロセスを，`ProxySpecs` で指定した機能をもつ中継プロセスを仲介させて生成する．


### `proxy:call/2`

```erlang
proxy:call(ProxyPid :: pid(), get_real_process) ->
    RealPid :: pid()
  | error.
```

`proxy:call/2` は（本体プロセスではなく）中継プロセスそのものが有する情報を引き出すための汎用の同期的メッセージ送受信機構を意図して用意しているが，現在のところ `get_real_process` というメッセージしか使うことを想定していない．

`get_real_process` は中継プロセスにそれが中継している先の本体プロセスのPIDを尋ねるためのメッセージ．50ミリ秒経っても中継プロセスから応答がない場合は `error` が返る．これを使うと中継プロセスによる “遮蔽” を超えてしまうことに注意されたい．


## `proxy` ビヘイビア

`proxy` ビヘイビアは中継モジュールのためのビヘイビアで，以下に挙げるコールバック函数を要請する．


### `Module:init/1`

```erlang
Module:init(Arg :: α) ->
    {ok,   State :: β}
  | ignore
  | {stop, Reason :: term()}.
```

中継モジュール `Module` の初期状態の設定を行なう．

`proxy:start_link/4` などが呼び出されたときに第4引数 `ProxySpecs` の要素 `{Module, Arg}` で与えた `Arg` が渡ってくるので，それに基づいて以下のいずれかを返す．

* `{ok, State}`
  - 中継モジュールとして `Module` を使い，初期状態を `State` とする．
* `ignore`
  - 中継モジュールとして `Module` は使わない．他の中継モジュールの起動/非起動には影響しない．
* `{stop, Reason}`
  - 中継プロセスの起動自体を中止する．
  - 正確には，既に起動した中継プロセスを初期状態設定の段階で即座に終了させる．既に初期状態の設定が済んだ手前側の各中継モジュール `M` に対しては終了処理 `M:terminate/2` が呼び出される．


### `Module:handle_arg/2`

```erlang
Module:handle_arg(
    Args  :: [term()],
    State :: β
) ->
    {ok,           UpdatedArgs :: [term()], UpdatedState :: β}
  | {hibernate,    UpdatedArgs :: [term()], UpdatedState :: β}
  | {remove_proxy, UpdatedArgs :: [term()], UpdatedState :: β}
  | {swap_proxy,   UpdatedArgs :: [term()], Reason :: term(), UpdatedState :: β, NewModule :: module(), NewArg :: term()}
  | {stop,         Reason :: term(), UpdatedState :: β}.
```

本体プロセスの起動に使う `{M, F, Args} :: mfargs()` の `Args` を前処理するために与える機構．

典型的には，特に何も加工しない次のような実装を与えればよい：

```erlang
handle_arg(Args, State) ->
    {ok, Args, State}.
```


### `Module:handle_up/2`

```erlang
Module:handle_up(
    RealPid :: pid(),
    State   :: β
) ->
    {ok,           UpdatedState :: β}
  | {remove_proxy, UpdatedState :: β}
  | {swap_proxy,   Reason :: term(), UpdatedState :: β, NewModule :: module(), NewArg :: term()}
  | {stop,         Reason :: term(), UpdatedState :: β}.
```

本体プロセスが生成されてそのPIDが返ってきた直後に各中継モジュールで呼び出される．


### `Module:handle_message/2`

```erlang
Module:handle_message(
    Msg   :: term(),
    State :: β
) ->
    {ok,     UpdatedMsg :: term(), UpdatedState :: β}
  | {ignore, UpdatedState :: β}
  | {stop,   Reason :: term(), UpdatedState :: β}
  | …
```

本体プロセスに中継する前の，`ProxyPid ! Msg` で送られてきたメッセージ `Msg` を前処理加工する．メッセージに対する対応は中継モジュール自体の状態を更新するだけで済む場合もあり，その場合は本体プロセスには中継しない．

* `{ok, UpdatedMsg, UpdatedState}`
  - 自身の状態を `UpdatedState` に更新し，後続の中継モジュールに `UpdatedMsg` を渡す（最後の中継モジュールの場合は，その `UpdatedMsg` が本体プロセスに中継される）．
* `{ignore, UpdatedState}`
  - 自身の状態を `UpdatedState` に更新するが，メッセージはそこで途絶えさせ，後続の中継モジュールや本体プロセスには渡さない．
* `{stop, Reason, UpdatedState}`
  - 自身の状態を `UpdatedState` にしてから，中継プロセスおよび本体プロセスを終了させる．
  - 各中継モジュール `M` の `M:terminate/2` が呼び出されてから，中継プロセスが自身を `exit(Reason)` で終了させる．本体プロセスは中継プロセスとリンクしているのでそれに伴って終了する（またはシステムプロセスの場合 `{'EXIT', _, _}` メッセージを受け取る）．

このほか，最後の中継モジュールが返したメッセージが `{'EXIT', _, Reason}` だった場合，中継プロセスは（そのメッセージを中継せず）まず本体プロセス（そのPIDを `RealPid` とする）を `exit(RealPid, Reason)` で終了させる．ここで本体プロセスが本当に終了した場合，中継プロセス自身もこれとリンクしているので何らかの `{'EXIT', RealPid, _}` メッセージが本体プロセスの終了に伴って送られてきて，各中継モジュール `M` に対して `M:handle_down/2` が呼び出される．この結果に基づいて中継プロセスは本体プロセスを再起動するか，または自身ごと終了するかを選ぶ．詳細は `Module:handle_down/2` 参照．


### `Module:handle_down/2`

```erlang
Module:handle_down(
    Reason :: term(),
    State  :: β
) ->
  | {ok,      UpdatedState :: β}
  | {restart, After :: non_neg_integer(), UpdatedState :: β}
  | {restart, UpdatedState :: β}
  | {stop,    Reason :: term(), UpdatedState :: β}.
```

本体プロセスが終了して中継プロセスにメッセージ `{'EXIT', RealPid, Reason}` が送られてきたときに各中継モジュールに関して順次呼び出され，自身である中継プロセスごと終了するか，それとも本体プロセスを再起動するかを選ぶための函数．

中継モジュールが全員 `ok` を返したか，或いは手前が全員 `ok` を返している上で誰かが `stop` を返した場合は中継プロセスを終了させる，そうでない場合は再起動．戻り値の指定は，より詳しくは以下の通り：

* `{ok, UpdatedState}`
  - 中継プロセスの終了を受け入れる．状態は `UpdatedState` に更新．
  - 後続の中継モジュールが `restart` と言った場合は覆る．
* `{restart, UpdatedState}`
  - `{restart, 0, UpdatedState}` と同じ．
* `{restart, After, UpdatedState}`
  - 自身の状態を `UpdatedState` に更新し，本体プロセスは `After` ミリ秒後に再起動する．
  - 後続の中継モジュールの意見は聞かない．
* `{stop, Reason, UpdatedState}`
  - 中継プロセスを終了させる．
  - `ok` と似るが，こちらは後続の中継モジュールの意見を聞かない．

中継プロセスが終了することが決定した場合の後処理は，次に示す `Module:terminate/2` が担う．


### `Module:terminate/2`

```erlang
Module:terminate(
    Reason :: term(),
    State  :: β
) -> any().
```

中継プロセスが終了する直前に呼び出され，中継モジュール内に関する終了処理を行なう．`Reason` は終了理由．


## `proxy` ライブラリの内部実装

`proxy` ライブラリを使うだけであれば把握する必要はないが，実装を読む際に参考になりうる知見をここに示しておく．

### `proxy.erl`

`proxy` はAPIを担っているモジュールである．

`start` や `start_link` などのAPIは `start_impl` という内部函数に合流しており，この中で “本体プロセス開始用の函数” は `proxy_start_func:make_start_func(M, F, Args)` で “start用のサンク” になる（※ **サンク** (*thunk*) とは実行待ちの函数のこと）．

`spawn` や `spawn_opt` などのAPIは `proxy_start_func:make_spawn_func(K, SpawnOpts -- [monitor])` で “spawn用のサンク” になる．


### `proxy_start_func.erl`

* ```erlang
  -type real_func() :: spawn_func() | start_func().

  -type spawn_func().

  -type start_func().
  ```

  - `real_func()` は本体プロセスを開始するためのサンクの型．
  - `start_func()` の方は “start用のサンク”，すなわち実行するとその内部で新たにプロセスを生成してPIDを返すもの．
  - `spawn_func()` の方は “spawn用のサンク”，すなわり新しいプロセスが生成されてその上で実行することが想定されているもの．

* ```erlang
  proxy_start_func:make_start_func(M, F, Args) -> proxy_start_func:start_func().
  ```

  - 後述の `proxy_start_func:start_link` に使われるサンクをつくる．
  - `proxy:start_impl` の中で呼ばれ，`M`，`F`，`Args` はユーザが `proxy:start_link(M, F, Args, _)` に指定したものがそのまま伝播してくる．

* ```erlang
  proxy_spawn_func:make_spawn_func(K, SpawnOpts) -> proxy_start_func:spawn_func().
  ```

  - 後述の `proxy_start_func:start_link` に使われるサンクをつくる．
  - `proxy:spawn_opt` の中で呼ばれ，`K`，`SpawnOpts` はユーザが `proxy:spawn_opt(K, _, SpawnOpts)` に指定したものがそのまま伝播してくる．

* ```erlang
  proxy_start_func:start_link(
      RealFunc :: proxy_start_func:real_func()
  ) ->
      {ok, pid()}
    | term()
  ```

  - 実際にサンクを起動する．
  - `spawn_func()` の場合は内部的にリンクを張ってプロセスを生成する．
  - `start_func()` の場合は，`start_func()` をつくる際に呼び出し側が指定した `M`，`F`，`Args` をもとに `apply(M, F, Args)` を評価して本体プロセス `RealPid` を生成し，この結果呼び出したプロセスと新たに生成された本体プロセスの間にリンクが張られたか `true = link(RealPid)` で確認する．
    * したがって，リンクを張らないような `M`，`F`，`Args` を指定して `make_start_func(M, F, Args)` で `start_func()` をつくっていたら，それを引数にしてこの `start_link` を呼び出した際に `badmatch` で落ちる．


### `proxy_server.erl`

中継プロセスの “メインループ” を担う．

* ```erlang
  proxy_server:start_loop(
      Name       :: proxy:proxy_name() | undefined,
      From       :: {pid(), term()} | undefined,
      RealFunc   :: proxy_start_func:real_func(),
      ProxySpecs :: [proxy:proxy_spec()]
  ) -> no_return().
  ```

  - `proxy:start_link` や `proxy:spawn_opt` に於いて `spawn_opt(proxy_server, start_loop, …)` で中継プロセスが立ち上げられ，以下のことを行なう：

    1. `Name` でプロセスを登録する（`Name` が `undefined` でない場合）．
       - `proxy_lib:name_register/1` を用いている．
       - 同名のプロセスが既に登録済みの場合はエラーとして `{already_started, pid()}` を返して自身である中継プロセスを終了する．この場合（未初期化なので）各 `Module:terminate` は呼ばれない．
    2. `ProxySpecs` に基づいて各中継モジュールを初期化する．
       - ここで `proxy_driver:init` を介して各中継モジュールの `Module:init` が呼び出される．
    3. 初期化に成功した場合はシステムプロセス化し，`RealFunc` に基づいて本体プロセスを生成する．
       - ここで `proxy_driver:handle_up` を介して各中継モジュールの `Module:handle_up` が呼び出される．
    4. 起動元である `From` に自身である中継プロセスのPIDを送信する（`From` が `undefined` でない場合）．
    5. メインループ `proxy_server:loop/1` に入る．


### `proxy_driver.erl`

中継モジュールの状態を管理する（その意味ではこの `proxy_driver` が `proxy` ライブラリの中核的な実装といえる）．各中継モジュールの `Module:init` や `Module:handle_{arg,up,down,message}` もここでさばく．`proxy_server` からのみ呼ばれる．

各中継モジュールに関する処理は，いずれも `invoke_proxy_list` という内部的な畳み込み函数を用いて行なわれる．
