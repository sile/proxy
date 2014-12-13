

# Module proxy #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


proxyサーバを扱うためのインターフェースモジュール.
Copyright (c) 2014 Takeru Ohta <phjgt308@gmail.com>


__This module defines the `proxy` behaviour.__<br /> Required callback functions: `init/1`, `handle_arg/2`, `handle_up/2`, `handle_message/2`, `handle_down/2`, `terminate/2`.

<a name="types"></a>

## Data Types ##




### <a name="type-args">args()</a> ###



<pre><code>
args() = [term()]
</code></pre>





### <a name="type-fun_name">fun_name()</a> ###



<pre><code>
fun_name() = atom()
</code></pre>





### <a name="type-priority_level">priority_level()</a> ###



<pre><code>
priority_level() = low | normal | high | max
</code></pre>





### <a name="type-proxy_arg">proxy_arg()</a> ###



<pre><code>
proxy_arg() = term()
</code></pre>





### <a name="type-proxy_spec">proxy_spec()</a> ###



<pre><code>
proxy_spec() = {module(), <a href="#type-proxy_arg">proxy_arg()</a>}
</code></pre>





### <a name="type-proxy_state">proxy_state()</a> ###



<pre><code>
proxy_state() = term()
</code></pre>





### <a name="type-spawn_option">spawn_option()</a> ###



<pre><code>
spawn_option() = link | monitor | {priority, <a href="#type-priority_level">priority_level()</a>} | {fullsweep_after, non_neg_integer()} | {min_heap_size, non_neg_integer()} | {min_bin_vheap_size, non_neg_integer()}
</code></pre>



  spawn_optのoption



### <a name="type-spawn_options">spawn_options()</a> ###



<pre><code>
spawn_options() = [<a href="#type-spawn_option">spawn_option()</a>]
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#call-2">call/2</a></td><td>proxy_server モジュールにメッセージを送る.</td></tr><tr><td valign="top"><a href="#spawn-2">spawn/2</a></td><td>proxy を挟んで process を生成する.</td></tr><tr><td valign="top"><a href="#spawn-4">spawn/4</a></td><td>proxy を挟んで process を生成する.</td></tr><tr><td valign="top"><a href="#spawn_opt-3">spawn_opt/3</a></td><td>proxy を挟み option を指定して process を生成する.</td></tr><tr><td valign="top"><a href="#spawn_opt-5">spawn_opt/5</a></td><td>proxy を挟み option を指定して process を生成する.</td></tr><tr><td valign="top"><a href="#start-4">start/4</a></td><td>proxy を挟んで server を起動する.</td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td>proxy を挟んで server を起動する.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="call-2"></a>

### call/2 ###


<pre><code>
call(ProxyPid::pid(), Msg::'$proxy_call') -&gt; ProxyServerPid::pid() | error
</code></pre>
<br />


proxy_server モジュールにメッセージを送る.


'$proxy_call' メッセージを送ると, proxy_server の Process ID を取得できる.
<a name="spawn-2"></a>

### spawn/2 ###


<pre><code>
spawn(Fun::function(), ProxySpecs::[<a href="#type-proxy_spec">proxy_spec()</a>]) -&gt; pid() | {pid(), reference()}
</code></pre>
<br />

proxy を挟んで process を生成する.
<a name="spawn-4"></a>

### spawn/4 ###


<pre><code>
spawn(Module::module(), Function::<a href="#type-fun_name">fun_name()</a>, Args::<a href="#type-args">args()</a>, ProxySpecs::[<a href="#type-proxy_spec">proxy_spec()</a>]) -&gt; pid() | {pid(), reference()}
</code></pre>
<br />

proxy を挟んで process を生成する.
<a name="spawn_opt-3"></a>

### spawn_opt/3 ###


<pre><code>
spawn_opt(Fun::function(), ProxySpecs::[<a href="#type-proxy_spec">proxy_spec()</a>], SpawnOpts::<a href="#type-spawn_options">spawn_options()</a>) -&gt; pid() | {pid(), reference()}
</code></pre>
<br />

proxy を挟み option を指定して process を生成する.
<a name="spawn_opt-5"></a>

### spawn_opt/5 ###


<pre><code>
spawn_opt(Module::module(), Function::<a href="#type-fun_name">fun_name()</a>, Args::<a href="#type-args">args()</a>, ProxySpecs::[<a href="#type-proxy_spec">proxy_spec()</a>], SpawnOpts::<a href="#type-spawn_options">spawn_options()</a>) -&gt; pid() | {pid(), reference()}
</code></pre>
<br />

proxy を挟み option を指定して process を生成する.
<a name="start-4"></a>

### start/4 ###


<pre><code>
start(Module::module(), Function::<a href="#type-fun_name">fun_name()</a>, Args::<a href="#type-args">args()</a>, ProxySpecs::[<a href="#type-proxy_spec">proxy_spec()</a>]) -&gt; {ok, pid()} | {error, Reason::term()}
</code></pre>
<br />

proxy を挟んで server を起動する.
<a name="start_link-4"></a>

### start_link/4 ###


<pre><code>
start_link(Module::module(), Function::<a href="#type-fun_name">fun_name()</a>, Args::<a href="#type-args">args()</a>, ProxySpecs::[<a href="#type-proxy_spec">proxy_spec()</a>]) -&gt; {ok, pid()} | {error, Reason::term()}
</code></pre>
<br />

proxy を挟んで server を起動する.
