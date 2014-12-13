

# Module proxy_restart #
* [Description](#description)
* [Data Types](#types)


proxyが停止した時に再起動するためのモジュール.
__Behaviours:__ [`proxy`](proxy.md).

<a name="types"></a>

## Data Types ##




### <a name="type-max_restart">max_restart()</a> ###



<pre><code>
max_restart() = non_neg_integer() | infinity
</code></pre>





### <a name="type-option">option()</a> ###



<pre><code>
option() = {max_restart, <a href="#type-max_restart">max_restart()</a>} | {max_time, timeout()} | {interval, non_neg_integer()} | {max_interval, non_neg_integer()} | {only_error, boolean()} | {restart_stop_fun, <a href="#type-restart_stop_fun">restart_stop_fun()</a>}
</code></pre>





### <a name="type-restart_stop_fun">restart_stop_fun()</a> ###



<pre><code>
restart_stop_fun() = fun((term()) -&gt; boolean())
</code></pre>



 Stop restarting if this function returns false
