

# Module proxy_lifetime #
* [Description](#description)
* [Data Types](#types)


実プロセスの生存寿命管理を行うためのプロキシ.
__Behaviours:__ [`proxy`](proxy.md).

<a name="types"></a>

## Data Types ##




### <a name="type-option">option()</a> ###



<pre><code>
option() = {start_time, <a href="#type-time_spec">time_spec()</a>} | {stop_time, <a href="#type-time_spec">time_spec()</a> | infinity}
</code></pre>



 default: infinity



### <a name="type-time_spec">time_spec()</a> ###



<pre><code>
time_spec() = <a href="erlang.md#type-timestamp">erlang:timestamp()</a>
</code></pre>


