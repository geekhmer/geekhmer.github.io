---
layout: post
title: "Erlang Concurrency"
date: 2015-01-26 18:46
comments: true
categories: [Erlang]
keywords: The Erlang Learning Sequence, Learning Erlang, Learn Erlang, Study Erlang, Studying Erlang, The Studying Erlang, Chicagoboss Framwork, Erlang Modules and Compiling, Modules and Compiling, Erlang Recursion, Recursion, Erlang Tail Recursion, Tail Recursion, Erlang Control Flow Statement, Erlang Control Flow, Flow Statement, Erlang List ane List Module, Erlang List, Erlang List Module, Erlang List Comprehension, List Comprehension, Erlang Concurrency, Concurrency
---

<p>
  <img src="/images/logo_erlang.png" alt="Erlang Concurrency" />
</p>

<p>
  Concurrency is defined as “the temporal property of two things happening at the same time”. In Computer Science, the definition is “concurrency is a property of systems in which several computations are executing simultaneously, and potentially interacting with each other. The computations may be executing on multiple cores in the same chip, preemptively time-shared threads on the same processor, or executed on physically separated processors.”
</p>

<p>
  Erlang was built with concurrency and fault-tolerance in mind.
</p>

<p>
  <h3>Processes</h3>
</p>

<p>
  The granularity of concurrency in Erlang is a process. A process is an activity/task that runs concurrently with and independently from the other processes (though processes can interact with each other using messages, links, etc.). Processes in Erlang are different than the processes and threads most people are familiar with. Erlang processes are lightweight, operate in (memory) isolation from other processes, and are scheduled by the Erlang’s Virtual Machine (VM). The creation time of process is very low, the memory footprint of a just spawned process is very small, and a single Erlang VM can have millions of processes running.
</p>

<p>
  <h3>Messaging Passing</h3>
</p>

<p>
  The communication model (among processes) in Erlang is message passing. Erlang processes share no memory. The way that processes communicate is via message passing (asynchronous). Every process (even the shell) holds a mailbox queue where incoming messages are placed until received by the process. Message passing is asynchronous because the sending process does not block on send. Sending a message in Erlang is a non-blocking operation that always succeed (more in the next post).
</p>

<p>
  <h3>Why Message Passing?</h3>
</p>

<p>
  We are so used to the shared memory model, why changing it? Here are some characteristics that are part of Erlang mostly because of the message passing memory model.
</p>

<p>
  <strong>Crash Independency</strong><br/>
  Message passing allows for easier distributed programming. Imagine if you want to distribute an application that uses shared memory. To do this, one should either use a message passing solution (such as MPI) or a Distributed Shared Memory system (DSM), that also uses message passing to operate. Why not using message passing in the first place? Especially in Erlang, message passing allows for location transparency (when sending a message there is no difference to the programmer if the receiver resides in the local or a remote node).
</p>

<p>
  <h3>Creating Processes</h3>
</p>

<p>
  Erlang provides Built-In Functions that are used to spawn new processes. The simplest one is spawn/1|3 (the 1|3 denotes that both spawn/1 and spawn/3 functions exist).
</p>

<p>
  <strong>Pid Datatype</strong><br/>
  Pid stands for Process identifier and is the datatype used for the unique process, identifiers that are assigned to every process.
</p>

<p>
  <strong>spawn/1|3</strong><br/>
  Creates a new process and returns its pid. The new process is placed in the system scheduler queue, so it will be run some time later.
</p>

<p>
  <strong>spawn/1</strong><br/>
  Called as spawn(Fun). The new process will run function Fun with an empty list ([]) as input.
</p>

<p>
  <strong>spawn/3</strong><br/>
  Called as spawn(Module, Function, Args). The new process will run function Module:Function with the elements of the list Args as input.
</p>

<p>
  Example:
</p>

{% codeblock process.erl lang:ruby %}
-module(process).
-export([start/0, say_something/2]).

say_something(_What, 0) -> done;
say_something(What, Times) -> 
  io:format("~p~n", [What]),
  say_something(What, Times - 1).

start() ->
  spawn(process, say_something, ["hello", 3]),
  spawn(process, say_something, ["bonjour", 2]).
{% endcodeblock %}

{% codeblock process1.erl lang:ruby %}
-module(process1).
-export([start/0, ping/2, pong/0]).

ping(0, Pong_Pid) ->
  Pong_Pid ! finished,
  io:format("ping finished~n", []);
ping(N, Pong_Pid) ->
  Pong_Pid ! {ping, self()},
  receive
    pong ->
      io:format("ping received pong~n", [])
  end,
  ping(N - 1, Pong_Pid).

pong() ->
  receive
    finished ->
      io:format("pong finished~n", []);
    {ping, Ping_Pid} ->
      io:format("pong received ping~n", []),
      Ping_Pid ! pong,
      pong()
  end.

start() ->
  Pong_Pid = spawn(message, pong, []),
  spawn(message, ping, [3, Pong_Pid]).
{% endcodeblock %}

<p>
  <h3>Creating Linked Processes</h3>
</p>

<p>
  A very useful feature is to create a new process that is linked to the “parent” one. The link between them guarantees that if one of the two fails (finishes abnormally), the other one also stops executing. This feature is very helpful because it reduces the need for “cleaning up” in case of a failure. Instead of explicitely handling an error, the “let it fail and let someone else handle it” philosophy can be used. The BIF(s) providing this functionality are the spawn_link/1|3.
</p>

<p>
  <strong>link/1</strong><br/>
  Creates a bidirectional link between the calling process and another process (or port), if there is not such a link already. If a process attempts to create a link to itself, nothing is done. Returns true.
</p>

<p>
  <strong>spawn_link/1|3</strong><br/>
  Provides the same functionality as spawn/1|3 with the addition that a link is atomically created between the caller and the newly spawned process.
</p>

<p>
  <strong>spawn_link/1</strong><br/>
  Same call convention as spawn/1.
</p>

<p>
  <strong>spawn_link/3</strong><br/>
  Same call convention as spawn/3.
</p>

<p>
  Example:
</p>

{% codeblock lang:ruby %}
spawnLink() ->
  spawn(fun spawnLink_/0).
 
spawnLink_() ->
  spawn_link(?MODULE, sayExit, []),
  justLoop().
 
justLoop() ->
  justLoop().
 
sayExit() ->
  timer:sleep(4000),
  erlang:kill(not_catched).
 
1> c(spawning).
{ok,spawning}
2> Pid = spawning:spawnLink().  
<0.42.0>
3> erlang:is_process_alive(Pid).
true
4> erlang:is_process_alive(Pid).
true
5> erlang:is_process_alive(Pid).
=ERROR REPORT==== 7-May-2011::12:24:54 ===
Error in process <0.43.0> with exit value: {undef,[{erlang,kill,[not_
false
6> erlang:is_process_alive(Pid).
false
{% endcodeblock %}

<p>
  <h3>Other Processes’-related Built-In Functions</h3>
</p>

<p>
  There are several other BIFs related to processes. The following are some commonly used.
</p>

<p>
  <strong>is_pid/1</strong><br/>
  Returns true if the argument is a pid, else false.
</p>

{% codeblock lang:ruby %}
1> Pid = spawn(io, format, ["Hello~n"]).
Hello
<0.37.0>
2> is_pid(Pid).
true
{% endcodeblock %}

<p>
  <strong>is_process_alive/1</strong><br/>
  Called as is_process_alive(Pid). Pid must refer to a process at the local node. Returns true if the process exists and is alive, that is not exiting and has not exited. Otherwise, returns false.
</p>

{% codeblock lang:ruby %}
1> is_process_alive(self()).
true
{% endcodeblock %}

<p>
  <strong>list_to_pid/1</strong><br/>
  Transforms the input string to a pid. This BIF is intended to be used for debugging and not in the application development.
</p>

{% codeblock lang:ruby %}
1> Pid == list_to_pid("<0.39.0>").
true
{% endcodeblock %}

<p>
  <strong>pid_to_list/1</strong><br/>
  Returns the textual representation of a pid. This BIF is intended to be used for debugging only.
</p>

{% codeblock lang:ruby %}
1> StringPid = pid_to_list(Pid).
"<0.37.0>"
{% endcodeblock %}

<p>
  <strong>register/2</strong><br/>
  Registers a process (or a port) with a name. This name can be later used to refer to the process.
</p>

{% codeblock lang:ruby %}
1> ShellPid = self().
<0.99.0>
2> register(shell, ShellPid).
true
{% endcodeblock %}

<p>
  <strong>registered/0</strong><br/>
  Returns a list with the names of all registered processes.
</p>

{% codeblock lang:ruby %}
1> registered().
[init,shell,error_logger,rex,kernel_sup,inet_db,
 global_name_server,file_server_2,code_server,
 erl_prim_loader,user_drv,standard_error_sup,
 application_controller,standard_error,kernel_safe_sup,user,
 global_group]
{% endcodeblock %}

<p>
  <strong>self/0</strong><br/>
  One of the most commonly used BIF, returns the pid of the calling processes. As you will see in the next post (about messaging), self is used in almost every message send.
</p>

{% codeblock lang:ruby %}
1> ShellPid = self().
<0.44.0>
{% endcodeblock %}

<p>
  <strong>erlang:send/2|3</strong><br/>
  Sends a message to a process. You will see message sending in detail in the next post.
</p>

<p>
  <strong>erlang:send_after/3</strong><br/>
  Sends a message after a given amount of time.
</p>

<p>
  <strong>unlink/1</strong><br/>
  Removes the link between two processes. Returns true even if there is no exist link.
</p>

<p>
  <strong>unregister/1</strong><br/>
  Called as unregister(Name). Removes the association between the Name and the process which it is associated with.
</p>

{% codeblock lang:ruby %}
1> unregister(shell).
true
2> registered().
[init,error_logger,rex,kernel_sup,inet_db,
 global_name_server,file_server_2,code_server,
 erl_prim_loader,user_drv,standard_error_sup,
 application_controller,standard_error,kernel_safe_sup,user,
 global_group]
{% endcodeblock %}

<p>
  <strong>whereis/1</strong><br/>
  Called as whereis(Name). Returns the pid of the process that is register with the name Name.
</p>

{% codeblock lang:ruby %}
1> whereis(shell).
<0.44.0>
{% endcodeblock %}
