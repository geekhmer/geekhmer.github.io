---
layout: post
title: "Erlang Message Passing"
date: 2015-01-27 23:07
comments: true
categories: [Erlang]
keywords: The Erlang Learning Sequence, Learning Erlang, Learn Erlang, Study Erlang, Studying Erlang, The Studying Erlang, Chicagoboss Framwork, Erlang Modules and Compiling, Modules and Compiling, Erlang Recursion, Recursion, Erlang Tail Recursion, Tail Recursion, Erlang Control Flow Statement, Erlang Control Flow, Flow Statement, Erlang List ane List Module, Erlang List, Erlang List Module, Erlang List Comprehension, List Comprehension, Erlang Concurrency, Concurrency, Erlang Message Passing, Message Passing
---

<p>
  <img src="/images/logo_erlang.png" alt="Erlang Message Passing" />
</p>

<p>
  The communication model (among processes) in Erlang is message passing. Erlang processes share no memory. The way that processes communicate is via (asynchronous) message passing. Every process (even the shell) holds a mailbox queue where incoming messages are placed until received by the process. Message passing is asynchronous because the sending process does not block on send. On the other hand, receiving a message in Erlang is a blocking operation.
</p>

<p>
  <h3>Characteristics</h3>
</p>

<p>
  In this subsection I will describe some of the characteristics of message passing in Erlang.
</p>

<p>
  <strong>Asynchronous</strong><br/>
  Message passing in Erlang is a non-blocking operation.
</p>

<p>
  <strong>Data Copy</strong><br/>
  The message’s data are copied from the sending process to the receiver’s message queue, so the receiver gets a fresh copy of the data.
</p>

<p>
  <strong>Ordering</strong><br/>
  Erlang runtime guarantees that if two messages are sent from node A to node B and both are delivered, then the ordering of these messages is kept (because ordering).
</p>

<p>
  <strong>Successful Send</strong><br/>
  The send operation always succeeds (even if the target is a non-existing process) and evaluates to the data sent. An exception is when trying to send data to a non-existing registered process.
</p>

<p>
  <h3>Sending Messages</h3>
</p>

<p>
  Erlang uses the exclamation mark (!) as the operator for sending a message.
</p>

<p>
  Syntax:
</p>

{% codeblock lang:ruby %}
%send message Message to the process with pid Pid
Pid ! Message
{% endcodeblock %}

<p>
  Send multiple messages "at once":
</p>

{% codeblock lang:ruby %}
Pid1 ! Message, Pid2 ! Message, Pid3 ! Message
Pid1 ! (Pid2 ! (Pid3 ! Message))
Pid1 ! Pid2 ! Pid3 ! Message
{% endcodeblock %}

<p>
  Example:
</p>

<p>
  As I have mentioned before, the shell is nothing more than a process. As a process, it has a message queue. In order to print and empty the shell’s message queue we can use the flush/0 BIFs.
</p>

{% codeblock lang:ruby %}
1> self() ! erlang_term_can_be_sent.
erlang_term_can_be_sent
2> flush().                             
Shell got erlang_term_can_be_sent
ok
3> self() ! [this, is, a, list, 'of', atoms].
[this,is,a,list,'of',atoms]
4> self() ! {this, [is, a], tuple, {'!', '!'}}.
{this,[is,a],tuple,{'!','!'}}
5> self() ! {self(), 123}.
{<0.35.0>,123}
6> flush().
Shell got [this,is,a,list,'of',atoms]
Shell got {this,[is,a],tuple,{'!','!'}}
Shell got {<0.35.0>,123}
ok
7> Pid1 = self(), Pid2 = self(), Pid3 = self().
<0.35.0>
8> Pid1 ! msg, Pid2 ! msg, Pid3 ! msg.
msg
9> flush().
Shell got msg
Shell got msg
Shell got msg
ok
10> Pid1 ! (Pid2 ! (Pid3 ! msg)).      
msg
11> flush().                     
Shell got msg
Shell got msg
Shell got msg
ok
12> Pid1 ! Pid2 ! Pid3 ! msg.    
msg
13> flush().                 
Shell got msg
Shell got msg
Shell got msg
ok
{% endcodeblock %}

<p>
  <h3>Receiving Messages</h3>
</p>

<p>
  Erlang uses pattern matching for receiving messages (same as in function clause selection and the case statement). The receive statement is used to deliver messages from the message queue.
</p>

<p>
  Syntax:
</p>

{% codeblock lang:ruby %}
receive
  Pattern1 when Guard1 ->
    ToDo1;
  Pattern2 when Guard2 ->
    ToDo2;
  _Other ->
    Catch_all
end
{% endcodeblock %}

<p>
  <strong>Receiving Order</strong><br/>
  The message processing is done in a FIFS (First In – First Served) order. Every incoming message is placed in the tail of the process’ message queue. When a receive statement is meet the following processing happens:<br/>
  1. The first message (head of the message queue) is pattern matched against the first receive clause. If match, execute the clause’s body, else go to the next step.<br/>
  2. The same message is pattern matched against the second (if any) receive clause. If match, execute the clause’s body, else go to the next step.<br/>
  3. ...
  4. The same message is pattern matched against the last clause. If match, execute the clause’s body, else go to the next step.
  5. The same iterative process starts again from step 1, but now with the next message from the message queue.<br/><br/>
  The message (if any) that is delivered through receive is removed from the message queue.
</p>

<p>
  Example:
</p>

{% codeblock pingpong.erl lang:ruby %}
-module(pingpong).
-export([play/1]).
 
play(N) when is_integer(N), N > 0 ->
  Pong = spawn(fun pong/0),
  ping(N, Pong).
 
ping(0,Pong) ->
  Pong ! exit,
  ok;
ping(N, Pong) ->
  Pong ! {self(), ping},
  receive
    pong ->
      io:format("~w : pong [~w]~n", [self(), N])
  end,
  ping(N - 1, Pong).
 
pong() ->
  receive
    {From, ping} ->
      io:format("~w : ping~n", [self()]),
      From ! pong,
      pong();
    exit ->
      ok
  end.
{% endcodeblock %}

<p>
  Example running:
</p>

{% codeblock lang:ruby %}
1> c(pingpong).
{ok,pingpong}
2> pingpong:play(4).
<0.49.0> : ping
<0.39.0> : pong [4]
<0.49.0> : ping
<0.39.0> : pong [3]
<0.49.0> : ping
<0.39.0> : pong [2]
<0.49.0> : ping
<0.39.0> : pong [1]
ok
{% endcodeblock %}

<p>
  <strong>Timeout</strong><br/>
  Receive is a blocking statement; it blocks until a message that matches one of the clauses is placed in the incoming message queue. Erlang allows the programmer to explicitly unblock the receive statement using a timeout (if a matching message is not delivered until the timeout expires). The complete format of receive statement, including the after construct.
</p>

<p>
  Syntax:
</p>

{% codeblock lang:ruby %}
receive
  Pattern1 when Guard1 ->
    ToDo1;
  Pattern2 when Guard2 ->
    ToDo2;
  _Other ->
    Catch_all
after Millisecs ->
  ToDo_timeout;
end
{% endcodeblock %}

<p>
  Example:
</p>

{% codeblock timeout.erl lang:ruby %}
-module(timeout).
-export([start/0, sleep/1]).
 
start() ->
  spawn(fun timeout/0).
 
timeout() ->
  receive
    cancel ->
      io:format("Timeout canceled~n")
  after 2000 -> % 2 seconds
    io:format("Timeout triggered~n"),
    timeout()
  end.
 
%a sleep function
sleep(Ms) ->    
  io:format("Sleeping for ~w ms~n", [Ms]),
  receive
  after Ms ->
    done
  end
{% endcodeblock %}

<p>
  Example running:
</p>

{% codeblock lang:ruby %}
1> c(timeout).
{ok,timeout}
2> P = timeout:start().
<0.42.0>
Timeout triggered
Timeout triggered
3> P ! cancel.
Timeout canceled
cancel
4> timeout:sleep(1000).
Sleeping for 1000 ms
done
5> timeout:sleep(3000).
Sleeping for 3000 ms
done
{% endcodeblock %}
