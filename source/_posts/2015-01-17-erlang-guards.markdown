---
layout: post
title: "Erlang Guards"
date: 2015-01-17 22:22
comments: true
categories: [Erlang]
keywords: The Erlang Learning Sequence, Learning Erlang, Learn Erlang, Study Erlang, Studying Erlang, The Studying Erlang, Chicagoboss Framwork, Erlang Guards, Guards
---

<p>
  <img src="/images/logo_erlang.png" alt="Erlang Variables" />
</p>

<h3>Guard structures</h3>

<p>
  Guards in Erlang are boolean functions placed after the key word, "when" and before the arrow, "->". Guards may appear as part of a function definition, 'receive', 'if', 'case' and 'try/catch' expressions.
</p>

<p>
  We use a guard in a function definition.
</p>

<p>
  Example:
</p>

{% codeblock example.erl lang:ruby %}
-module(example).
-compile(export_all).
   
the_answer_is(N) when N =:= 42 -> true;
the_answer_is(N) -> false.

% c(example).
% ok
%
% example:the_answer_is(42).
% true
%
% example:the_answer_is(21).
% false
{% endcodeblock %}

<p>
  <strong>fun definition</strong>
</p>

{% codeblock lang:ruby %}
F = fun
  (N) when N =:= 42 -> true;
  (N) -> false
end.
{% endcodeblock %}

<p>
  <strong>receive expression</strong>
</p>

{% codeblock lang:ruby %}
receive
  {answer, N} when N =:= 42 -> true;
  {answer, N} -> false
end.
{% endcodeblock %}

<p>
  <strong>if expression</strong>
</p>

{% codeblock lang:ruby %}
if 
  N =:= 42 -> true;
  true -> false
end.
{% endcodeblock %}

<p>
  <strong>case expression</strong>
</p>

{% codeblock lang:ruby %}
case L of
  {answer, N} when N =:= 42 -> true;
  _ -> false
end.
{% endcodeblock %}

{% codeblock lang:ruby %}
case L of
  {node, N} when N =:= 42 -> true;
  _AnyNode -> false
end.
{% endcodeblock %}

<p>
  <strong>try/catch</strong>
</p>

{% codeblock lang:ruby %}
try find(L) of
  {answer, N} when N =:= 42 -> true;
  _ -> false
catch
  {notanumber, R} when is_list(R) -> alist;
  {notanumber, R} when is_float(R) -> afloat
  _ -> noidea
end.
{% endcodeblock %}

<p>
  <h3>Multiple Guards</h3>
  It is possible to use multiple guards within the same function definition or expression. When using multiple guards, a semicolon, ";", signifies a boolean "OR", while a comma, ",", signifies boolean "AND".
</p>

<p>
  Example:
</p>

{% codeblock lang:ruby %}
the_answer_is(N) when N == 42, is_integer(N) -> true;
geq_1_or_leq_2(N) when N >= 1; N =< 2 -> true;
{% endcodeblock %}

<p>
  <h3>Guard Functions</h3>
  There are several built-in-functions which be used in a guard.
</p>

{% codeblock lang:ruby %}
is_alive/0
is_boolean/1
is_builtin/3
is_constant/1
is_float/1
is_function/2
is_function/1
is_integer/1
is_list/1
is_number/1
is_pid/1
is_port/1                      
is_record/3
is_record/2
is_reference/1
is_tuple/1

tuple_size/1
is_binary/1
is_bitstring/1
bit_size/1
byte_size/1
length(Z) > N
{% endcodeblock %}

{% codeblock lang:ruby %}
A > B
A < B
A == B
A =< B
A >= B
A /= B
A =:= B
A =/= B
{% endcodeblock %}
