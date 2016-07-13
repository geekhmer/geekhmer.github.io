---
layout: post
title: "Erlang Tail Recursion"
date: 2015-01-18 21:33
comments: true
categories: [Erlang]
keywords: The Erlang Learning Sequence, Learning Erlang, Learn Erlang, Study Erlang, Studying Erlang, The Studying Erlang, Chicagoboss Framwork, Erlang Modules and Compiling, Modules and Compiling, Erlang Recursion, Recursion, Erlang Tail Recursion, Tail Recursion
---

<p>
  <img src="/images/logo_erlang.png" alt="Erlang Variables" />
</p>

<p>
  A function is called tail recursive when the recursive call to itself happens only in the last expression of the body in every clause.
</p>

<p>
  Syntax:
</p>

{% codeblock lang:ruby %}
non_tail_recursive(...) ->
  non_tail_recursive(...),
  other_expression,
  ... .
tail_recursive(...) ->
  other_expression,
  ...
  tail_recursive(...).
{% endcodeblock %}

<p>
  The first function is not the tail recursive as the recursive call is followed by other expression. While the second is, since the recursive call is the last statement.
</p>

<p>
  <strong>Tail Recursion & Performance</strong><br/>
  In many programming languages tails recursion is a good approach performance. But in general, it is not the case in the latest releases of Erlang. Tail recursion is not guaranteed to give you better performance.
</p>

<p>
  <strong>Tail Recursion VS Non Tail Recursion</strong><br/>
</p>

<p>
  Non Tail Recursive:
</p>

{% codeblock lang:ruby %}
length([]) -> 0;
length([_ | T]) -> 1 + length(T).
{% endcodeblock %}

<p>
  Tail Recursive:
</p>

{% codeblock lang:ruby %}
length(List) -> length(List, 0).
 
length([], L) -> L;
length([_ | T], L) -> length(T, L + 1).
{% endcodeblock %}
