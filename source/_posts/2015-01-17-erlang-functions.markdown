---
layout: post
title: "Erlang Functions"
date: 2015-01-17 20:30
comments: true
categories: [Erlang]
keywords: The Erlang Learning Sequence, Learning Erlang, Learn Erlang, Study Erlang, Studying Erlang, The Studying Erlang, Chicagoboss Framwork, Erlang Functions, Functions
---

<p>
  <img src="/images/logo_erlang.png" alt="Erlang Variables" />
</p>

<p>
  As you know by now, Erlang is a functional programming language. In my point of view, different programming has different problem solving philosophy:<br/>
  - Procedural: describe the steps needed to be taken to solve the problem.<br/>
  - Logical (Declarative): describe the problem properly and let the language solve it.<br/>
  - Object-orientation: design the objects that will lead you to the solution.<br/>
  - Functional: define small and precise functions that all together solve the problem.
</p>

<p>
  <strong>Declaring a Function</strong><br/>
</p>

<p>
  Syntax:
</p>

{% codeblock lang:ruby %}
function_name(Argument1, Argument2, ...) ->
  Statement1,
  Statement2,
  ...
{% endcodeblock %}

<p>
  Example:
</p>

{% codeblock example.erl lang:ruby %}
-module(example).
-export([double/1]).

double(N) -> 2 * N.
{% endcodeblock %}
