---
layout: post
title: "Erlang Control Flow Statement"
date: 2015-01-20 23:17
categories: [Erlang]
keywords: The Erlang Learning Sequence, Learning Erlang, Learn Erlang, Study Erlang, Studying Erlang, The Studying Erlang, Chicagoboss Framwork, Erlang Modules and Compiling, Modules and Compiling, Erlang Recursion, Recursion, Erlang Tail Recursion, Tail Recursion, Erlang Control Flow Statement, Erlang Control Flow, Flow Statement
---

<p>
  <img src="/images/logo_erlang.png" alt="Erlang Control Flow Statement" />
</p>

<p>
  As we saw in the previous post, pattern matching with different function clauses can be used in order to control the execution flow in Erlang. Erlang also provides the if, case, and receive control flow constructs that can be used in a function body. In this post I will only present the if and casestatements since receive is used for message passing and I will write a dedicated post about the subject. Both if and case are similar to the statements of other programming languages.
</p>

<p>
  <strong>If Statement</strong>
</p>

<p>
  Syntax:
</p>

{% codeblock lang:ruby %}
if
  BoolbeanExpression1 ->
    IfBody1;
  BooleanExpression2 ->
    IfBody2;
    ...
  true ->
    BodyCathAll
end
{% endcodeblock %}

<p>
  The different clauses, except the last one are like "else if" in other languages, while the last one (true ->) is like the "else"; it succeeds when all the previous clauses have failed.
</p>

<p>
  Example:
</p>

{% codeblock lang:ruby %}
is_greater_than(X, Y) ->
  if
    X > Y ->
      true;
    true ->
      false
  end
{% endcodeblock %}

<p>
  <strong>Case Statement</strong>
</p>

<p>
  Syntax:
</p>

{% codeblock lang:ruby %}
case Expression of
  Value1 [when Guard1] ->
    CaseBody1;
  Value2 [when Guard2]->
    CaseBody2;
  _Other ->
    CaseBodyCatchAll
end
{% endcodeblock %}

<p>
  Notice that the last clause (_Other) is like the default clause in other programming languages. The Expression should always return a value (if it is a function call) that will be used to perform the pattern matching.
</p>

<p>
  Example:
</p>

{% codeblock lang:ruby %}
check(List) when is_list(List) ->
  case lists:reverse(List) of
    List ->
      true;
    _ ->
      false
    end;
check(_) ->
    {error, arg_not_list}.
{% endcodeblock %}

