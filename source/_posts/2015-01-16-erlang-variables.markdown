---
layout: post
title: "Erlang Variables"
date: 2015-01-16 21:46
comments: true
categories: [Erlang]
keywords: The Erlang Learning Sequence, Learning Erlang, Learn Erlang, Study Erlang, Studying Erlang, The Studying Erlang, Chicagoboss Framwork, Erlang Variables, Variables
---

<p>
  <img src="/images/logo_erlang.png" alt="Erlang Variables" />
</p>

<p>
  <strong>Dynamic Datatyping</strong><br/>
  Erlang is a dynamic datatyping programming language. That means that when "declaring" a variable you do not need to statically specify the datatypes. For example, this is how we declare and initialize an integer in Erlang:
</p>

{% codeblock lang:ruby %}
I = 17.
{% endcodeblock %}

<p>
  This approach has both advantages and disadvantages. Advantages: when programming, it is fast and convenient as we don't need to declare the variables datatypes. Disadvantages: In big projects it can lead to code readability problems unless well documented.
</p>

<p>
  <strong>Variables Declaration</strong><br/>
  Erlang is influenced by <a href="http://en.wikipedia.org/wiki/Prolog">Prolog</a>. As with Prolog variables is a string consisting of letters, numbers and underscore characters, and beginning with an upper-case letter or underscore.<br/>
</p>

<p>
  Example:
</p>

{% codeblock lang:ruby %}
X
Name1
PhoneNumber
Phone_number
_
_Height
[H|_] = [1, , 2, 3]
{% endcodeblock %}

<p>
  <strong>Variable Assignement</strong><br/>
  Another feature that Erlang inherited from Prolog is binding with pattern matching. In a nutshell, a value is not assigned to a variable but bound with pattern matching. The most important thing is that variables in Erlang are single assignement, it mean that once bound to a value, their value cannot change for their lifetime.<br/>
</p>

<p>
  Example (open terminator and try the following):
</p>

{% codeblock lang:ruby %}
1> Age = 10.
10
2> Age = 11.
{% endcodeblock %}

<p>
  We will get an error:
</p>

<p>
  <code>** exception error: no match of right hand side value 11</code>
</p>

<p>
  The problem is that A is bound to the value 10, so Erlang tries to pattern match 10 with the value 11 which is impossible.
</p>
