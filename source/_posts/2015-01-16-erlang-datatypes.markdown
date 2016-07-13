---
layout: post
title: "Erlang Datatypes"
date: 2015-01-16 23:37
comments: true
categories: [Erlang]
keywords: The Erlang Learning Sequence, Learning Erlang, Learn Erlang, Study Erlang, Studying Erlang, The Studying Erlang, Chicagoboss Framwork, Erlang Datatypes, Datatypes
---

<p>
  <img src="/images/logo_erlang.png" alt="Erlang Variables" />
</p>

<p>
  I will introduce the most datatypes that being used in Erlang such as number, atom, function, tuple, map, list, record, and boolean.
</p>

<p>
  <strong>Number</strong><br/>
  There are two datatypes of numeric are integers and floats.
</p>

<p>
  Example:
</p>

{% codeblock lang:ruby %}
1> 42.
42
2> 2.3.
2.3
{% endcodeblock %}

<p>
  <strong>Atom</strong><br/>
  Atom is a literal, a constant with name. Atom should be enclosed in single quotes (') if it does not begin with a lower-case letter or if it contains other characters than alphanumeric characters, underscore (_), or @ sign.
</p>

<p>
  Example:
</p>

{% codeblock lang:ruby %}
hi
phone_number
'Sunday'
'phone number'
{% endcodeblock %}

<p>
  <strong>Fun</strong><br/>
  Fun is a functional object. Funs make it possible to create an anonymous function and pass the function itself.
</p>

<p>
  Example:
</p>

{% codeblock lang:ruby %}
1> Fun1 = fun (X) -> X+1 end.
#Fun<erl_eval.6.39074546>
2> Fun1(6).
7
{% endcodeblock %}

<p>
  <strong>Tuple</strong><br/>
  Tuple is a compound data type, it consists of elements of any datatypes.
</p>

<p>
  Syntax:
</p>

{% codeblock lang:ruby %}
{Element1, Element2, ..., ElementN}
{% endcodeblock %}

<p>
  Example:
</p>
{% codeblock lang:ruby %}
> T = {bunlong, 27, {may, 17}}.
{bunlong, 27, {may, 17}}
> element(1, T).
bunlong
> T2 = setelement(2, T, 25).
{bunlong, 25, {may, 17}}
> tuple_size(T).
3
> tuple_size({}).
0
{% endcodeblock %}

<p>
  <string>Map</string><br/>
  Map is a compound data type with a variable number of key-value associations.
</p>

<p>
  Syntax:
</p>

{% codeblock lang:ruby %}
#{Key1=>Value1,...,KeyN=>ValueN}
{% endcodeblock %}

<p>
  Example:
</p>

{% codeblock lang:ruby %}
1> M1 = #{name=>bunlong,age=>26,date=>{may,07}}.
#{age => 26,date => {may,07},name => bunlong}
2> maps:get(name,M1).
bunlong
3> maps:get(date,M1).
{may,07}
4> M2 = maps:update(age,27,M1).
#{age => 27,date => {may,07},name => bunlong}
5> map_size(M).
3
6> map_size(#{}).
0
{% endcodeblock %}

<p>
  <strong>List</strong><br/>
  As in all functional programming language, list is one of the most used datatyped. Again, Erlang borrows the list syntax from Prolog. Because of their importance. List is a compound data type with a variable number of terms.
</p>

<p>
  Syntax:
</p>

{% codeblock lang:ruby %}
[Element1, Element2, ..., ElementN]
{% endcodeblock %}

<p>
  Example:
</p>

{% codeblock lang:ruby %}
> L = [a, 2, {c, 4}].
[a, 2, {c, 4}]
> [H|T] = L.
[a, 2, {c, 4}]
> H.
a
> T.
[2, {c, 4}]
> L2 = [d|T].
[d, 2, {c, 4}]
> length(L).
3
> length([]).
0
{% endcodeblock %}

<p>
  <strong>Record</strong><br/>
  A record is a data structure for storing a fixed number of elements. It has named fields and is similar to a struct in C. However, record is not a true data type.
</p>

<p>
  Example:
</p>

{% codeblock lang:ruby %}
-module(person).
-export([new/2]).
-record(person, {name, age}).
new(Name, Age) ->
  #person{name=Name, age=Age}.

% > person:new(bunlong, 27).
% {person, bunlong, 27}
{% endcodeblock %}

<p>
  <strong>Boolean</strong><br/>
  There is no Boolean data type in Erlang. Instead the atoms true and false are used to denote Boolean values.
</p>

<p>
  Example:
</p>

{% codeblock lang:ruby %}
> 2 =< 3.
true
> true or false.
true
{% endcodeblock %}

<p>
  So are so good, there are some other datatypes, such as binary, reference, Pid, etc. I will explain them when needed. see you in the next articles! :)
</p>
