---
layout: post
title: "Erlang List &amp; List Module"
date: 2015-01-22 23:01
comments: true
categories: [Erlang]
keywords: The Erlang Learning Sequence, Learning Erlang, Learn Erlang, Study Erlang, Studying Erlang, The Studying Erlang, Chicagoboss Framwork, Erlang Modules and Compiling, Modules and Compiling, Erlang Recursion, Recursion, Erlang Tail Recursion, Tail Recursion, Erlang Control Flow Statement, Erlang Control Flow, Flow Statement, Erlang List ane List Module, Erlang List, Erlang List Module
---

<p>
  <img src="/images/logo_erlang.png" alt="Erlang Control Flow Statement" />
</p>

<p>
  List is the the most important data type in Erlang, as in every functional programming language. In this article, I will present the Erlang’s lists module and its most important functions.
</p>

<p>
  <h3>Syntax</h3>
</p>

{% codeblock lang:ruby %}
[Element1, Element2, ..., ElementN]
{% endcodeblock %}

<p>
  N is called the length of the list. So, [] is the empty list.
</p>

<p>
  <h3>Decomposing & Pattern Matching</h3>
</p>

<p>
  An empty list pattern matches with [].
</p>

{% codeblock lang:ruby %}
1> Empty = [].
[]
2> Empty == [].
true
3> Empty = []. 
[]
4> NotEmpty = [1].
[1]
5> Empty == NotEmpty.
false
6> Empty = NotEmpty.
** exception error: no match of right hand side value [1]
{% endcodeblock %}

<p>
  A non-empty list pattern matches with [Head | Tail].
</p>

{% codeblock lang:ruby %}
1> [Head | Tail] = [1, 2, 3, 4]. 
[1,2,3,4]
2> Head. 
1
3> Tail.
[2,3,4]
4> [Head1 | Tail1] = Tail.
[2,3,4]
5> Head1.
2
6> Tail1.
[3,4]
7> [Head2 | Tail2] = [].
** exception error: no match of right hand side value []
{% endcodeblock %}

<p>
  <strong>Normal Representation</strong><br/>
  The format [Element1, Element2, ..., ElementN] is a shorthand of [Element1 | [Element2 | ... | [ElementN | []] ... ] representation.<br/>
  Example: the list [1, 2, 3] is a shorthand of [1 | [2 | 3 | []]], that is the normal representation of a list.
</p>

<p>
  Decomposing:
</p>

{% codeblock lang:ruby %}
1> [A | [B |[C | D]]] = [1, 2, 3].
[1,2,3]
2> A.
1
3> B. 
2
4> C.
3
5> D.
[]
{% endcodeblock %}

<p>
  Composing:
</p>

{% codeblock lang:ruby %}
1> List = [2,3].
[2,3]
2> List1 = [1 | List]. 
[1,2,3]
3> List2 = [-1 | [0 | List1]]. 
[-1,0,1,2,3]
4> List3 = [[-3, -2] | List2].
[[-3,-2],-1,0,1,2,3] % the head is just 1 element
{% endcodeblock %}

<p>
  Of course, since it is more readable and easier to write, the shorthand representation is usually used: 
</p>

{% codeblock lang:ruby %}
1> [A, B, C] = [1, 2, 3].
[1,2,3]
2> [A, B, C, D] = [1, 2, 3]. % does not match cause
% the left-hand side matches a 4-elements list
** exception error: no match of right hand side value [1,2,3]
3> [A, B, C | D] = [1, 2, 3].
[1,2,3]
4> D.
[]
5> [-3, -2 | [-1, 0 | [1 | [2,3]]]].  
[-3,-2,-1,0,1,2,3]
{% endcodeblock %}

<p>
  <strong>List Parsing</strong><br/>
  The pattern matching you saw before can be used in a function in order to parse the list:
</p>

{% codeblock lang:ruby %}
parse([]) ->
  parsed.
parse([Head | Tail]) ->
  parse(Tail).
{% endcodeblock %}

<p>
  <strong>Concatenation</strong><br/>
  Two lists can be concatenated using ++:
</p>

{% codeblock lang:ruby %}
1> L1 = [1, 2], L2 = [3, 4, 5].
[3,4,5]
2> L1 ++ L2.
[1,2,3,4,5]
3> L1 ++ L2 ++ L1.
[1,2,3,4,5,1,2]
4> Mirror = fun(List) -> List ++ lists:reverse(List) end.
#Fun<erl_eval .6.13229925>
5> Mirror([a, b, {c}]).
[a,b,{c},{c},b,a]
{% endcodeblock %}

<p>
  <strong>Difference</strong><br/>
  You can take the difference of two lists (the left-hand side one without the element of the right-hand side) using the -- operator:
</p>

{% codeblock lang:ruby %}
1> [1, 2, 3] -- [1, 2, 3].
[]
2> [1, 2, 3] -- [1, 3].   
[2]
3> [1, 2, 3] -- [1, 3, 3].
[2]
4> [1, 3, 2, 3] -- [1, 3, 3].
[2]
5> [3, 1, 3, 2, 3] -- [1, 3, 3].
[2,3]
6> [1, 2, 3] -- [a, b].         
[1,2,3]
7> Delete = fun(List, Element) -> List -- [Element] end.
#Fun<erl_eval .12.113037538>
8> Delete([1,2,3,4,1], 1).
[2,3,4,1]
{% endcodeblock %}

<p>
  <h3>Module Lists</h3>
</p>

<p>
  The lists module defines some commonly used list processing functions. This module is extremely useful, so it is a good idea to “remember” what functions it provides.
</p>

<p>
  <strong>all/2</strong><br/>
  Called as all(Pred, List). Returns true if Pred(Element) returns true for all lists’ elements.
</p>

{% codeblock lang:ruby %}
1> L = [2, 4, 6, 8],
1> F = fun(X) -> X rem 2 == 0 end,
1> lists:all(F, L).
true
2> f().
ok
3> L = [2, 4, 5, 8],
3> F = fun(X) -> X rem 2 == 0 end,
3> lists:all(F, L).
false
{% endcodeblock %}

<p>
  <strong>append/1|2</strong><br/>
  Concatenates the lists to one.
</p>

{% codeblock lang:ruby %}
1> lists:append([[1, 2], [3], [4, 5]]).
[1,2,3,4,5]
2> lists:append([1, 2], [3, 4]).
[1,2,3,4]
{% endcodeblock %}

<p>
  Notice that the operator ++ and the function append/2 are the same.
</p>

<p>
  <strong>delete/2</strong><br/>
  Deletes an element from the list (first occurrence, if any).
</p>

{% codeblock lang:ruby %}
1> lists:delete(a, [d, a, d, a, d]).
[d,d,a,d]
{% endcodeblock %}

<p>
  <strong>concat/1</strong><br/>
  Accepts a list of items (atom, integer, float, string) and returns the concatenation of their textual representation as a list.
</p>

{% codeblock lang:ruby %}
1> lists:concat(["ab", '.', 1]).     
"ab.1"
{% endcodeblock %}

<p>
  <strong>filter/2</strong><br/>
  Called as filter(Pred, List). Returns a list containing only the elements that return true for the Pred.
</p>

{% codeblock lang:ruby %}
1> Gt10 = fun(X) -> X > 10 end, lists:filter(Gt10, [1, 2, 22, 3, 44, 5, 66]).
[22,44,66]
2> L = [1, a, b, 2, c, 3.0, d, {4}]. 
[1,a,b,2,c,3.0,d,{4}]
3> lists:filter(fun(X) -> is_number(X) end, L).
[1,2,3.0]
{% endcodeblock %}

<p>
  <strong>flatten/1</strong><br/>
  Returns a flattened (no element is a list) version of the input list.
</p>

{% codeblock lang:ruby %}
1> lists:flatten([1, [2], [3, 4, [5, [6, 7]]], [[[[8]]]]]).
[1,2,3,4,5,6,7,8]
{% endcodeblock %}

<p>
  <strong>key*** functions</strong><br/>
  There are several functions which their name starts with the word "key". They are all used to process lists of tuples.
</p>

{% codeblock lang:ruby %}
1> Kl = [{a, k1, a}, {b, k2, b}, {c, k3, c}, {e, k5, e}].
[{a,k1,a},{b,k2,b},{c,k3,c},{e,k5,e}]
2> lists:keydelete(k3, 2, Kl).
[{a,k1,a},{b,k2,b},{e,k5,e}]
3> lists:keysearch(k3, 2, Kl).
{value,{c,k3,c}}
4> lists:keysearch(k4, 2, Kl).
false
5> lists:keyreplace(k3, 2, Kl, {new, tuple}).
[{a,k1,a},{b,k2,b},{new,tuple},{e,k5,e}]
{% endcodeblock %}

<p>
  <strong>last/1</strong><br/>
  Returns the last element of the list.
</p>

{% codeblock lang:ruby %}
1> lists:last([1, 2, 3]).
3
{% endcodeblock %}

<p>
  <strong>map/2</strong><br/>
  Called as map(Fun, List). Applies function Fun to every item of the list and returns the resulting list.
</p>

{% codeblock lang:ruby %}
1> lists:map(fun(I) -> 2 * I end, [1, 2, 3]).
[2,4,6]
{% endcodeblock %}

<p>
  <strong>partition/2</strong><br/>
  Partitions a list to two according to if the elements satisfy or not a given predicate.
</p>

{% codeblock lang:ruby %}
1> lists:map(fun(I) -> 2 * I end, [1, 2, 3]).
[2,4,6]
{% endcodeblock %}

<p>
  <strong>reverse/1|2</strong><br/>
  Returns the reverse of a list.
</p>

{% codeblock lang:ruby %}
1> lists:reverse([a, b, c, d]).
[d,c,b,a]
2> lists:reverse([a, b, c, d], [1, 2, 3]).
[d,c,b,a,1,2,3]
{% endcodeblock %}

<p>
  <strong>sort/1|2</strong><br/>
  Sorts a list to increasing order or according to a given function.
</p>

{% codeblock lang:ruby %}
1> L = [3, 1, 2, 5, 4].
[3,1,2,5,4]
2> lists:sort(L).
[1,2,3,4,5]
3> Gt = fun(I, J) -> I > J end.
#Fun<erl_eval .12.113037538>
4> lists:sort(Gt, L).
[5,4,3,2,1]
{% endcodeblock %}

<p>
  <strong>sum/1</strong><br/>
  Returns the sum of the elements of a list containing numbers.
</p>

{% codeblock lang:ruby %}
1> lists:sum([1, 2.2, 3]).
6.2
{% endcodeblock %}

<p>
  <strong>u*** functions</strong><br/>
  There are several function which their name starts with "u" and the results they return contain no duplicates.
</p>

{% codeblock lang:ruby %}
1> lists:usort([5, 3, 2, 3, 2, 1, 4, 5]).
[1,2,3,4,5]
{% endcodeblock %}
