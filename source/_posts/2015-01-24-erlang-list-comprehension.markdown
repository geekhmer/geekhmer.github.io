---
layout: post
title: "Erlang List Comprehension"
date: 2015-01-24 21:02
categories: [Erlang]
keywords: The Erlang Learning Sequence, Learning Erlang, Learn Erlang, Study Erlang, Studying Erlang, The Studying Erlang, Chicagoboss Framwork, Erlang Modules and Compiling, Modules and Compiling, Erlang Recursion, Recursion, Erlang Tail Recursion, Tail Recursion, Erlang Control Flow Statement, Erlang Control Flow, Flow Statement, Erlang List ane List Module, Erlang List, Erlang List Module, Erlang List Comprehension, List Comprehension
---

<p>
  <img src="/images/logo_erlang.png" alt="Erlang List Comprehension" />
</p>

<p>
  <h3>Syntax:</h3>
</p>

{% codeblock lang:ruby %}
[Expression || Generators1, Guards1, Generators2, ...]
{% endcodeblock %}

<p>
  <strong>Expression</strong><br/>
  The expression specifies the elements of the result. Example: <code>[I || <- [1, 2, 3]]</code> returns the input list element as is.
</p>

<p>
  <strong>Generators</strong><br/>
  Generators create the data used in ther filter-map operations. A generator has the "Pattern <- Data" format, where "Data" is a list or an expression that results to a list and Pattern is a pattern used to match with the elements of the list. This pattern can be used to disassembly elements. Example two valid generators are <code>I <- lists:seq(1, 10)</code> and <code>{X, Y} <- [{'A', 'Excellent'}, {'B', 'Good'}, {'C', 'Fair'}]</code>.
</p>

<p>
  <strong>Guards</strong><br/>
  Guards are expression that return either true or false, the same as the guards we have seen in the previous posts. They apply to the variables that are on the left of the guard and the ones that are accessible to the scope where the comprehension runs. Example: <code>I <- [1, 2, 3, 4], I rem 1 == 0</code> is a valid generator.
</p>

<p>
  <h3>Example:</h3>
</p>

<p>
  I will show some examples that implements some list functions. The most of them already exists in the lists module as I did in the past, I will add these functions to the module called mylists.
</p>

<p>
  <strong>map/2</strong><br/>
  The results contains the elements of the input list after applied to the input function.
</p>

{% codeblock lang:ruby %}
% Generator: the items of the list provided
% Guard: no guard expression, all items are kept
% Expression: the item from the generator after applied to the Function

map(Function, List) ->
  [Function(I) || I <- List].

1> mylists:map(fun(I) -> 2 * I end, [1, 2, 3, 4, 5]).
[2,3,6,8,10]
{% endcodeblock %}

<p>
  <strong>deleteall/2</strong><br/>
  Deletes all occurrences of an element from the list.
</p>

{% codeblock lang:ruby %}
% Generator: the items of the list provided
% Guard: the item should not be equal (both value and type) with the Elem
% Expression: keep the elements of the list that "pass" the guard test, as they are

deleteall(Elem, List) ->
  [I || I <- List, I =/= Elem].

1> mylists:deleteall(3, [1, 2, 3, 4, 3, 2, 1]).
[1,2,4,2,1]
{% endcodeblock %}

<p>
  <strong>partition/2</strong><br/>
  Partition a list into two, according to if the elements satisfy or not a given predicate.
</p>

{% codeblock lang:ruby %}
partition(Pred, List) ->
  {[I || I <- List, Pred(I)], [I || I <- List, not(Pred(I))]}.
 
% an alternative implementation
partition2(Pred, List) ->
    Sat = filter(Pred, List),
    {Sat, List -- Sat}.
 
1> mylists:partition(fun(I) -> is_atom(I) end, [1, a, 2, b, 3.0]).
{[a,b],[1,2,3.0]}
{% endcodeblock %}

<p>
  <strong>replicated/2</strong><br/>
  Creates a list of Items of length Times.
</p>

{% codeblock lang:ruby %}
% Generator: only used for fixing the length
% Expression: a fixed item

replicated(Item, Times) ->
  [Item || _ <- lists:seq(1, Times)].
 
1> mylists:replicated(':-)', 10).
[':-)',':-)',':-)',':-)',':-)',':-)',':-)',':-)',':-)',':-)']
{% endcodeblock %}

<p>
  <strong>replicate_items/2</strong><br/>
  Replicates each elements of the list Times times.
</p>

{% codeblock lang:ruby %}
replicate_items(Times, List) ->
  mylists:flatten([[Item || _ <- lists:seq(1, Times)] || Item <- List]).
 
% same as
% replicate_items(Times, List) ->
%   mylists:flatten([replicated(Item, Times) || Item <- List]).
 
1> mylists:replicate_items(3, [a, b, c]). 
[a,a,a,b,b,b,c,c,c]
{% endcodeblock %}

<p>
  <strong>member/2</strong><br/>
  Returns true if an element is a member of the list, else returns false.
</p>

{% codeblock lang:ruby %}
member(Elem, List) ->
  [] /= [ok || I <- List, I == Elem].
 
1> mylists:member(a, [3, 2, 1, a, x, z]).
true
2> mylists:member(a, [3, 2, 1, aa, x, z]).
false
{% endcodeblock %}

<p>
  <strong>member_times/2</strong><br/>
  Returns the number of occurences of an element in a list.
</p>

{% codeblock lang:ruby %}
member_times(Elem, List) ->
  length([ok || I <- List, I == Elem]).
 
1> mylists:member_times(a, [1, a, 2, 3, b, a, c]).
2
2> mylists:member_times(a, [1, a, 2, 3, b, c]).   
1
3> mylists:member_times(a, [1, 2, 3, b, c]).   
0
{% endcodeblock %}

<p>
  <strong>quicksort/1</strong><br/>
  This is the famous Quicksort implementation that is often used to show the power and compactness of Erlang.
</p>

{% codeblock lang:ruby %}
qsort([]) ->
  [];
qsort([Pivot | List]) ->
  qsort([I || I <- List, I < Pivot]) 
  ++ 
  [Pivot | qsort([I || I <- List, I >= Pivot])].
 
1> mylists:qsort([7, 1, 3, 9, 1, 2, 0, 4, 6, 5]).
[0,1,1,2,3,4,5,6,7,9]
{% endcodeblock %}

<p>
  <strong>Multiple Generators</strong><br/>
  Now I will present some examples with multiple generators.
</p>

{% codeblock lang:ruby %}
1> [{I, J} || I <- [1, 2, 3], J <- [a, b, c]].
[{1,a},{1,b},{1,c},{2,a},{2,b},{2,c},{3,a},{3,b},{3,c}]
 
% duplicate list
2> [I || _ <- [a, a], I <- [1, 2, 3]].
[1,2,3,1,2,3]
 
% duplicate elements
3> [I || I <- [1, 2, 3], J <- [a, a]].
[1,1,2,2,3,3]
 
% discard elements and duplicate the others
4> Discard = [2, 4].
[2,4]
5> [I || I <- [1, 2, 3, 4, 5, 6], not(lists:member(I, Discard)), _ <- [a, a]].
[1,1,3,3,5,5,6,6]
 
% subsequences
8> [[I || I <- lists:seq(1, J)] || J <- lists:seq(1, 10)].
[[1],
 [1,2],
 [1,2,3],
 [1,2,3,4],
 [1,2,3,4,5],
 [1,2,3,4,5,6],
 [1,2,3,4,5,6,7],
 [1,2,3,4,5,6,7,8],
 [1,2,3,4,5,6,7,8,9],
 [1,2,3,4,5,6,7,8,9,10]]
{% endcodeblock %}
