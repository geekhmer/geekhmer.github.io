---
layout: post
title: "Erlang Modules and Compiling"
date: 2015-01-17 11:09
comments: true
categories: [Erlang]
keywords: The Erlang Learning Sequence, Learning Erlang, Learn Erlang, Study Erlang, Studying Erlang, The Studying Erlang, Chicagoboss Framwork, Erlang Modules and Compiling, Modules and Compiling, Erlang Modules, Modules
---

<p>
  <img src="/images/logo_erlang.png" alt="Erlang Variables" />
</p>

<p>
  Erlang code is divided into modules. A module consists of a sequence of attributes and function declarations, each terminated by period (.). It provided the contained functions with a common namespace as well, You can imagine a module as a package in Java, or a header file in C. Program in Erlang spans over more than one modules.
</p>

<p>
  Example:
</p>

{% codeblock m.erl lang:ruby %}
-module(m).          % module attribute
-export([fact/1]).   % module attribute

fact(N) when N>0 ->  % beginning of function declaration
  N * fact(N-1);     %  |
fact(0) ->           %  |
  1.                 % end of function declaration
{% endcodeblock %}

<p>
  <h3>Defining Modules</h3>
  Assume that we want to create a module that will contain our own implementation of list functions and name it mlists.<br/>
  First, we create a file named mlists.erl that will contain the module. The container file and the module names have to be the same.
</p>

<p>
  <h3>Calling a Module's Function</h3>
  The calling function of a module, convention in Erlang is <code>module:function(argument1, argument2, ...)</code>.
</p>

<p>
  Example:
</p>

{% codeblock lang:ruby %}
1> lists:max([1,3,2]).
3
{% endcodeblock %}

<p>
  <h3>Calling a Module Function within the Module</h3>
  A function defined within the module file can be called either as <code>module_name:function(arguments1, argument2, ...)</code>, or <code>function(arguments1, argument2, ...)</code>, so the module name can be skipped.
</p>

<p>
  <h3>Modules Attributes</h3>
  Then we need to define the module’s attributes. An attribute is the information that we provide to the Erlang compiler. It is placed as a directive (usually) in the top of the file and has the <code>-attribute_name(attribue_value(s)).</code> format. The one attribute that we have to define is the one providing the module name.
</p>

<p>
  <strong>-module(Module).</strong><br/>
  Module declaration, defining the name of the module. The name Module, an atom, should be the same as the file name minus the extension erl. Otherwise code loading will not work as intended. 
</p>

<p>
  Example in the top of mlists.erl place:
</p>

{% codeblock mlists.erl lang:ruby %}
-module(mlists).
{% endcodeblock %}

<p>
  <strong>-export(Functions).</strong><br/>
  Exported functions is used to define which functions the module exports, where "exports" means that they will be available to be called outside the module.
</p>

<p>
  All the functions that are not exported by the module are only visible within the file, similar with the private functions in Ruby, Java and the static ones in C.
</p>

<p>
  Example:
</p>

{% codeblock mlists.erl lang:ruby %}
-module(mlists).
-export([function1/arity1, function2/arity2, ...]).
{% endcodeblock %}

<p>
  <strong>-import(Module,Functions).</strong></br>
  You can use this directive in order to import the selected exported functions of a module in the namespace of another one. It means that if you do so, you will be able to call the functions without the module prefix. Although in some cases it could be convenient, it is not recommended to use this directive, because it decreases the code’s readability.
</p>

<p>
  <strong>-compile(Options).</strong><br/>
  Compile is used to pass compilation instructions to the compiler.
</p>

<p>
  Example:
</p>

{% codeblock lang:ruby %}
-compile([export_all]).
{% endcodeblock %}

<p>
  <strong>-on_load(Function).</strong><br/>
  Names a function that should be run automatically when a module a loaded.
</p>

<p>
  <h3>Compiling Modules</h3>
  Erlang programs must be compiled to object code. The compiler can generate a new file which contains the object code. The current abstract machine which runs the object code is called BEAM. To compile the modules start an Erlang emulator on the folder that contains your source files. In order to compile a .erl, using <code>c(Module)</code> Bult-in Function.
</p>

<p>
  Example:
</p>

{% codeblock lang:ruby %}
1> c(mlist).
{ok, mlist}
{% endcodeblock %}

<p>
  If there is no error occurs, the compiler generates the compiled .beam file.
</p>

<p>
  <h3>Loading a Module</h3>
  The object code must be loaded into the Erlang runtime system. To load the modules start an Erlang emulator on the folder that contains your source files. In order to load a compiled module .beam, using <code>l(Module)</code> Bult-in Function.
</p>

{% codeblock lang:ruby %}
1> l(mlist).
{module, mlist}
{% endcodeblock %}

<p>
  I promise, from now on the posts will be far more interesting. Next one, or two posts will be about defining functions in Erlang. You can imagine how important functions are for a functional programming language. see ya! :)
</p>
