---
layout: post
title: "Ruby Hooks for Metaprogramming"
date: 2015-03-22 10:45
comments: true
categories: [Ruby]
keywords: Ruby Hooks for Metaprogramming
---

<p>
  <img src="/images/rights_and_wrongsof_ruby.jpg" width="400" alt="Ruby Hooks for Metaprogramming" />
</p>


<h3>
  Overview
</h3>

<p>
  <strong>What is a Hook?</strong><br/>
   In programming, a hook is a place and usually an interface provided in packaged code that allows a programmer to insert customized programming. For example, a programmer might want to provide code that analyzed how often a particular logic path was taken within a program.
</p>

<h3>
  Ruby Hooks
</h3>

<p>
  Ruby lets you hook in and change a lot of behavior of the core language. Methods, constants, classes, variables… etc. Ruby lets you query them all, and change a lot about them.
</p>

<p>
  Here’s summaries and links for all the hooks I could find:
</p>

<p>
  <strong>Methods</strong><br/>
  - <code>respond_to_missing?</code> - A way to make sure your dynamic methods defined with method_missing also handle respond_to?<br/>
  - <code>method_missing</code> - Called when a method cannot be found, potentially to allow dynamically defining one instead.<br/>
  - <code>method_added</code> - Called whenever a method is added which can be used to modify the method.<br/>
  - <code>method_removed</code> - Called whenever a method is removed.<br/>
  - <code>singleton_method_added</code> - Method added to the singleton class of the object, to be callable just on this one instance.<br/>
  - <code>singleton_method_removed</code> - Method removed from singleton class.<br/>
  - <code>method_undefined</code> - A method has been undefined, with undef_method. Undef_method is different from remove_method because remove_method may still allow superclasses to define the method – undef_method means it’s gone entirely.<br/>
  - <code>singleton_method_undefined</code> - Called when a singleton method is undefined entirely.<br/>
  - <code>initialize_copy</code> - An optional callback when cloning any Object.
</p>

<p>
  <strong>Classes</strong><br/>
  - <code>inherited</code> - A Ruby class is subclassed.
</p>

<p>
  <strong>Modules</strong><br/>
  - <code>append_features</code> - A Module is included, and its constants, methods and variables used.<br/>
  - <code>included</code> - A Module is included, which usually obsoletes "append_features".<br/>
  - <code>extend_object</code> - A Module extends an Object.<br/>
  - <code>extended</code> - An Object is extended by a module, which mostly obsoletes extend_object.<br/>
  - <code>const_missing</code> - A constant isn’t already present.
</p>

<p>
  <strong>Marshalling</strong><br/>
  - <code>marshal_dump</code> - Called on an object to have it dump itself in Ruby Marshal format.<br/>
  - <code>marshal_load</code> - Called on an object to have it load itself in Ruby Mashal format.
</p>

<p>
  <strong>Coercion</strong><br/>
  - <code>coerce</code> - Called by the first type in a two-argument operator on the second argument, to make it turn into something the first argument can recognize.<br/>
  - <code>induced_from</code> - Deprecated, please don’t use.<br/>
  - <code>to_i</code>, <code>to_f</code>, <code>to_s</code>, <code>to_a</code>, <code>to_hash</code>, <code>to_proc</code> and others - Conversions, indicating that the object is being used as a type and should try to convert itself to that type.
</p>