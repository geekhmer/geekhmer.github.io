---
layout: post
title: "What We Should Test with RSpec in Rails"
date: 2014-08-04 22:27
comments: true
categories: [Ruby on Rails, Ruby, testing]

keywords: what we should test with rspec in rails, rspec, test with rspec
description: What We Should Test with RSpec in Rails
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" />
</p>

<p>
  Well, what is takes to begin testing Rails applications. The hardest part of being a beginner is that you often don't know what you should test with RSpec in Rails.
</p>

<p>
  Here is the most important thing is that you are testing: Feature specs, Model specs, Controller specs, View specs, Route specs.
</p>

<p>
  <strong>Feature specs</strong><br/>
  Feature specs is a kind of acceptance test, the tests that walk through your entire application ensuring that each of the components work together.<br/>
  They are written from the perspective of a user clicking around the application and filling in forms on the page.<br/>
  While Feature specs are great for testing high level functionality, keep in mind that feature specs is slow to run.
</p>

<p>
  <strong>Model specs</strong><br/>
  Model specs are similar to unit tests in that they are used to test smaller parts of the system, such as classes or methods, and they interact with the database too.
</p>

<p>
  <strong>Controller specs</strong><br/>
  When testing multiple paths through a controller is necessary, we favor using controller specs over feature specs, as they are faster to run and often easier to write.
</p>

<p>
  <strong>View specs</strong><br/>
  View specs is great for testing the conditional display of information in the templates. Most developers forget about these tests and use feature specs instead.<br/>
  While you can cover each view conditional with a feature specs, I prefer to user view specs.
</p>

<p>
  <strong>Route specs</strong><br/>
  Most Ruby on Rails developers don’t test their routes, If you ever need to test an abstract base controller independently from any subclass, you will like need to add route specs for your testing.
</p>

<p>
  So far so good, this was just an overview of what we should get started testing Rails. :)
</p>