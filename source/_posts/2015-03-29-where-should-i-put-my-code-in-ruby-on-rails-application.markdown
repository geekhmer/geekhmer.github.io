---
layout: post
title: "Where Should I Put My Code in Ruby on Rails Application?"
date: 2015-03-29 09:44
comments: true
categories: [Ruby on Rails]
keywords: Where Should I Put My Code in Ruby on Rails Application?
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails Nested Form Fields" />
</p>

<p>
  Sometimes you’re sure that’s not the right place for that piece of code, but where does it go? "Refactor" is only a good answer if you know how to fix it.
</p>

<p>
  In a Rails application, and what kind of code goes where?
</p>

<p>
  <strong>Models</strong><br/>
  For code about your database or domain objects, the model is your first step in Rails. Models are powerful, easy to test, reusable across applications and more like non-Rails code than most of Rails — familiar, even if you don’t know Rails yet.<br/>
</p>

<p>
  If there’s a good way to put the code in your model, that’s usually a safe bet and a good idea.
</p>

<p>
  Write tests too, of course!
</p>

<p>
  <strong>Controllers</strong><br/>
  It’s easy to put lots of code in your controllers, but it’s almost always a mistake. Business logic for your app should get out of the controller and into the model as quickly as possible. Logic about how things are shown to the user should go into the view. In general, the controller should be a tiny, thin glue layer putting together your other components.
</p>

<p>
  <strong>Views</strong><br/>
  Having lots of logic in your views is a huge anti-pattern. Don’t do it. It’s hard to test, it’s hard to find, it’s hard to write sandwiched in between the HTML… Just don’t.
</p>

<p>
  Instead, your views should contain HTML, variables that turn into HTML, and calls to helper methods that generate HTML — or whatever your final output format is. There should be no logic in there to test. No conditionals, no loops, no non-display methods. If you add an output format, there should be no code to repeat because all the interesting data transforms already happened, and no other output format cares about your HTML-only helpers. Right?
</p>

<p>
  <strong>Helpers</strong><br/>
  Rails "helpers" are very specifically view helpers. They’re automatically included in views, but not in controllers or models. That’s on purpose.
</p>

<p>
  Code in the application helper is included in all the views in your application. Code in other helpers is included in the corresponding view. If you find yourself writing big loops, method calls or other logic in the view but it’s clearly display logic, move it into a method in the helper.
</p>

<p>
  <strong>Lib Directory</strong><br/>
  Every Rails app starts with a /lib directory, but not much explanation of it.
</p>

<p>
  Remember that helpers are specifically view helpers? What if you wanted a controller helper? Or a model helper? Sometimes you can use a parent controller or parent model, but that’s not always the best choice.
</p>

<p>
  If you want to write a helper module for non-view logic, the /lib directory is usually the best place to put it. For example, logging code or some kinds of error handling may be a cross-cutting concern like that.
</p>

<p>
  Also, if you’re putting everything in the ApplicationController or ApplicationHelper, those can get big. Consider factoring some of that code out into helpers, or into /lib.
</p>

<p>
  Stuff in /lib isn’t always automagically included for you like controllers and models. So you may need to explicitly require the file, not just use the name of the class. (<a href="http://geekhmer.github.io/blog/2014/04/24/autoload-all-files-in-lib-directory-ruby-on-rails/">Autoload All Files in Lib Directory Ruby on Rails</a>)
</p>

<p>
  <strong>Gems</strong><br/>
  Sometimes you have reusable pieces in your application. A controller or model might be needed by multiple different Rails apps. A particular piece of logic for logging or display might be useful to a lot of different folks. You might even find a different way of doing things that most Rails apps would benefit from.
</p>

<p>
  These are all cases where you want to create a new gem and have your applications use it instead of sharing a directory of code.
</p>

<p>
  These days it’s really easy to create a new gem, so don’t be intimidated. If you haven’t worked through the first free chapter of Rebuilding Rails, this may be a good time to do it — it’ll show you how to quickly, easily create and use a new gem.
</p>

<p>
  <strong>Assets</strong><br/>
  In a few cases, you’re not even writing Ruby code. Instead, it may be Sass, Scss, JavaScript or CoffeeScript. In this case, it generally belongs under app/assets.
</p>

<p>
  <strong>Concerns and Exceptions</strong><br/>
  Rails has a very specific, very unusual setup. I think it’s a good idea for small apps, but only use Rails until it hurts. If your application gets too big or complicated, the Rails code organization may hurt more than it helps you.
</p>

<p>
  There are several "grow out of Rails" approaches to apply alternate architectures to the framework. From Hexagonal Rails to Objects on Rails to the more general Clean Ruby DCI approach. I won’t tell you which to use, but I’ll tell you that you’re better off starting with plain, simple Rails and growing out of it.
</p>

<p>
  Most Rails apps, and even more Rails controllers, don’t need to get all that big. They often don’t need to change much. Why go complicated when simple is working great?
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
