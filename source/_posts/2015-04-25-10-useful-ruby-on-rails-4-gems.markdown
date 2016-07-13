---
layout: post
title: "10 Useful Ruby on Rails 4 Gems"
date: 2015-04-25 16:09
comments: true
categories: [Ruby, Ruby on Rails]
keywords: 10 Useful Ruby on Rails 4 Gems
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="10 Useful Ruby on Rails 4 Gems" />
</p>

<p>
  The Ruby on Rails framework is an extremely powerful tool for developing web applications. It comes with plenty of built-in features which help accelerate the development of your web application such as intelligent routing and an object-relation mapper, all using an MVC pattern.
</p>

<p>
  Rails is designed in a way to be very easily extended using Ruby gems. This has created a large ecosystem of Ruby gems which can extend your application and accelerate your development process even more by reducing the time involved working in developing common functionality. We’re going to go over a few gems which we consider to be very useful.
</p>

<p>
  <strong>Devise</strong><br/>
  <a href="https://rubygems.org/gems/devise" target="_blank">Devise</a> is most probably the most commonly used Gem when using Ruby on Rails. It provides an easy-to-use authentication solution for your Rails application which will allow you to get login, registration, forget password, account locks and much more account-related features by simply using this Gem.
</p>

<p>
  <strong>Pundit</strong><br/>
  It’s important to know the distinction between authentication and authorization. Devise helps authenticate users and verify who they are while authorization ensures that the user is allowed to perform an action or access a resource. <a href="https://rubygems.org/gems/pundit" target="_blank">Pundit</a> takes care of this entire process by providing a simple way of defining authorization systems using nothing but Ruby classes.
</p>

<p>
  <strong>Slim</strong><br/>
  There are many template systems out there. Ruby on Rails uses the eRuby template system by default, however, it’s typically something that users will choose to replace. <a href="https://rubygems.org/gems/slim" target="_blank">Slim</a> is a common replacement because it allows you to maintain very easy to read templates due to it’s simple syntax while maintaining very fast compilation times.
</p>

<p>
  <strong>Draper</strong><br/>
  <a href="https://rubygems.org/gems/draper" target="_blank">Draper</a> allows you to build decorators around your models. It helps make your views much cleaner and lets you avoid writing helpers. Instead of procedurally calling helpers with models, you instead define a decorator which wraps the original model and provides a list of extended method and attributes to your object.
</p>

<p>
  <strong>Cells</strong><br/>
  You’ll often re-use many components of your application. Typically, partials are used for this type of behavior however you must make sure that your controllers that call the partial all have consistent behavior. <a href="https://rubygems.org/gems/cells" target="_blank">Cells</a> allow you to take parts of your controller and encapsulate them into their own little controller. This helps make your code much cleaner and avoid the long helper/partial/filter mes.
</p>

<p>
  <strong>FriendlyId</strong><br/>
  Typically, resources URLs are identified by their primary key which is usually their database ID. However, this can result in unoptimized web page URLs which are not user-friendly to read either. <a href="https://rubygems.org/gems/friendly_id" target="_blank">FriendlyId</a> can easily transform your URLs to much friendlier and easy to remember URLs for little to no code changes in your web application.
</p>

<p>
  <strong>Simple Form</strong><br/>
  Forms are at the heart of every single web application. If there is any level of interaction with the user, it typically is done using a web form. <a href="https://rubygems.org/gems/simple_form" target="_blank">Simple Form</a> helps simplify this very simple yet repetitive task. By implementing a simple and easy to use DSL for building forms, you can spend less time writing HTML for your forms and more time on the core business logic of your application.

</p>

<p>
  <strong>Paperclip</strong><br/>
  File attachments are never easy to work with. They typically involve a significant amount of work to implement and even more time to make sure that they are implemented in a very secure manner. <a href="https://rubygems.org/gems/paperclip" target="_blank">Paperclip</a> takes care of this entire process for you inside your Rails application and extends it even more to things like transforming images to thumbnails and much more.
</p>

<p>
  <strong>Kaminari</strong><br/>
  <a href="https://rubygems.org/gems/kaminari" target="_blank">Kaminari </a> is not a very descriptive name for a Gem, however, it is one of the most popular Gems with almost 5 million downloads. It enables you to paginate anything from ActiveRecord relations to simple arrays using a clean, easy to use and simple scope-based API which is fully agnostic to whatever ORM or template engine you use.
</p>

<p>
  <strong>Sidekiq</strong><br/>
  There are many choices for background processing tools when using Ruby on Rails, however, <a href="https://rubygems.org/gems/sidekiq" target="_blank">Sidekiq</a> is the one of the most popular ones. The reason behind it’s popularity is the simplicity of it’s API and how it scales much better than other background processors.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
