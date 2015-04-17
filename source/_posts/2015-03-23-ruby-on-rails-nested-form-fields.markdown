---
layout: post
title: "Ruby on Rails Nested Form Fields"
date: 2015-03-23 23:47
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Ruby on Rails Nested Form Fields
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails Nested Form Fields" />
</p>

<p>
  It’s super common to want to edit records along with their has_many associations on a single page.
</p>

<p>
  This Rails gem helps creating forms for models with nested has_many associations and relies on jQuery to dynamically add and remove nested form fields without a page reload.<br/>
  - Works for arbitrarily deeply nested associations (tested up to 4 levels).<br/>
  - Works with form builders like simple_form.<br/>
  - Requires at least Ruby 1.9 and the Rails asset pipeline.
</p>

<p>
  To install, add nested_form_fields to your application’s Gemfile:
</p>

{% codeblock Gemfile lang:ruby %}
gem 'nested_form_fields'
{% endcodeblock %}

<p>
  Run bundle intall to install the gem:
</p>

{% codeblock lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  In application.js file add:
</p>

{% codeblock application.js lang:ruby %}
//= require nested_form_fields
{% endcodeblock %}

<p>
  assuming that you have a User model with nested videos:
</p>

{% codeblock user.rb lang:ruby %}
class User < ActiveRecord::Base
  has_many :videos
  accepts_nested_attributes_for :videos, allow_destroy: true
end
{% endcodeblock %}

<p>
  Use the nested_fields_for helper inside your user form to add the video fields:
</p>

{% codeblock lang:ruby %}
= form_for @user do |f|
  = f.nested_fields_for :videos do |ff|
    = ff.text_field :video_title
{% endcodeblock %}

<p>
  Links to add and remove fields can be added using the add_nested_fields_link and remove_nested_fields_link helpers:
</p>

{% codeblock lang:ruby %}
= form_for @user do |f|
  = f.nested_fields_for :videos do |ff|
    = ff.remove_nested_fields_link
    = ff.text_field :video_title
  = f.add_nested_fields_link :videos
{% endcodeblock %}

<p>
  Note that remove_nested_fields_link needs to be called within the nested_fields_for call and add_nested_fields_link outside of it via the parent builder.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>