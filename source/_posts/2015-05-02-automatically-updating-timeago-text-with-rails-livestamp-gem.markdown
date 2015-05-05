---
layout: post
title: "Automatically-updating Timeago Text with rails-livestamp Gem"
date: 2015-05-02 08:29
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Automatically-updating Timeago Text with rails-livestamp Gem
---

<p>
  <img src="/images/rubygems_logo.png" width="200" alt="Automatically-updating Timeago Text with rails-livestamp Gem" />
</p>

<p>
  <a href="https://github.com/Bunlong/rails-livestamp" target="_blank">rails-livestamp</a> is a simple jQuery plugin that makes it easy to provides automatically updating text to your timestamped HTML elements.
</p>

<p>
  <strong>Installation</strong><br/>
  Add this line to your application's Gemfile:
</p>

{% codeblock Gemfile lang:ruby %}
gem 'rails-livestamp', '~> 1.1.3'
{% endcodeblock %}

<p>
  And then execute:
</p>

{% codeblock lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  Or install it yourself as:
</p>

{% codeblock lang:ruby %}
gem install rails-livestamp
{% endcodeblock %}

<p>
  To use rails-livestamp add this require statement to your application.js file:
</p>

{% codeblock application.js lang:ruby %}
//= require rails-livestamp
{% endcodeblock %}

<p>
  <strong>Usage</strong><br/>
  No extra JavaScript required! Just use a <span> with the data-livestamp attribute set to the desired Unix timestamp (in seconds), like this:
</p>

<p>
  If you use ERB:
</p>

{% codeblock lang:ruby %}
You discovered rails-livestamp <span data-livestamp="#{Time.now.to_i}"></span>.
{% endcodeblock %}

<p>
  If you use HAML:
</p>

{% codeblock lang:ruby %}
You discovered rails-livestamp 
%span{"data-livestamp" => "#{Time.now.to_i}"}.
{% endcodeblock %}

<p>
  And you will see something like this:
</p>

{% codeblock lang:ruby %}
You discovered rails-livestamp a minutes ago. 
{% endcodeblock %}

<p>
  Wait half a minute - the livestamp will update automatically.
</p>
