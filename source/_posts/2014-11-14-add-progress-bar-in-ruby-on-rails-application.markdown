---
layout: post
title: "Add Progress Bar in Ruby on Rails Application"
date: 2014-11-14 22:02
comments: true
categories: [Ruby, Ruby on Rails]

keywords: progress bar, 
description: Add Progress Bar in Ruby on Rails Application
---

<p>
  <img src="/images/youtube_progress_bar.png" width="700" alt="Add Progress Bar in Ruby on Rails Application" />
</p>

<p>
  Okay, today I would like to show how to add progress bar in Ruby on Rails application. For add progress bar in Ruby on Rails application I use nprogress-rails ruby gem.
</p>

<p>
  <strong>Installation</strong><br/>
  Add nprogress-rails ruby gem to Gemfile:
</p>

{% codeblock Gemfile lang:ruby %}
gem 'nprogress-rails'
{% endcodeblock %}

<p>
  And then open terminal type:
</p>

{% codeblock lang:ruby %}
$ bundle
{% endcodeblock %}

<p>
  <strong>Usage</strong><br/>
  Add the requires to the application.js:
</p>

{% codeblock application.js lang:ruby %}
//= require nprogress
//= require nprogress-turbolinks
{% endcodeblock %}

<p>
  Also, into the application.css.scss:
</p>

{% codeblock application.css.scss lang:ruby %}
*= require nprogress
*= require nprogress-bootstrap
{% endcodeblock %}

<p>
  And then, add NProgress.start(); to application.js for loading progress bar whenever you load each pages:
</p>

{% codeblock application.js lang:ruby %}
$(document).ready(function() {
  NProgress.start();
});
{% endcodeblock %}

<p>
  So far so good, let enjoy the progress bar in your application. Thank for reading :)
</p>