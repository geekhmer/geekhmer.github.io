---
layout: post
title: "assets_helper gem"
date: 2013-12-19 21:27
comments: true
categories: [Ruby]
keywords: assets_helper gem, assets helper gem, ruby, ruby gem
description: assets_helper is the gem that use to include css and javascript by controller name automatically, it mean that include only css and javascript file in the controller that you are running.
---

<!-- **Content start here** -->
<p>
  <a href="http://rubygems.org/gems/assets_helper" target="_blank">assets_helper</a> is the gem that use to include css and javascript by controller name automatically, it mean that include only css and javascript file in the controller that you are running.
</p>

<p>
  <strong>Installation</strong><br/>
  Add this line to your application's Gemfile:<br/>
  <code>gem 'assets_helper'</code>
</p>
<p>
  And then execute:<br/>
  <code>$ bundle</code>
</p>
<p>
  Or install it yourself as:<br/>
  <code>$ gem install assets_helper</code>
</p>

<p>
  <strong>Usage</strong><br/>
  1. Add <code>before_filter :include_css, :include_javascript</code> in application_controller.rb:<br/>
</p>
{% codeblock application_controller.rb lang:ruby %}
class ApplicationController < ActionController::Base
  protect_from_forgery

  before_filter :include_css, :include_javascript
end
{% endcodeblock %}

<p>
  2. Add <code>= yield :asset</code> n your layout file (exp: application.html.haml):<br/>
</p>
{% codeblock application.html.haml lang:ruby %}
!!!
%html
  %head
    %title JongEat
    = stylesheet_link_tag    "application", :media => "all"
    = javascript_include_tag "application"
    = yield :asset
    = csrf_meta_tags
{% endcodeblock %}

<p>
  3. Example if you create a controller name homes:<br/>
  <code>$ rails generate controller homes</code>
</p>

<p>
  4. Go to app/assets/javascripts and create a folder name homes like your controller (homes) you create below. And input javascript file that you use for controller homes in this folder ( app/assets/javascripts/homes ), and no need to include in application.js file.<br/>
</p>
<p>
  <a class="fancybox" href="/images/javascript_homes.png"><img src="/images/javascript_homes.png" alt="" width="680" /></a>
</p>
<p>
  5. Go to app/assets/stylesheets and create a folder name homes like your controller(homes) you create below. And input css file that you use for controller homes in this folder(app/assets/stylesheets), and no need to include or import in application.css file.<br/>
</p>
<p>
  <a class="fancybox" href="/images/css_homes.png"><img src="/images/css_homes.png" width="680" /></a><br/>
</p>

So whenever you run homes controller, it include only css and javascript file that you use for homes controller, and can make your project run fast.<br/>
<p>
  <a class="fancybox" href="/images/browser_homes_controller.png"><img src="/images/browser_homes_controller.png" width="680" /></a><br/>
</p>