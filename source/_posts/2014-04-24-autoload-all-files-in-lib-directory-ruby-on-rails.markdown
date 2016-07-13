---
layout: post
title: "Autoload All Files in Lib Directory Ruby on Rails"
date: 2014-04-24 21:59
comments: true
categories: [Ruby on Rails]

keywords: ruby, ruby on rails, autoload all files in lib directory ruby on rails, autoload files in lib directory ruby on rails.
description: Autoload All Files in Lib Directory Ruby on Rails
---

<p>
  In Ruby on Rails to create your own ruby library you have to create ruby library in /lib directory, so far so good to autoload all ruby library in /lib directory you have to add some code below in /config/application.rb.
</p>

{% codeblock application.rb lang:javascript %}
config.autoload_paths += %W(#{config.root}/lib)
config.autoload_paths += Dir["#{config.root}/lib/**/"]
config.autoload_paths << "#{config.root}/lib"
{% endcodeblock %}