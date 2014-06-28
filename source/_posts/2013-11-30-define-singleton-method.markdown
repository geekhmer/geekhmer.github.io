---
layout: post
title: "define_singleton_method"
date: 2013-11-30 09:16
comments: true
categories: [Ruby, Ruby Metaprogramming]
keywords: define_singleton_method, define singleton method, ruby metaprogramming
description: define singleton method
---

<p>
  define_singleton_method use to define singleton method.
</p>

<p>
  <strong>example</strong><br/>
</p>
{% codeblock example lang:ruby %}
class Sayer
  def self.shouts(*words)
    words.each do |word|
      define_singleton_method "shout_#{word}" do
        p word
      end
    end
  end

  shouts :hello, :goodbye
end

Sayer.shout_hello
Sayer.shout_goodbye
{% endcodeblock %}