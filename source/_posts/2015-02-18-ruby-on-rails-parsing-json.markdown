---
layout: post
title: "Ruby on Rails Parsing JSON"
date: 2015-02-18 22:27
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Ruby on Rails Parsing JSON, Rails Parsing JSON, Ruby Parsing JSON
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails Parsing JSON" />
</p>

<p>
  Sometime you have to deal with parsing out JSON. Luckily, this is very easy in any recent version of Ruby. Given the string below:
</p>

{% codeblock lang:ruby %}
{
  "products": [{
    "name": "Mac Pro",
    "price": "2500"
  }]
}
{% endcodeblock %}

<p>
  You can easily parse this string by using:
</p>

{% codeblock lang:ruby %}
require 'json'
h = JSON.parse(string)
{% endcodeblock %}

<p>
  In the above code, it will return a ruby hash that mimics the JSON structure. This means that you can do stuff like this:
</p>

{% codeblock lang:ruby %}
h["products"][0]["name"] # => "Mac Pro"
{% endcodeblock %}

<p>
  So far so good, that's it! See ya! :)
</p>
