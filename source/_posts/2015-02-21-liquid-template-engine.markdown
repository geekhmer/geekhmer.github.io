---
layout: post
title: "Liquid Template Engine"
date: 2015-02-21 15:00
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Ruby on Rails Liquid Template Engine, Rails Liquid Template Engine, Liquid Template Engine, Liquid Templates Engine, Liquid Templating Engine
---

<p>
  <img src="/images/move_to_rails.png" alt="Liquid Template Engine" />
</p>

<p>
  Liquid is a templating engine that allows you to enable your users to quickly and easily customize your views at run-time while maintaining the safety, security, and integrity of your servers.
</p>

<p>
  <strong>Liquid Syntax</strong><br/>
  In liquid, there are two different types of markup.
</p>

<p>
  The first type of markup is output markup, is denoted by double sets of curly braces.
</p>

{% codeblock lang:ruby %}
{% raw %}
Hi {{ user.name }}
{% endraw %}
{% endcodeblock %}

<p>
  The second type of markup is tag markup, is typically used for logic and control structures such as loops.
</p>

{% codeblock lang:ruby %}
{% raw %}
<ul>
  {% for user in users %}
    <li>{{ user.name }}</li>
  {% endfor %}
</ul>
{% endraw %}
{% endcodeblock %}

<p>
  You can do basic if statements as well:
</p>

{% codeblock lang:ruby %}
{% raw %}
{% if user.name != 'JingLong' %}
  Hi JingLong!
{% elsif user.name == 'Bunlong' %}
  Hey Bunlong!
{% else %}
  Hello {{ user.name }}
{% endif %}
{% endraw %}
{% endcodeblock %}

<p>
  <strong>List of All of the Tags Available in Liquid</strong><br/>
</p>

Tag | Description
--- | ---
assign | Assigns some value to a variable.
capture | Block tag that captures text into a variable.
case | Block tag, its the standard case...when block.
comment | Block tag, comments out the text in the block.
cycle | Cycle is usually used within a loop to alternate between values, like colors or DOM classes.
for | For loop.
if | Standard if/else block.
include | Includes another template; useful for partials.
raw | Temporarily disable tag processing to avoid syntax conflicts.
unless | Mirror of if statement.

<p>
  <br/>
  So far so good, That’s it! See ya! :)
</p>