---
layout: post
title: "Ruby Readable Booleans"
date: 2014-08-30 09:58
comments: true
categories: [Ruby on Rails, Ruby]

keywords: Ruby Readable Booleans, Ruby, Rails, Ruby on Rails, Ruby Developer, Rails Developer, Ruby on Rails Developer, Cambodia Ruby Developer, Cambodia Ruby on Rails Developer
description: Ruby Readable Booleans
---

<p>
  <img src="/images/rights_and_wrongsof_ruby.jpg" width="400" />
</p>

<p>
  There's a great little trick you can do to improve the readability of your code. A common problem is dealing with methods that have a boolean flag arguments. Here's an example I ran into just today in a Rails application:
</p>

{% codeblock lang:ruby %}
def rating_stars(..., clickable = false)
  # ...
end
{% endcodeblock %}

<p>
  The problem with this is that you typically see calls like this scattered around the application:
</p>

{% codeblock lang:ruby %}
<%= rating_stars(..., true) %>
{% endcodeblock %}

<p>
  Would you know what true did there if I hadn't shown you the name of the variable first? I didn't. I had to go hunting for that method definition.
</p>

<p>
  Ironically the opposite problem, a magical dangling false, is much more rare in my experience. That's typically the default for these kind of arguments and it just makes more sense and reads better to leave it out.
</p>

<p>
  Anyway, the point is that we can typically improve the ease of understanding the common case. Remember that one of the new features of Ruby 2.0 is keyword arguments. keyword arguments make it easier create readable method. For example, after looking up the method and understanding what was needed: 
</p>

{% codeblock lang:ruby %}
def rating_stars(..., clickable: false)
  # ...
end
{% endcodeblock %}

{% codeblock lang:ruby %}
<%= rating_stars(..., clickable: true) %>
{% endcodeblock %}

<p>
  So far so good, my hope is that might save a future maintainer a trip to the method definition to understand the call.
</p>