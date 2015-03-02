---
layout: post
title: "Slim Template Language"
date: 2015-02-22 22:29
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Slim Template Language
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Slim Template Language" />
</p>

<p>
  Slim is a replacement for ERB, similar to HAML. Slim provides a nice lean syntax that's easy to pick up and learn. In this article we will learn how to use Slim. Let’s run through this with me.
</p>

<p>
  <strong>Basic Syntax</strong><br/>
  Slim is actually really easy to learn. Indentation matters, but the indentation depth can be chosen as you like. If you want to first indent 2 spaces, then 5 spaces, it's your choice.
</p>

<p>
  Example: 
</p>

{% codeblock lang:ruby %}
p
  | This is a very long paragraph
{% endcodeblock %}

<p>
  Which translates to:
</p>

{% codeblock lang:ruby %}
<p>
  This is a very long paragraph.
</p>
{% endcodeblock %}

<p>
  In the above example you'll notice two things. First, the pipe (|) character is used to delimit free-form text on a new line. Second, we simply used p to specify the paragraph tag. This works for any tag, for instance you could just as easily have done:
</p>

{% codeblock lang:ruby %}
h1 
  | Large Title
{% endcodeblock %}

<p>
  Or more simply:
</p>

{% codeblock lang:ruby %}
h1 Large Title
{% endcodeblock %}

<p>
  In the above example that we didn't specify the pipe (|) character. The pipe (|) character is only needed if we are entering free-form text on a new line.
</p>

<p>
  You can create a div with a default class by using:
</p>

{% codeblock lang:ruby %}
class
{% endcodeblock %}

<p>
  Which translates to:
</p>

{% codeblock lang:ruby %}
<div class="class"></div>
{% endcodeblock %}

<p>
  You can create a div with an id by using:
</p>

{% codeblock lang:ruby %}
#id
{% endcodeblock %}

<p>
  Which translates to:
</p>

{% codeblock lang:ruby %}
<div id="id"></div>
{% endcodeblock %}

<p>
  What about html attributes? Well, we specify them similarly to how we would in html:
</p>

{% codeblock lang:ruby %}
meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"
{% endcodeblock %}

<p>
  Which translates to:
</p>

{% codeblock lang:ruby %}
<meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"></meta>
{% endcodeblock %}

<p>
  To specify a doctype:
</p>

{% codeblock lang:ruby %}
doctype html
{% endcodeblock %}

<p>
  Which translates to:
</p>

{% codeblock lang:ruby %}
<!DOCTYPE html>
{% endcodeblock %}

<p>
  Well, all is great, but what about Ruby code? Inserting Ruby code is pretty easy. Code that doesn't return a result, such as if statements or loops, are prefixed with a dash (-).
</p>

{% codeblock lang:ruby %}
- if @products.nil?
  p No results found. Please try again.
{% endcodeblock %}

<p>
  Which is equivalent to:
</p>

{% codeblock lang:ruby %}
if @products.nil?
  <p>
    No results found.  Please try again.
  </p>
end
{% endcodeblock %}

<p>
  Slim will auto close your Ruby code as well, so end isn't needed.
</p>

<p>
  If you have code that returns output, such as the printing of a variable or property, you can use the equal (=) sign.
</p>

{% codeblock lang:ruby %}
= current_user.name
{% endcodeblock %}

<p>
  Which is equivalent to:
</p>

{% codeblock lang:ruby %}
<%= current_user.name %>
{% endcodeblock %}

<p>
  Your strings are Rubyized:
</p>

{% codeblock lang:ruby %}
| Hello #{user.name}
{% endcodeblock %}

<p>
  You can embed javascript as well:
</p>

{% codeblock lang:ruby %}
javascript:
  alert("hello world!")
{% endcodeblock %}

<p>
  So far so good, That's it! See ya! :)
</p>