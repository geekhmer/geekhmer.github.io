---
layout: post
title: "Ruby - Check for nil Without Using an Explicit if"
date: 2014-12-02 00:06
comments: true
categories: [Ruby]
keywords: Check for nil Without Using an Explicit if, Ruby, Ruby on Rails, Rails 4, Ruby on Rails 4
---

<p>
  <img src="/images/rights_and_wrongsof_ruby.jpg" width="400" alt="Ruby - Check for nil Without Using an Explicit if" />
</p>

<p>
  All checks for nil are a condition, but Ruby provides many ways to check for
  nil without using an explicit if. Watch out for nil conditional checks behind other syntax:
</p>

<p>
  <strong>Explicit if with nil?</strong>
</p>

{% codeblock lang:ruby %}
if listing.nil?
  nil
else
  listing.name
end
{% endcodeblock %}

<p>
  Using other syntax:
</p>

<p>
  <strong>Implicit nil check through truthy conditional</strong>
</p>

{% codeblock lang:ruby %}
if listing
  listing.name
end
{% endcodeblock %}

<p>
  <strong>Relies on nil being falsey</strong>
</p>

{% codeblock lang:ruby %}
listing && listing.name
{% endcodeblock %}

<p>
  <strong>Call to try</strong>
</p>

{% codeblock lang:ruby %}
listing.try(:name)
{% endcodeblock %}
