---
layout: post
title: "Ruby on Rails Model Change Tracking with ActiveModel::Dirty"
date: 2015-02-19 09:09
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Ruby on Rails Model Change Tracking with ActiveModel::Dirty, Rails Model Change Tracking with ActiveModel::Dirty
---

<p>
  <img src="/images/move_to_rails.png" alt="Ruby on Rails Model Change Tracking with ActiveModel::Dirty" />
</p>

<p>
  ActiveModel::Dirty is a library that is built into Ruby on Rails that allows you to quickly and easily track what attributes on a model have changed. This article will show you how to use it.
</p>

<p>
  <strong>Checking Whenever the Model Has Changed</strong><br/>
  Checking whether the model has changed using the <code>changed?</code> method:
</p>

{% codeblock lang:ruby %}
product.changed?  # => false
{% endcodeblock %}

<p>
  If you change an attribute on the model, the <code>changed?</code> method will returns true:
</p>

{% codeblock lang:ruby %}
product.name = "Mac Pro"
product.changed? # => true
{% endcodeblock %}

<p>
  <strong>Tracking the Change</strong><br/>
  You can track what the change by using the <code>attr_change</code> method:
</p>

{% codeblock lang:ruby %}
product.name_change # => ["Mac Pro", "IBM ThinkPad"]
{% endcodeblock %}

<p>
  You can get a list of all of the changed attributes on the model as well:
</p>

{% codeblock lang:ruby %}
product.changed # => ["name"]
{% endcodeblock %}

<p>
  You can get the original value using the <code>attr_was</code> method as well:
</p>

{% codeblock lang:ruby %}
product.name_was #=> "IBM ThinkPad"
{% endcodeblock %}

<p>
  You can view a list of all of the original values using the <code>changed_attributes</code> method as well:
</p>

{% codeblock lang:ruby %}
product.changed_attributes # => {"name" => "IBM ThinkPad"}
{% endcodeblock %}

<p>
  You can list all of the changes on the model using the <code>changes</code> method as well:
</p>

{% codeblock lang:ruby %}
product.changes # => {"name" => ["IBM ThinkPad", "Mac Pro"]}
{% endcodeblock %}


<p>
  You can view changes that were made even after the model was saved using the <code>previous_changes</code> method as well:
</p>

{% codeblock lang:ruby %}
<pre class="prettyprint">
  product.save
  product.changes # => {}
  product.previous_changes # => {"name" => ["IBM ThinkPad", "Mac Pro"]}
</pre>
{% endcodeblock %}

<p>
  You can view changes that were made even after the model was saved using the <code>previous_changes</code> method as well:
</p>

{% codeblock lang:ruby %}
<pre class="prettyprint">
  product.save
  product.changes # => {}
  product.previous_changes # => {"name" => ["IBM ThinkPad", "Mac Pro"]}
</pre>
{% endcodeblock %}

<p>
  You can reset the model using the reload! method as well:
</p>

{% codeblock lang:ruby %}
product.reload!
product.previous_changes # => {}
{% endcodeblock %}

<p>
  So far so good, for more detail <a href="http://api.rubyonrails.org/classes/ActiveModel/Dirty.html" target="_blank">Ruby on Rails API</a>. That's it! See ya!
</p>