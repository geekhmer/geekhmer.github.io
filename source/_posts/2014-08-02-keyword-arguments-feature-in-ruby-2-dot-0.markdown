---
layout: post
title: "Keyword Arguments Feature in Ruby 2.0"
date: 2014-08-02 09:27
comments: true
categories: [Ruby]
keywords: keyword arguments feature in ruby 2.0, keyword arguments ruby, keyword arguments in ruby, keyword argument ruby, keyword argument in ruby
---

<p>
  <img src="/images/rights_and_wrongsof_ruby.jpg" width="400" />
</p>

<p>
  One of the new features of Ruby 2.0 is keyword arguments. keyword arguments make it easier create method that take optional named arguments.
</p>

<p>
  keyword arguments in the method definition must be symbols given in the new-style hash syntax.<br/>
  Assume we have a method:
</p>

{% codeblock lang:ruby %}
def print(message: 'Hello')
  puts message
end

print #=> Hello

print(message: 'Hi') #=> Hi

print({ message: 'Hi' }) #=> Hi
{% endcodeblock %}

<p>
  Ruby 2.0 blocks can also be defined with keyword arguments:
</p>

{% codeblock lang:ruby %}
define_method(:print) do |message: 'Hello'|
  puts message
end

print #=> Hello

print(message: 'Hi') #=> Hi

print({ message: 'Hi' }) #=> Hi
{% endcodeblock %}

<p>
  <strong>Keyword arguments vs Positional arguments</strong><br/>
  Assume we have a method with positional arguments:
</p>

{% codeblock lang:ruby %}
def total(subtotal, tax, discount)
  subtotal + tax - discount
end

total(100, 10, 5) # => 105
{% endcodeblock %}

<p>
  This method does its job, but as a reader of the code using the total method, I have no idea what those arguments mean without looking up the implementation of the method.
</p>

<p>
  By using keyword arguments, we know what the arguments mean without looking up the implementation of the called method:
</p>

{% codeblock lang:ruby %}
def obvious_total(subtotal:, tax:, discount:)
  subtotal + tax - discount
end

obvious_total(subtotal: 100, tax: 10, discount: 5) # => 105

obvious_total({ subtotal: 100, tax: 10, discount: 5 }) # => 105
{% endcodeblock %}

<p>
  Keyword arguments allow us to switch the order of the arguments, without affecting the behavior of the method:
</p>

{% codeblock lang:ruby %}
obvious_total(subtotal: 100, discount: 5, tax: 10) # => 105

obvious_total({ subtotal: 100, discount: 5, tax: 10 }) # => 105
{% endcodeblock %}
