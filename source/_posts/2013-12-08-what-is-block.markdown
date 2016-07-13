---
layout: post
title: "What is Block?"
date: 2013-12-08 10:29
comments: true
categories: [Ruby]

keywords: block, what is block
description: What is Block?
---

<p>
  A block is just a bit of code that can be executed. 
</p>

{% codeblock example lang:ruby %}
[1, 2, 3].each { |num| puts num }

# print
# 1
# 2
# 3
{% endcodeblock %}

{% codeblock example lang:ruby %}
[1, 2, 3].each do |num|
  puts num
end

# print
# 1
# 2
# 3
{% endcodeblock %}

<p>
  A block is a piece of code that is declared but not run in the place it's written. 
  The idea is to run when to call it.
</p>

{% codeblock default block using yield lang:ruby %}
def to_do
  yield
  yield
end

to_do do
  puts "hello"
end

# print
# hello
# hello
{% endcodeblock %}

{% codeblock block with parameter using yield lang:ruby %}
def to_do
  yield "bunlong"
  yield "sky"
end

to_do do |name|
  puts "hi, #{name}"
end

# print
# hello, bunlong
# hello, sky
{% endcodeblock %}

{% codeblock default block using call lang:ruby %}
def method_name(&block)
  block.call
end

method_name { puts "hello" } # prints hello
{% endcodeblock %}

{% codeblock block with parameter using call lang:ruby %}
def eat(meal, &consume)
  if block_given?
    meal.each {|good| consume.call(good)}
  end
end

puts eat(['cheese', 'steak', 'wine']) {|food| puts 'Hmm, #{food}'}

# prints
# Hmm, cheese
# Hmm, steak
# Hmm, wine
{% endcodeblock %}