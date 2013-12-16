---
layout: post
title: "What is Closure, Proc and Lambda?"
date: 2013-12-08 17:02
comments: true
categories: [Ruby]
keywords: closure, proc, lambda, what is closure, what is proc, what is lambda
description: What is Closure, Proc and Lambda?
---

<!-- **Content start here** -->
<p>
<strong>1. Closure</strong><br/>
<strong>Closure</strong> is block of code/function/method that has two properties:<br/>
- It can be passed around like an object (to be called later).<br/>
- It remembers the values of all the variables that were in scope when the function was created, then it is able to access those variables when it is called.<br/><br/>
<strong>Proc & Lambda</strong> are closure.
</p>

<p>
<strong>2. Proc (Procedure)</strong><br/>
Proc is a closure and an object that can be bound to a variable and reused.
</p>
{% codeblock default proc lang:ruby %}
p = Proc.new { puts "Hello Rubyist" }
p.call # prints Hello Rubyist
{% endcodeblock %}

{% codeblock proc with parameter lang:ruby %}
p = Proc.new { |a, b| puts "x: #{a}, y: #{b}" }
p.call(1, 2) # prints x: 1, y: 2
{% endcodeblock %}

{% codeblock proc as closure lang:ruby %}
def proc_maker(arg)
  Proc.new { |a| a + arg }
end

p = proc_maker(10)
p.call(25) # return => 35
{% endcodeblock %}

{% codeblock proc as closure lang:ruby %}
def to_do(p)
  p.call
end

p = Proc.new { puts "proc" }
to_do(p) # prints proc
{% endcodeblock %}

{% codeblock proc as closure lang:ruby %}
def to_do(proc1, proc2)
  proc1.call
  proc2.call
end

a = Proc.new { puts "first proc" }
b = Proc.new { puts "second proc" }
to_do(a, b)

# prints
# first proc
# second proc
{% endcodeblock %}

<p>
<strong>3. Lambda</strong><br/>
Lambdas are a more strict form of Proc, something like:<br/>
- Lambda are more strict with regard to argument checking.<br/>
- Lambdas can return a value with the return keyword.
</p>
{% codeblock default lambda lang:ruby %}
l = lambda { puts "Hello Rubyist" }
l.call # prints "Hello Rubyist"
{% endcodeblock %}

{% codeblock lambda with parameter lang:ruby %}
l = lambda { |a, b| puts "x: #{a}, y: #{b}" }
l.call(1, 2) # prints x: 1, y: 2
{% endcodeblock %}

{% codeblock lambda as closure lang:ruby %}
def lambda_maker(arg)
  lambda { |a| a + arg }
end

l = lambda_maker(10)
l.call(25) # return 35
{% endcodeblock %}

{% codeblock lambda as closure lang:ruby %}
def to_do(l)
  l.call
end

l = lambda { puts "lambda" }
to_do(l) # prints lambda
{% endcodeblock %}

{% codeblock lambda as closure lang:ruby %}
def to_do(lambda1, lambda2)
  lambda1.call
  lambda2.call
end

a = lambda { puts "first proc" }
b = lambda { puts "second proc" }
to_do(a,b)

# prints
# first proc
# second proc
{% endcodeblock %}