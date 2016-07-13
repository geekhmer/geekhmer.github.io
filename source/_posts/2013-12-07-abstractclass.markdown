---
layout: post
title: "AbstractClass"
date: 2013-12-07 22:31
comments: true
categories: [Ruby, Ruby Metaprogramming]

keywords: abstractclass, abstract class, ruby metaprogramming
description: AbstractClass
---

{% codeblock example lang:ruby %}
class AbstractKlass  
  def method_name  
    puts "#{hello} #{name}"  
  end  
end  
  
class ConcreteKlass < AbstractKlass  
  def hello; "Hello"; end
  def name; "Rubyist"; end

  # def hello
  #   "Hello"
  # end

  # def name
  #   "Rubyist"
  # end
end

ConcreteKlass.new.method_name # => "Hello Rubyist"  
{% endcodeblock %}
