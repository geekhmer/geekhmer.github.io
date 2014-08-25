---
layout: post
title: "include_vs_extend"
date: 2013-11-30 10:56
comments: true
categories: [Ruby, Ruby Metaprogramming]

keywords: include, extend, include_vs_extend, ruby
description: include_vs_extend
---

<p>
  <strong>include</strong><br/>
  include use for adding methods to an instance of a class.
</p>

<p>
  <strong>example</strong>
</p>

{% codeblock example lang:ruby %}
module Foo
  def foo
    p 'Hi foo'
  end
end

class Bar
  include Foo
end

bar = Bar.new
bar.foo # => Hi foo

Bar.foo # => NoMethodError: undefined method ‘foo’ for Bar:Class
{% endcodeblock %}

<p>
  <strong>extend</strong><br/>
  extend use for adding class methods.
</p>

<p>
  <strong>example</strong>
</p>

{% codeblock example lang:ruby %}
module Foo
  def foo
    p 'Hi foo'
  end
end

class Baz
  extend Foo
end

Bar.foo # => Hi foo

bar = Bar.new
bar.foo # => NoMethodError: undefined method ‘foo’ for #<Baz:0x1e708>
{% endcodeblock %}

<p>
  <strong>Using include to append both class and instance methods</strong><br/>
  You will see in Ruby is to use include to append both class and instance methods.<br/>
  The reason for this is that include has a self.included hook you can use to modify 
  the class that is including a module.
</p>

<p>
  <strong>example</strong>
</p>

{% codeblock example lang:ruby %}
module Foo
  def self.included(base)
    base.extend(ClassMethods)
  end
  
  module ClassMethods
    def bar
      p 'class method'
    end
  end
  
  def foo
    p 'instance method'
  end
end

class Bar
  include Foo
end

Bar.bar # => class method
Bar.foo # => NoMethodError: undefined method ‘foo’ for Baz:Class

Bar.new.foo # => instance method
Bar.new.bar # => NoMethodError: undefined method ‘bar’ for #<Baz:0x1e3d4>
{% endcodeblock %}