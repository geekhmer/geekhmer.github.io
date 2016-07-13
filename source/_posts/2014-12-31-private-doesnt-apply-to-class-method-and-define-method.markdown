---
layout: post
title: "Private doesn't apply to class method and define method"
date: 2014-12-31 11:00
comments: true
categories: Ruby
keywords: Ruby private Keyword, Private doesn't apply to class method and define method, private keyword, Private doesn't apply to class method, Private doesn't apply to define method
---

<p>
  <img src="/images/rights_and_wrongsof_ruby.jpg" width="400" alt="Ruby - Check for nil Without Using an Explicit if" />
</p>

<p>
  Ruby's private keyword might do a lot less than you think.
</p>

<p>
  <strong>The "private" does not apply to class methods defined on self</strong>
</p>

<p>
  This does not make anything private:
</p>

{% codeblock lang:ruby %}
class Klass
  private
  def self.print
    'Hello'
  end
end
{% endcodeblock %}

<p>
  You need to use private_class_method instead:
</p>

{% codeblock lang:ruby %}
class Klass
  def self.print
    'Hello'
  end

  private_class_method :print
end
{% endcodeblock %}

<p>
  <strong>The "private" does not apply to define_method</strong>
</p>

<p>
  This does not make anything private:
</p>

{% codeblock lang:ruby %}
class Klass
  private
  define_method :print do
    'Hello'
  end
end
{% endcodeblock %}

<p>
  You need to use private with an argument instead:
</p>

{% codeblock lang:ruby %}
class Klass
  define_method :print do
    'Hello'
  end
  
  private :print
end
{% endcodeblock %}
