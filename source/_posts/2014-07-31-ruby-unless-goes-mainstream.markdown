---
layout: post
title: "Ruby 'unless' Goes Mainstream"
date: 2014-07-31 23:44
comments: true
categories: [Ruby]

keywords: ruby unless, unless, ruby's unless goes mainstream, ruby unless goes mainstream
description: Ruby 'unless' Goes Mainstream
---

<p>
  I don't like or not a fan of Ruby's 'unless' keyword. I mean its nice but it take time to get used to.
</p>

<p>
  Sample 'unless' statements like this:
</p>

{% codeblock lang:ruby %}
puts product.name unless product.name.nil?
{% endcodeblock %}

<p>
  The code statement above is nice and easy. I'd say that in english "Print the product's name unless the product doesn't have a name".
</p>

<p>
  Why do we need unless? Let replace it with something else:
</p>

{% codeblock lang:ruby %}
class Object
  def not_nil?
    !nil?
  end
end
{% endcodeblock %}

<p>
  Then we can code:
</p>

{% codeblock lang:ruby %}
puts product.name if product.name.not_nil?
{% endcodeblock %}

<p>
  So far so good, that's much better, 'unless' never made it into mainstream language.<br/>
  OO scripting languages like Ruby can make new keyword. :)
</p>
