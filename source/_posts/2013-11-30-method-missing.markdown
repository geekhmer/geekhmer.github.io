---
layout: post
title: "method_missing"
date: 2013-11-30 11:15
comments: true
categories: [Ruby, Ruby Metaprogramming]
keywords: method_missing, method missing, ruby
description: method_missing
---

<!-- **Content start here** -->
<p>
  method_missing use for creating dynamic methods.
</p>
<p>
  <strong>example</strong><br/>
</p>
{% codeblock example lang:ruby %}
class Klass
  def method_missing(method_name, *args, &block)
    if method_name.to_s =~ /^find_by_(.+)$/
      run_find_by_method($1, *args, &block)
    else
      super
    end
  end

  def run_find_by_method(attrs, *args, &block)
    attrs = attrs.split('_and_')

    attrs_with_args = [attrs, args].transpose

    conditions = Hash[attrs_with_args]

    str = ''
    conditions.each { |key, value|
      str += "#{key} = '#{value}' and "
    }

    p "select * from tbl where #{str[0..-6]}"
  end
end

k = Klass.new
k.find_by_name("Bunlong")
k.find_by_name_and_email("Bunlong", "bunlong.van@gmail.com")
{% endcodeblock %}