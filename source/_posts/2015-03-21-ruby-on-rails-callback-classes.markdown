---
layout: post
title: "Ruby on Rails Callback Classes"
date: 2015-03-21 14:51
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Ruby on Rails Callback Classes
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails Callback Classes" />
</p>

<p>
  If you want to reuse callback code for more than one object that Rails provides a way to write callback classes. All you have to do is pass a given callback queue an object that responds to the name of the callback and takes the model object as a parameter.
</p>

{% codeblock lang:ruby %}
class MarkDeleted
  def self.before_destroy(model)
    model.update_attribute(:deleted_at, Time.current)
    false
  end
end
{% endcodeblock %}

<p>
  The behavior of MarkDeleted is stateless, so I added the callback as a class method.
</p>

{% codeblock lang:ruby %}
class Account < ActiveRecord::Base
  before_destroy MarkDeleted
end
{% endcodeblock %}

{% codeblock lang:ruby %}
class Invoice < ActiveRecord::Base
  before_destroy MarkDeleted
end
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>