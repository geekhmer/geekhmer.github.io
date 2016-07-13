---
layout: post
title: "Liskov Substitution Principle in Ruby"
date: 2015-03-18 23:00
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Liskov Substitution Principle in Ruby
---

<p>
  <img src="/images/rights_and_wrongsof_ruby.jpg" width="400" alt="Liskov Substitution Principle in Ruby" />
</p>

<p>
  Assume that we already had the code:
</p>

{% codeblock lang:ruby %}
class Animal
  def walk
     do_some_walkin
  end
end

class Cat < Animal
  def run
    run_like_a_cat
  end
end
{% endcodeblock %}

<p>
  This principle applies only to inheritance. In order to comply with the Liskov Substitution Principle, Subtypes must be substitutable for their base types.
</p>

<p>
  Well, so they must have the same interface. Since ruby does not have abstract methods, we can do it like this:
</p>

{% codeblock lang:ruby %}
class Animal
  def walk
    do_some_walkin
  end

  def run
    raise NotImplementedError
  end
end

class Cat < Animal
  def run
    run_like_a_cat
  end
end
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>