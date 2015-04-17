---
layout: post
title: "Interface Segregation Principle in Ruby"
date: 2015-03-18 23:41
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Interface Segregation Principle in Ruby
---

<p>
  <img src="/images/rights_and_wrongsof_ruby.jpg" width="400" alt="Interface Segregation Principle" />
</p>

<p>
  Simply Interface Segregation Principle states that: When a client depends upon a class that contains interfaces that the client does not use, but that other clients do use, then that client will be affected by the changes that those other clients force upon the class.
</p>

<p>
  If you have a class that has two clients (objects using it):
</p>

{% codeblock lang:ruby %}
class Car
  def open
  end

  def start_engine
  end

  def change_engine
  end
end

class Driver
  def drive
    @car.open
    @car.start_engine
  end
end

class Mechanic
  def do_stuff
    @car.change_engine
  end
end
{% endcodeblock %}

<p>
  As you can see, our Car class has an interface that's used partially by both the Driver and the Mechanic. We can improve our interface like this:
</p>

{% codeblock lang:ruby %}
class Car
  def open
  end

  def start_engine
  end
end

class CarInternals
  def change_engine
  end
end

class Driver
  def drive
    @car.open
    @car.start_engine
  end
end

class Mechanic
  def do_stuff
    @car_internals.change_engine
  end
end
{% endcodeblock %}

<p>
  So far so good, splitting the interface into two, we can comply to the Interface Segregation Principle. That's it!!! See ya!!! :)
</p>
