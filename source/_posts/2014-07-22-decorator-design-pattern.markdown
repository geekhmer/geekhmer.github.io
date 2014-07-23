---
layout: post
title: "Decorator Design Pattern"
date: 2014-07-22 20:10
comments: true
categories: [Ruby, Design Pattern]
keywords: ruby,design pattern,decorator design pattern
---

<p>
  In the object-oriented world, simple applications usually require small classes with static behaviors. Adding, modifying, and sharing those behaviors can be achieved by mixing in modules or inheriting from other classes at compile time.<br/>

</p>

<p>
  However, more complex applications might require a particular instance of a class to gain additional functionality at runtime. To modify the behavior of an object dynamically, we can utilize the decorator design pattern.<br/>
</p>

<p>
  <strong>When to Decorate</strong><br/>
  Decoration can be used to add behavior to any individual object without affecting the behavior of other objects of the same class. Essentially the existing object is being wrapped with additional functionality. 
</p>

<p>
  <strong>Some practical problems that can be solved by decoration are</strong><br/>
  - applying one or more UI elements to a specific UI widget at runtime.<br/>
  - saving an ActiveRecord model in various ways based on conditionals in a Rails controller.<br/>
  - adding additional information to data streams by pre/appending with additional stream data.<br/>
</p>

<p>
  <strong>Implementations of Decorators in Ruby</strong><br/>
  There are several ways to implement the decorator pattern in Ruby, but I cover my 3 favorite ways:<br/>
  - Class + Method Missing decorator<br/>
  - Module + Extend + Super decorator<br/>
  - Plain Old Ruby Object decorator<br/>
</p>

<p>
  <strong>Class + Method Missing Decorator</strong><br/>
  The benefits of this implementation are:<br/>
  - can be wrapped infinitely using Ruby instantiation.<br/>
  - delegates through all decorators.<br/>
  - can use the same decorator more than once on the same component.<br/>
  - transparently uses component's original interface.<br/>
</p>

<p>
  The drawbacks of this implementation are:<br/>
  - uses method_missing.<br/>
  - the class of the decorated object is the decorator.<br/>
</p>

<p>
  Sample example
</p>

{% codeblock sample.rb lang:ruby %}
module Decorator
  def initialize(decorated)
    @decorated = decorated
  end

  def method_missing(meth, *args)
    if @decorated.respond_to?(meth)
      @decorated.send(meth, *args)
    else
      super
    end
  end

  def respond_to?(meth)
    @decorated.respond_to?(meth)
  end
end

class Coffee
  def cost
    2
  end
end

class Milk
  include Decorator

  def cost
    @decorated.cost + 0.4
  end
end

class Whip
  include Decorator

  def cost 
    @decorated.cost + 0.2
  end
end

class Sprinkles
  include Decorator

  def cost
    @decorated.cost + 0.3
  end
end

Whip.new(Coffee.new).cost #=> 2.2
Sprinkles.new(Whip.new(Milk.new(Coffee.new))).cost #=> 2.9

# Factory class
class CoffeeFactory
  def self.cappuccino
    Sprinkles.new(Cream.new(Milk.new(Coffee.new)))
  end
end

CoffeeFactory.cappucino.kind_of? Coffee #=> true
{% endcodeblock %}

<p>
  <strong>Module + Extend + Super Decorator</strong><br/>
  The benefits of this implementation are:<br/>
  - it delegates through all decorators.<br/>
  - has all of the original interface because it is the original object.<br/>
</p>

<p>
  The drawbacks of this implementation are:<br/>
  - can not use the same decorator more than once on the same object.<br/>
  - difficult to tell which decorator added the functionality.<br/>
</p>

<p>
  Sample example
</p>

{% codeblock sample.rb lang:ruby %}
class Coffee
  def cost
    2
  end
end

module Milk
  def cost
    super + 0.4
  end
end

module Sugar
  def cost
    super + 0.2
  end
end

coffee = Coffee.new
coffee.extend(Milk)
coffee.cost   #=> 2.4
coffee.extend(Sugar)
coffee.cost   #=> 2.6
{% endcodeblock %}

<p>
  <strong>Plain Old Ruby Object Decorator</strong><br/>
  The benefits of this implementation are:<br/>
  - can be wrapped infinitely using Ruby instantiation.<br/>
  - delegates through all decorators.<br/>
  - can use same decorator more than once on component.<br/>
</p>

<p>
  The drawbacks of this implementation are:<br/>
  - cannot transparently use component's original interface.<br/>
</p>

<p>
  Sample example
</p>

{% codeblock sample.rb lang:ruby %}
class Coffee
  def cost
    2
  end

  def origin
    "Cambodia"
  end
end

class Sugar
  def initialize(component)
    @component = component
  end

  def cost
    @component.cost + 0.2
  end
end

class Milk
  def initialize(component)
    @component = component
  end

  def cost
    @component.cost + 0.4
  end
end

coffee = Coffee.new
Sugar.new(Milk.new(coffee)).cost  #=> 2.6
Sugar.new(Sugar.new(coffee)).cost #=> 2.4
Sugar.new(Milk.new(coffee)).class #=> Sugar
Milk.new(coffee).origin           #=> NoMethodError
{% endcodeblock %}

<p>
  So far so good, Let decorate your way with Decorator Design Pattern. :)
</p>