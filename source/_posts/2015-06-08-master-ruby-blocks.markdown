---
layout: post
title: "Master Ruby Blocks"
date: 2015-06-08 20:29
comments: true
categories: [Ruby]
keywords: Master Ruby Blocks, Ruby Blocks
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="Master Ruby Blocks" />
</p>

<p>
  Blocks are one of the most powerful and often overlooked feature of ruby. I must confess that it took me a while to figure out how ruby blocks work and how they can be useful in practice.
</p>

<p>
  There is something about <code>yield</code> that makes blocks very hard to understand at first. I'm going to talk about some of the concepts and provide a few examples so by the end of this post you'll have a solid understanding of ruby blocks.
</p>

<p>
  <strong>The basics: What are ruby blocks?</strong><br/>
  1. What are ruby blocks?<br/>
  2. How yield works?<br/>
  3. What does <code>&block</code> mean?<br/>
  4. Return value<br/>
  5. How does <code>.map(&:something)</code> work?<br/>
  6. Iterators and how to build one yourself<br/>
  7. Initialize objects with default values using blocks<br/>
  8. Ruby block examples
</p>

<p>
  <strong>What are ruby blocks?</strong><br/>
  A block is basically just code that you put inside <code>do</code> and <code>end</code>. That's it. "But where's the magic?" you might ask. We'll get there in just a minute but first things first.
</p>

<p>
  You can write the block in two ways:<br/>
  1. Multi-line, between <code>do</code> and <code>end</code><br/>
  2. Inline, between <code>{</code> and <code>}</code>
</p>

<p>
  Both versions will do the exact same thing so it's up to you which one you choose. As a general style-guide, it's better to use the multi-line version if your code has more than one line, just to make it easier to read.
</p>

<p>
  Here's a basic example of a multi-line block:
</p>

{% codeblock lang:ruby %}
[1, 2, 3].each do |n|
  puts "Number #{n}"
end
{% endcodeblock %}

<p>
  It's called a multi-line block because it's not inline, not because it's got more than one line of code (which is not the case here). The same example can be written with an inline block:
</p>

{% codeblock lang:ruby %}
[1, 2, 3].each {|n| puts "Number #{n}"}
{% endcodeblock %}

<p>
  Both versions will print numbers 1, 2 and 3 in order. The little n letter you see between the pipes (<code>|n|</code>) is called a block parameter and it's value in this case is going to be each of the numbers in turn, in the order they are listed inside the array. So for the first iteration, the value of n will be 1, then for the second iteration, the value will be 2, and then 3.
</p>

{% codeblock lang:ruby %}
Number 1
Number 2
Number 3
 => nil
{% endcodeblock %}

<p>
  <strong>How yield works?</strong><br/>
  Here's the bad wolf. This guy is responsible for all the confusion and magic around ruby blocks. I think most of the confusion comes from the way it calls the block and how it's passing parameters to it. We'll be looking at both scenarios in this section.
</p>

{% codeblock lang:ruby %}
def my_method
  puts "reached the top"
  yield
  puts "reached the bottom"
end

my_method do
  puts "reached yield"
end
{% endcodeblock %}

{% codeblock lang:ruby %}
reached the top
reached yield
reached the bottom
 => nil
{% endcodeblock %}

<p>
  So basically when the execution of <code>my_method</code> reaches the line with the call to <code>yield</code>, the code inside the block gets executed. Then, when the code inside the block finishes, the execution of <code>my_method</code> continues.
</p>

<p>
  <img src="/images/ruby_block_flow.png" alt="Ruby Block Flow" />
</p>

<p>
  <strong>Passing blocks to methods</strong><br/>
  A method doesn't need to specify the block in it's signature in order to receive a block parameter. You can just pass a block to any function but unless that function calls <code>yield</code>, the block won't get executed.
</p>

<p>
  On the other hand, if you do call <code>yield</code> in your method, then the block parameter becomes mandatory and the method will raise an exception if it doesn't receive a block.
</p>

<p>
  If you want to make the block an optional parameter, you can use the <code>block_given?</code> method which will return either true or false depending on if a block was passed in to the method or not.
</p>

<p>
  <strong>Yield takes parameters too</strong><br/>
  Any parameter passed to <code>yield</code> will serve as a parameter to the block. So when the block runs, it can use the parameters passed in from the original method. Those parameters can be variables local to the method in which <code>yield</code> lives in.
</p>

<p>
  The order of the arguments is important because the order you use to pass in the parameters is the order in which the block receives them.
</p>

<p>
  <img src="/images/ruby_block_arguments.png" alt="Ruby Block Arguments" />
</p>

<p>
  One thing to note here is that the parameters inside the block are local to the block (unlike those passed in from the method to the block).
</p>

<p>
  <strong>What does &block (ampersand parameter) mean?</strong><br/>
  You've probably seen this <code>&block</code> all over the place in ruby code. It's how you can pass a reference to the block (instead of a local variable) to a method. In fact, ruby allows you to pass any object to a method as if it were a block. The method will try to use the passed in object if it's already a block but if it's not a block it will call to_proc on it in an attempt to convert it to a block.
</p>

<p>
  Also note that the <code>block</code> part (without the ampersand) is just a name for the reference, you can use whatever name you like if it makes more sense to you.
</p>

{% codeblock lang:ruby %}
def my_method(&block)
  puts block
  block.call
end

my_method { puts "Hello!" }
{% endcodeblock %}

{% codeblock lang:ruby %}
#<Proc:0x0000010124e5a8@tmp/example.rb:6>
Hello!
{% endcodeblock %}

<p>
  <strong>Return value</strong><br/>
  <code>yield</code> returns the last evaluated expression (from inside the block). So in other words, the value that <code>yield</code> returns is the value the block returns.
</p>

{% codeblock lang:ruby %}
def my_method
  value = yield
  puts "value is: #{value}"
end

my_method do
  2
end
{% endcodeblock %}

{% codeblock lang:ruby %}
value is 2
  => nil
{% endcodeblock %}

<p>
  <strong>How does .map(&:something) work?</strong><br/>
  You've probably used shortcuts like <code>.map(&:capitalize)</code> a lot, especially if you've done any Rails coding. It's a very clean shortcut to <code>.map { |title| title.capitalize }</code>.
</p>

<p>
  But how does it really work?
</p>

<p>
  It turns out that the Symbol class implements the to_proc method which will unwrap the short version into it's longer variant. Nice right?
</p>

<p>
  <strong>Iterators and how to build one yourself</strong><br/>
  You can call yield as many times as you want inside a method. That's basically how iterators work. Calling <code>yield</code> for each of the elements in the array mimics the behavior of the built in ruby iterators.
</p>

<p>
  Let's see how we can write a method similar to the map method in ruby.
</p>

{% codeblock lang:ruby %}
def my_map(array)
  new_array = []

  for element in array
    new_array.push yield element
  end

  new_array
end

my_map([1, 2, 3]) do |number|
  number * 2
end
{% endcodeblock %}

{% codeblock lang:ruby %}
2
4
6
{% endcodeblock %}

<p>
  <strong>Initialize objects with default values</strong><br/>
  A cool pattern we can use with ruby blocks is to initialize an object with default values. You've probably seen this pattern if you've ever ventured into a .gemspec file from any ruby gem.
</p>

<p>
  The way it works is, you have an initializer that calls <code>yield(self)</code>. In the context of the <code>initialize</code> method, <code>self</code> is the object being initialized.
</p>

{% codeblock lang:ruby %}
class Car
  attr_accessor :color, :doors

  def initialize
    yield(self)
  end
end

car = Car.new do |c|
  c.color = "Red"
  c.doors = 4
end

puts "My car's color is #{car.color} and it's got #{car.doors} doors."
{% endcodeblock %}

{% codeblock lang:ruby %}
My car's color is Red and it's got 4 doors.
{% endcodeblock %}

<p>
  <strong>Ruby blocks examples</strong><br/>
  Examples are all the rage these days so let's try to find a few interesting ways of using blocks in real world (or as close to real world as possible) scenarios.
</p>

<p>
  <strong>Wrap text in html tags</strong><br/>
  Blocks are the perfect candidate whenever you need to wrap a chunk of dynamic code within some static code. So for example if you want to generate an html tag for some text. The text is the dynamic part (cause you never know what you'll want to wrap) and the tags are the static part, they never change.
</p>

{% codeblock lang:ruby %}
def wrap_in_h1
  "<h1>#{yield}</h1>"
end

wrap_in_h1 { "Here's my heading" }

# => "<h1>Here's my heading</h1>"

wrap_in_h1 { "Ha" * 3 }

# => "<h1>HaHaHa</h1>"
{% endcodeblock %}

<p>
  Note that the power of using blocks over methods is when you need to reuse some of the behavior but do something slightly different with it. So let's say we have a string we want to wrap inside html tags and then do something different with it.
</p>

{% codeblock lang:ruby %}
def wrap_in_tags(tag, text)
  html = "<#{tag}>#{text}</#{tag}>"
  yield html
end

wrap_in_tags("title", "Hello") { |html| Mailer.send(html) }
wrap_in_tags("title", "Hello") { |html| Page.create(:body => html) }
{% endcodeblock %}

<p>
  In the first case we're sending the {% raw %}<title>Hello</title>{% endraw %} string via email and in the second case we're creating a Page record. Both cases use the same method but they do different things.
</p>

<p>
  <strong>Take a note</strong><br/>
  Let's say we want to build a way to quickly store ideas into a database table. For that to work we want to pass in the note and have the method deal with the database connections. Ideally we'd like to call <code>Note.create { "Nice day today" }</code> and not worry about opening and closing database connections. So let's do this.
</p>

{% codeblock lang:ruby %}
class Note
  attr_accessor :note

  def initialize(note=nil)
    @note = note
    puts "@note is #{@note}"
  end

  def self.create
    self.connect
    note = new(yield)
    note.write
    self.disconnect
  end

  def write
    puts "Writing \"#{@note}\" to the database."
  end

  private
  def self.connect
    puts "Connecting to the database..."
  end

  def self.disconnect
    puts "Disconnecting from the database..."
  end
end

Note.create { "Foo" }
{% endcodeblock %}

{% codeblock lang:ruby %}
Connecting to the database...
@note is Foo
Writing "Foo" to the database.
Disconnecting from the database...
{% endcodeblock %}

<p>
  The implementation details of connecting, writing and disconnecting to and from the database were left out since they're out of the scope of this article.
</p>

<p>
  <strong>Find divisible elements of an array</strong><br/>
  It seems like I'm getting further and further away from "the real world scenario" but anyways, I'm gonna shoot one last example. So let's say you want to get every element of an array that is divisible by 3 (or any number you choose), how would you do that with ruby blocks?
</p>

{% codeblock lang:ruby %}
class Fixnum
  def to_proc
    Proc.new do |obj, *args|
      obj % self == 0
    end
  end
end

numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10].select(&3)
puts numbers
{% endcodeblock %}

{% codeblock lang:ruby %}
3
6
9
{% endcodeblock %}

<p>
  You can think of blocks as simply a chunk of code, and yield allows you to inject that code at some place into a method. That means you can have one method work in different ways, you don't have to write multiple methods (you can reuse one method to do different things).
</p>
