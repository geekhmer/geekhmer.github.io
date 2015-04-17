---
layout: post
title: "Ruby DSL (Domain-Specific Language)"
date: 2015-03-14 23:47
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Ruby DSL (Domain-Specific Language), Ruby DSL
---

<p>
  <img src="/images/rights_and_wrongsof_ruby.jpg" width="400" />
</p>

<h3>What is a DSL?</h3>

<p>
  A domain-specific language (DSL) is a computer language specialized to a particular application domain. This is in contrast to a general-purpose language (GPL), which is broadly applicable across domains, and lacks specialized features for a particular domain.
</p>

<p>
  A DSL is like a mini language inside of a language. It's a specific syntax for describing a domain/area of your application. Think of a DSL like a mini game within a game... like when you were raising/racing Chocobos in Final Fantasy 7. A DSL in Ruby is still written in Ruby, it just provides a nicer interface and language for performing common tasks.
</p>

<h3>DSL you have seen</h3>

<p>
  You may not have realized it, but if you write Rails applications you most likely work with a variety of DSLs every day. Below are a few examples of some commons ones.
</p>

<p>
  <strong>Rails Routing</strong>
</p>

{% codeblock lang:ruby %}
Rails.application.routes.draw do
  root 'pages#home'
  resources :pages, only: [:index, :show]
end
{% endcodeblock %}

<p>
  <strong>Rspec</strong>
</p>

{% codeblock lang:ruby %}
describe Array do
  describe "includes_subset?" do
    it "finds subsets" do
      a = [1,2,3,4,5]
      b = [1,2]
      expect(a.includes_subset?(b)).to eq(true)
    end
  end
end
{% endcodeblock %}

<p>
  <strong>Capistrano</strong>
</p>

{% codeblock lang:ruby %}
role :app, %w{deploy@localhost}

server 'localhost',
  user: 'deploy',
  roles: %w{app},
  ssh_options: {
    keys: %w(/home/deploy/.ssh/id_rsa),
    auth_methods: 'publickey'
  }
{% endcodeblock %}

<h3>Blocks are Key to DSLs</h3>
<p>
  Knowing how blocks work is crucial to understanding how DSLs work. I think of a block in Ruby as basically just a chunk of code that you can pass around to other methods. If you're used to writing Javascript you're definitely comfortable passing around functions, and blocks in Ruby are fairly similar to that.
</p>

{% codeblock lang:ruby %}
double = ->(num) {
  num * 2
}
puts double.call(2)
# 4
{% endcodeblock %}

<p>
  <strong>Passing Blocks</strong><br/>
  You've most likely used blocks before when calling the each method on an Array, or the collect method.
</p>

{% codeblock lang:ruby %}
[1, 2, 3].each do |item|
  puts item * 2
end
# 2, 4, 6
{% endcodeblock %}

<p>
  What you are doing is passing the block of code between do and end to the each method of the Array class.
</p>

<p>
  <strong>Receiving Blocks</strong><br/>
  On the recipients side, there are a couple ways to receive the block that was passed. The first way is via the yield keyword in Ruby. yield will call the block of code that was passed to the method. You can even pass variables to the block of code.
</p>

{% codeblock lang:ruby %}
class Array
  def each
    array_size = self.size - 1
    for i in 0..array_size
      yield self[i]
    end
  end
end
{% endcodeblock %}

<p>
  There is an alternate way of receiving a block of code that is called a named-block. This essentially allows you to save the block of code to a variable, which gives you the flexibility to pass the block on to another method. If you wish to execute the code inside of the block, you can do so via the call method on it.
</p>

{% codeblock lang:ruby %}
class Array
  def each(&block)
    array_size = self.size - 1
    for i in 0..array_size
      block.call self[i]
    end
  end
end
{% endcodeblock %}

<p>
  <strong>Block Context</strong><br/>
  Normally a block has the context of the place it was instantiated. It is possible to change this, to be the place where it is being called, but to do this instead of doing the usual yield or block.call(), you call it via the instance_eval(&block) method.
</p>

{% codeblock lang:ruby %}
class Dummy
  def initialize
    @name = "Inside"
  end

  def print_name(&block)
    # Has outside context
    block.call
    # Has inside context
    instance_eval(&block)
  end
end

@name = "Outside"

Dummy.new.print_name do
  puts @name
end
{% endcodeblock %}

<p>
  <strong>Create your own DSL</strong><br/>
  Create a Ruby DSL to generate HTML. The output was to look like the following, and below is how the DSL should be called. To each HTML element you can pass text, options (html attributes), and a block. All are optional.
</p>

{% codeblock lang:ruby %}
<html>
  <body>
    <div id="container">
      <ul class="pretty">
        <li class="active">Item 1</li>
        <li>Item 2</li>
      </ul>
    </div>
  </body>
</html>
{% endcodeblock %}

{% codeblock lang:ruby %}
output = FancyMarkup.new.document do
  body do
    div id: "container" do
      ul class: "pretty" do
        li "Item 1", class: :active
        li "Item 2"
      end
    end
  end
end
{% endcodeblock %}

<p>
  The solution I eventually came up with (after some serious refactoring) is below. I used some Ruby meta-programming by not actually defining methods for each HTML element, but rather utilizing method_missing as a catch-all, which then passes the element name, the args, and the block on to the tag method, which does the heavy lifting of generating the HTML.
</p>

<p>
  One other thing I utilized here was that you can also check if a block was actually passed to the method by using the block_given? method.
</p>

{% codeblock lang:ruby %}
class FancyMarkup

  attr_accessor :indents, :html

  def initialize
    @indents = 0
    @html = ""
  end

  # Catch-all method to avoid creating methods
  # for each HTML element.
  def method_missing(m, *args, &block)
    tag(m, args, &block)
  end

  # The first method called when creating an
  # HTML document.
  def document(*args, &block)
    tag(:html, args, &block)
  end

  private

  # Create the HTML tag
  # @param (String|Symbol) HTML tag (ul, li, strong, etc...)
  # @param (Array) Can contain a String of text or a Hash of attributes
  # @param (Block) An optional block which will further nest HTML
  def tag(html_tag, args, &block)
    content = find_content(args)
    options = html_options(find_options(args))

    html << "\n#{indent}<#{html_tag}#{options}>#{content}"
    if block_given?
      @indents += 1
      instance_eval(&block)
      @indents -= 1
      html << "\n#{indent}"
    end
    html << "</#{html_tag}>"
  end

  # Searching the tag arguments, find the text/context element.
  def find_content(args)
    args.select{|arg| arg.is_a? String}.first || nil
  end

  # Searching the tag arguments, find the hash/attributes element
  def find_options(args)
    args.select{|arg| arg.is_a? Hash}.first || {}
  end

  # Indent output number of spaces
  def indent
    "  " * indents
  end

  # Build html options string from Hash
  def html_options(options)
    options.collect{|key, value|
      value = value.to_s.gsub('"', '\"')
      " #{key}=\"#{value}\""
    }.join("")
  end
end
{% endcodeblock %}

<p>
  So far so good, I've realized how important blocks are to Ruby. Learning them well and understanding how DSLs work will improve the Ruby code that I write. That's it!!! See ya!!!
</p>