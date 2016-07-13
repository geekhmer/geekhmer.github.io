---
layout: post
title: "Use Memoization to Speed Up Your Code in Ruby/Rails"
date: 2014-08-11 00:28
comments: true
categories: [Ruby on Rails, Ruby]

keywords: Use Memoization to Speed Up Your Code in Ruby/Rails, Memoization Ruby, Ruby Memoization, Memoization Rails, Rails Memoization, Ruby on Rails Memoization, Memoization Ruby on Rails
description: Use Memoization to Speed Up Your Code in Ruby/Rails
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" />
</p>

<p>
  Have you ever heard Memoization? In this article you will get an introduction of Memoization. You will learn what it is, how you can use it to speed up your code.
</p>

<p>
  <strong>What is Memoization?</strong><br/>
  Memoization is the process of storing a computed value to avoid duplicated work by future calls.
</p>

<p>
  <strong>What are Memoization used for?</strong><br/>
  - Perform some work<br/>
  - Store the work result<br/>
  - Use stored results in future calls
</p>

<p>
  <strong>Using</strong><br/>
  In Ruby the most common pattern for using Memoization is the conditional assignment operator: <code>||=</code>.
</p>

<p>
  <strong>Example</strong><br/>
  In Rails if you've ever worked with a user login system, you really family with the pattern of loading the <code>current_user</code> in the <code>application_controller.rb</code>:
</p>

{% codeblock application_controller.rb lang:ruby %}
def current_user
  User.find(session[:user_id]) if session[:user_id]
end
{% endcodeblock %}

<p>
  Within each request in a Rails application you will usually see multiple calls to <code>current_user</code> that means <code>User.find</code> method is run multiple times.<br/>
</p>

<p>
  In this case, we know the problem is because there are multiple calls to <code>current_user</code> occurring. Let's fix this code by introducing Memoization into the <code>current_user</code> method and storing the result of <code>User.find</code> method by using conditional assignment to an instance variable:
</p>

{% codeblock application_controller.rb lang:ruby %}
def current_user
  @current_user ||= User.find(session[:user_id]) if session[:user_id]
end
{% endcodeblock %}

<p>
  Well, there are no more calls to rebuild the <code>User</code> object each time <code>current_user</code> method is called.
</p>

<p>
  <strong>When should you memoize?</strong><br/>
  - When you've got duplicated database<br/>
  - When you've got expensive calculations<br/>
  - When you've got repeated calculations that don't change<br/>
</p>

<p>
  <strong>When shouldn't you memoize</strong><br/>
  - Memoization shouldn't be used with methods that take parameters:
</p>

{% codeblock lang:ruby %}
#incorrect
def full_name
  @full_name ||= "#{first_name} #{last_name}"
end

puts full_name('Bunlong', 'Van') #=> "Bunlong Van"

puts full_name('Ryan', 'Van') #=> "Bunlong Van"
{% endcodeblock %}

<p>
  - Memoization shouldn't be used with methods that use instance variables:
</p>

{% codeblock lang:ruby %}
#incorrect
def full_name
  @full_name ||= "#{first_name} #{last_name}"
end

@first_name = 'Bunlong'
@last_name = 'Van'

puts full_name #=> "Bunlong Van"

@first_name = 'Ryan'
@last_name = 'Van'

puts full_name #=> "Bunlong Van"
{% endcodeblock %}

<p>
  <strong>Memoization conditional assignment have problem when return nil or false in Ruby</strong><br/>
  If you are not clear in using conditional assignment it can bite you, let try code below:
</p>

{% codeblock lang:ruby %}
def print
  @print || = begin
                puts "printing"
                sleep 2
                false
              end
end

print #=> "printing"

print #=> "printing"
{% endcodeblock %}

<p>
  Suprised that "printing" was printed twice? Conditional assignment is always going to run if the instance variable <code>@print</code> is false or nil.<br/>
</p>

<p>
  Well, we can solve the problem by using <code>defined?</code>:
</p>

{% codeblock lang:ruby %}
def print
  return @print if defined?(@print)
  puts "printing"
  sleep 2
  @print = false
end

print #=> "printing"

print
{% endcodeblock %}

<p>
  <strong>Memoization conditional assignment have problem when return nil or false in Rails</strong><br/>
</p>

{% codeblock lang:ruby %}
def current_user
  @current_user ||= User.find(session[:user_id]) if session[:user_id]
end
{% endcodeblock %}

<p>
  What's happen if <code>User.find</code> return nil? Conditional assignment is always going to run and current_user method is always going to call if the instance variable <code>@print</code> is false or nil.
</p>

<p>
  Well, we can fix the problem by using the Memoizable API in Rails:
</p>

{% codeblock lang:ruby %}

# somewhere inside the class
extend ActiveSupport::Memoizable

def current_user
  @current_user ||= User.find(session[:user_id]) if session[:user_id]
end
memoize :current_user
{% endcodeblock %}

<p>
  Or we can use <a href="https://github.com/dkubb/memoizable" target="_blank">Memoizable</a> to fix this problem
<p>

<p>
  So far so good, I hope this article comes in handy for some of you who haven’t heard of memoization yet or who just didn’t really understand what’s going on there. :)
</p>
