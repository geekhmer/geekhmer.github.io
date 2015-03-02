---
layout: post
title: "Ruby on Rails Generate Random Data"
date: 2015-02-11 19:21
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Ruby on Rails Generate Random Data, Rails Generate Random Data
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails Generate Random Data" />
</p>

<p>
  Sometimes we need to programmatically generate text in Ruby on Rails. We may not have any data available to play with or we may need text in order to mock up the user interface. In this article I will cover 2 different methods of generating random text. Let’s run through this with me.
</p>

<p>
  <strong>Random Letters</strong><br/>
  If you just need fill text, you can add this helper to your application helper (<code>app/helpers/application_helper.rb</code>):
</p>

{% codeblock application_helper.rb lang:ruby %}
module ApplicationHelper
  def random_string(length, include_uppercase = true, include_lowercase = true, include_numbers = false)
    l = []
    l.push ('a'..'z') if include_uppercase
    l.push ('A'..'Z') if include_lowercase
    
    l.push (0..9) if include_numbers
    l = l.map { |i| i.to_a }.flatten
    string = (0...length).map { l[rand(l.length)] }.join
  end
end
{% endcodeblock %}

<p>
  The helper has 1 required argument is length, which determines the length of the string. To generate a random string of 200 characters, call the helper like below:
</p>

{% codeblock lang:ruby %}
<%= random_string(200) %>
{% endcodeblock %}

<p>
  <strong>Forgery Gem</strong><br/>
  The <a href="https://github.com/sevenwire/forgery" target="_blank">forgery</a> gem provides a great way to generate random data. Not only can forgery generate random words (based off lorem ipsum), but it can also generate random email addresses, and much more. To use forgery, just include the forgery gem in your gemfile:
</p>

{% codeblock Gemfile lang:ruby %}
gem 'forgery', '0.6.0'
{% endcodeblock %}

<p>
  Then run <code>bundle install</code> to install the Gem.
</p>

<p>
  After installing the Gem, you are ready to go to generate words:
</p>

{% codeblock lang:ruby %}
<%= Forgery(:lorem_ipsum).words(100) %>
{% endcodeblock %}

<p>
  To generate a random email address:
</p>

{% codeblock lang:ruby %}
<%= Forgery('internet').email_address %>
{% endcodeblock %}

<p>
  So far so good, I hope all you guys enjoy it. See ya! :)
</p>
