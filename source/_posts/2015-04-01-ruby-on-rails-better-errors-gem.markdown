---
layout: post
title: "Ruby on Rails Better Errors Gem"
date: 2015-04-01 23:55
comments: true
categories: [Ruby on Rails]
keywords: Ruby on Rails Better Errors Gem, Rails Better Errors Gem
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails Better Errors Gem" />
</p>

<p>
  <a href="https://github.com/charliesome/better_errors" target="_blank">Better Errors</a> replaces the standard Rails error page with a much better and more useful error page. It is also usable outside of Rails in any Rack app as Rack middleware.
</p>

<p>
  Instead of a plain default error page, Better Errors will display a full interactive stack trace with source code inspection.
</p>

<p>
  If you also include the companion binding_of_caller2 gem into your application Better Errors will be able to also let you inspect local and instance variables and even embed a full REPL into every stack frame of your error page backtrace. Of course you should only ever do that in a development environment.
</p>

<p>
  To use it simply add the following to your Gemfile:
</p>

{% codeblock lang:ruby %}
group :development do
  gem "better_errors"
  gem 'binding_of_caller'
end
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
