---
layout: post
title: "How to Debug a Ruby on Rails Application"
date: 2015-02-27 00:35
comments: true
categories: [Ruby on Rails, Ruby]
keywords: How to Debug a Ruby on Rails Application, How to Debug a Rails Application, Debug a Ruby on Rails Application, Debug a Rails Application
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="How to Debug a Ruby on Rails Application" />
</p>

<p>
  This article I will show you how you can easily debug your Ruby on Rails application.
</p>

<p>
  <strong>Setup</strong><br/>
  If you’re using the default Rails Rack server, Webrick – the best debugging tool I’ve been able to find out there is <code>pry</code>. First you need to add the pry gem to your Gemfile. Since this is used for development purposes. Open up your Gemfile file so that it looks sometings like:
</p>

{% codeblock Gemfile lang:ruby %}
group :development, :test do
  gem 'pry'
end
{% endcodeblock %}

<p>
  Then run <code>bundle install</code> to install it:
</p>

{% codeblock lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  <strong>Usage</strong><br/>
  Basically, Whenever one of your actions crashes and you can’t figure out straight away what caused it to crash. What you need to do is invoke <code>binding.pry</code> somewhere in your code, which will create a Pry session at the point it’s called. This will pretty much open a 'developer console' where you can play around with all the objects that have been defined up until you called <code>binding.pry</code>. Then you can then easily debug your action, like you’d regularly do in an irb console. When you’re done with the Pry session the program will resume.
</p>

<p>
  Here’s how this would work in a regular create action from Users controller:
</p>

{% codeblock users_controller.rb lang:ruby %}
def create
  user = User.create(user_params)
  binding.pry # Start a pry session to see if the new user looks the way we want
  if user.save
     flash.now[:success] = "User saved successfully"
     sign_in user
     redirect_to user
  else
     flash.now[:error] = "Something went wrong: #{user.errors.full_messages}"
     render 'new'
  end
end
{% endcodeblock %}

<p>
  Well, after you run the create action – you should view the Pry session in the console, where your rails server is running.
</p>

<p>
  So far so good, That's it!!! See yar!!! :)
</p>
