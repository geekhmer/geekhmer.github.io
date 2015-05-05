---
layout: post
title: "Ruby on Rails Web Console"
date: 2015-04-19 08:29
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Ruby on Rails Web Console
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails Web Console" />
</p>

<p>
  As you know, Ruby on Rails has long included the ability to load up an interactive console environment for your application. Simply running <code>rails c</code> would start this console, which allows you to do things like ActiveRecord queries, troubleshooting, and much more. Since the full Rails stack gets loaded you have access to pretty much anything your application has access to. This is useful to troubleshoot things on the spot.
</p>

<p>
  Starting with Ruby on Rails 4.2, developers will have access to a web based version of this console. The good news is that you don't have to wait for Ruby on Rails 4.2 to hit in order to take advantage of this awesome capability. You can quickly and easily use the web console in your current Rails application by performing the steps below.
</p>

<p>
  <strong>Setup</strong><br/>
  To use the Rails web console, we need to include the web-console gem. Simply add the line below to your Gemfile, then run a bundle install to install it.
</p>

{% codeblock Gemfile lang:ruby %}
group :development do
  gem 'web-console', '~> 2.0'
end
{% endcodeblock %}

<p>
  Terminal Commands:
</p>

{% codeblock Gemfile lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  Next, we need to turn on the web console in our application configuration. To do this, simply add the following line to your development.rb environment file:
</p>

{% codeblock config/environments/development.rb lang:ruby %}
config.web_console.automount = true
{% endcodeblock %}

<p>
  Now, let's create a controller to play with the console. Run the command below to create a controller that we can use to play around with the console. For this example, we will create a controller called Homes with a method called show.
</p>

<p>
  Terminal Commands:
</p>

{% codeblock lang:ruby %}
rails g controller Homes show
{% endcodeblock %}

<p>
  Now, open up your routes file and modify it so that it looks like the code listed below:
</p>

{% codeblock config/routes.rb lang:ruby %}
Rails.application.routes.draw do
  root to: "homes#show"
end
{% endcodeblock %}

<p>
  Now open up the show view for the Homes controller and add in the code listed below:
</p>

{% codeblock app/views/homes/show.html.erb lang:ruby %}
<%= console %>
{% endcodeblock %}

<p>
  If you start a rails server and navigate to your development environment, you'll notice that there is a console at the bottom of the screen. You can execute Ruby and Rails code here. The console is running within your Rails application so pretty much anything goes.
</p>

<p>
  By default, console application is mounted to http://yoursite/console. However, what if we wish to change the path? We can easily do it by adding the line listed below to our development.rb file:
</p>

{% codeblock config/environments/development.rb lang:ruby %}
config.web_console.default_mount_path = '/your/favorite/path'
{% endcodeblock %}

<p>
  <strong>Whitelisting IPs</strong><br/>
  The console by default only works with if you are accessing it locally via 127.0.0.1. If you are on a network or using a VM, you may need to add your ip to a whitelist. You can easily do that by adding the line listed below to your development.rb file. Don't forget to restart your Rails server for changes to take effect.
</p>

{% codeblock config/environments/development.rb lang:ruby %}
config.web_console.whitelisted_ips =  %w( 127.0.0.1 192.168.0.100 )
{% endcodeblock %}

<p>
  You can also add an entire subnet. Remember to make sure you add 127.0.0.1 or the local machine won't have access.
</p>

{% codeblock config/environments/development.rb lang:ruby %}
config.web_console.whitelisted_ips =  %w( 127.0.0.1 192.168.0.016 )
{% endcodeblock %}

<p>
  <strong>Run Other Commands</strong><br/>
  By default the console runs the rails console. However, you can have it run another command instead. For instance, we can turn the web console into an terminal running bash by adding the following line to our development.rb file:
</p>

{% codeblock config/environments/development.rb lang:ruby %}
config.web_console.command = 'sudo /bin/login'
{% endcodeblock %}

<p>
  Be aware that passwords sent using this method are sent in plaintext. if that concerns you, you should consider using SSL.
</p>

<p>
  <strong>Security Considerations</strong><br/>
  At this point, it's not advisable to run this on production. Exposing the console endpoint in a production environment means anyone can get access to the Rails console and therefore access to the system itself. Stick to the development/test environments. Hopefully in the future the Rails team will provide additional features that will allow console use in a production environment.
</p>

<p>
  As mentioned earlier, console information is sent plain text. If this concerns you, consider using SSL or not using the web console.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
