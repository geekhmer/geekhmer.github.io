---
layout: post
title: "How To Install Rails and Nginx with Passenger on Ubuntu?"
date: 2015-04-24 20:32
comments: true
categories: [Ruby, Ruby on Rails]
keywords: How To Install Rails and Nginx with Passenger on Ubuntu?
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="How To Install Rails and Nginx with Passenger on Ubuntu?" />
</p>

<p>
  Ruby on Rails is an open source web application framework which runs on the Ruby programming language. It allows the developers creating pages and applications that gather information from the web server, talk to or query the database, and render templates out of the box directly to the internet browsers.
</p>

<p>
  As a result, Rails features a routing system that is independent of the web server. The web server is used to render the content only. The choice for that would be Nginx. Nginx is fast webserver with a strong focus on high concurrency, high performance and low memory usage.
</p>

<h3>
  Install Ruby and Rails
</h3>

<p>
  Before we proceed with installation, we should make sure that our system repositories are up to date:
</p>

{% codeblock lang:ruby %}
sudo apt-get update
{% endcodeblock %}

<p>
  Once we are up to date with the latest available packages, the next step is to install Ruby Version Manager. It is application that allows to manage several different ruby versions easily, we can install RVM and then load it with:
</p>

{% codeblock lang:ruby %}
curl -L get.rvm.io | bash -s stable
source /usr/local/rvm/scripts/rvm
{% endcodeblock %}

<p>
  We must make sure that we have all dependencies from RVM. To make sure that we have all required dependencies, we execute the following command:
</p>

{% codeblock lang:ruby %}
rvm requirements
{% endcodeblock %}

<p>
  It will make sure that we are up to date and install the missing requirements (if any). Once we have RVM installed and configured, we can proceed to install and configure Ruby.
</p>

{% codeblock lang:ruby %}
rvm install 2.1.1
rvm use 2.1.1 --default
{% endcodeblock %}

<p>
  These two commands will install Ruby and set the system to use version 2.2.1 by default. The next step is to make sure we have all components for Ruby on Rails. Ruby Gems is a package manager for the Ruby programming language that provides a standard format for distributing Ruby programs and libraries, a tool designed to easily manage the installation of gems, and a server for distributing them, we can install it with this command and then use it to install Rails:
</p>

{% codeblock lang:ruby %}
rvm rubygems current
gem install rails
{% endcodeblock %}

<p>
  This process could take some time, but after it finish, Ruby on Rails is installed.
</p>

<h3>
  Install Passenger and Nginx
</h3>

<p>
  We need to make sure that we can easily deploy Ruby on rails to any web server. We will install and use Passenger for that. It will serve as interface or bridge for communication between Ruby and the web server, you can install it with the following command:
</p>

{% codeblock lang:ruby %}
gem install passenger
{% endcodeblock %}

<p>
  Once passenger is installed, the rest of the required setup is fully automated. We execute the command:
</p>

{% codeblock lang:ruby %}
rvmsudo passenger-install-nginx-module
{% endcodeblock %}

<p>
  Once we do this, it checks for all dependencies automatically and install those that are missing. If some manual user action is required, Passenger will tell us, as well as give us detailed instructions how to do it.
</p>

<p>
  Now we need to configure nginx to “talk” to Passenger. In order to do that, we need to open the nginx configuration file (/opt/nginx/conf/nginx.conf), using our favorite editor and add the following:
</p> 

{% codeblock nginx.conf lang:ruby %}
server {
  listen 80;
  server_name example.com;
  passenger_enabled on;
  root /var/www/rails_app/public;
}
{% endcodeblock %}

<p>
  In order to create our rails app, we need to install Node.js first:
</p>

{% codeblock lang:ruby %}
sudo apt-get install nodejs
{% endcodeblock %}

<p>
  Once that is that, we should go to our directory (in this case it is /var/www/rails_app/public) and create the application. After all this is installed and configured, we simply need to start Nginx.
</p>

{% codeblock lang:ruby %}
rails new my_first_rails_app
sudo service nginx start
{% endcodeblock %}

<p>
  We can try and access our new Ruby on Rails application using our browser. It seems that it was much easier to setup the environment and create our first Ruby on Rails project than we thought.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>