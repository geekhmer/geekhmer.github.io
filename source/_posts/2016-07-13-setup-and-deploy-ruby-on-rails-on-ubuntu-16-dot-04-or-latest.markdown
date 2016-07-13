---
layout: post
title: "Setup and Deploy Ruby On Rails on Ubuntu 16.04 or Latest"
date: 2016-07-13 15:00
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Setup and Deploy Ruby On Rails on Ubuntu 16.04 or Latest
---

<p>
  <img src="/images/rails_nginx_ubuntu.jpg" width="600" alt="Setup and Deploy Ruby On Rails on Ubuntu 16.04 or Latest" />
</p>


<p>
  Since we setup Ubuntu for our development environment, we also want to use it in production. This keeps your application running consistently between development and production. 
</p>

<h3>Install Ruby</h3>

<p>
  The first step is to install some dependencies for Ruby:
</p>

{% codeblock lang:ruby %}
sudo apt-get update
sudo apt-get install git-core curl zlib1g-dev build-essential libssl-dev libreadline-dev libyaml-dev libsqlite3-dev sqlite3 libxml2-dev libxslt1-dev libcurl4-openssl-dev python-software-properties libffi-dev
{% endcodeblock %}

<p>
  Next we're going to be installing Ruby using rvm:
</p>

{% codeblock lang:ruby %}
sudo apt-get install libgdbm-dev libncurses5-dev automake libtool bison libffi-dev
gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3
curl -sSL https://get.rvm.io | bash -s stable
source ~/.rvm/scripts/rvm
rvm install 2.3.1
rvm use 2.3.1 --default
ruby -v
{% endcodeblock %}

<p>
  The last step is to install Bundler
</p>

{% codeblock lang:ruby %}
gem install bundler
{% endcodeblock %}

<h3>Install Nginx</h3>

<p>
  Phusion is the company that develops Passenger and they recently put out an official Ubuntu package that ships with Nginx and Passenger pre-installed.
</p>

<p>
  We'll be using that to setup our production server because it's very easy to setup:
</p>

{% codeblock lang:ruby %}
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 561F9B9CAC40B2F7
sudo apt-get install -y apt-transport-https ca-certificates

# Add Passenger APT repository
sudo sh -c 'echo deb https://oss-binaries.phusionpassenger.com/apt/passenger xenial main > /etc/apt/sources.list.d/passenger.list'
sudo apt-get update

# Install Passenger & Nginx
sudo apt-get install -y nginx-extras passenger
{% endcodeblock %}

<p>So now we have Nginx and passenger installed. We can manage the Nginx webserver by using the service command:</p>

{% codeblock lang:ruby %}
sudo service nginx start
{% endcodeblock %}

<p>
The service command also provides some other methods such as <code>restart</code> and <code>stop</code> that allow you to easily restart and stop your webserver.
</p>

<p>Next, we need to update the Nginx configuration to point Passenger to the version of Ruby that we're using. You'll want to open up <code>/etc/nginx/nginx.conf</code> in your favorite editor. I like to use vim, so I'd run this command:</p>

{% codeblock lang:ruby %}
sudo vim /etc/nginx/nginx.conf
{% endcodeblock %}

<p>Find the following lines, and uncomment them:</p>


{% codeblock lang:ruby %}
##
# Phusion Passenger config
##
# Uncomment it if you installed passenger or passenger-enterprise
##

include /etc/nginx/passenger.conf;
{% endcodeblock %}

<p>Once you've configured <code>/etc/nginx/nginx.conf</code> , you can run <code>sudo service nginx restart</code> to restart Nginx with the new Passenger configuration.</p>

<h3>Install MySQL and PostgreSQL</h3>

<p>Setting up your production database is pretty easy. Make sure to keep in mind that you should use a different password for your production databases.</p>

<p>Depending on what database you want to use, follow the steps related to the database:</p>

<p>
  <strong>Install MySQL</strong><br/>
  All you need to do in order to install MySQL is to run the following command:
</p>

{% codeblock lang:ruby %}
sudo apt-get install mysql-server mysql-client libmysqlclient-dev
{% endcodeblock %}

<p>You can use the root user and password set during installation for your database or you can add a new user to MySQL.</p>

<p>
  <strong>Install PostgreSQL</strong><br/>
  We can install PostgreSQL like so:
</p>

{% codeblock lang:ruby %}
sudo apt-get install postgresql postgresql-contrib libpq-dev
{% endcodeblock %}

<p>Next we need to setup the postgres user:</p>

{% codeblock lang:ruby %}
sudo su - postgres
createuser --pwprompt
exit
{% endcodeblock %}

<p>The password you type in here will be the one to put in your <code>my_app/current/config/database.yml</code> later when you deploy your app for the first time.</p>

<h3>Adding The Nginx Host</h3>

<p>In order to get Nginx to respond with the Rails app, we need to modify it's sites-enabled.</p>

<p>Open up <code>/etc/nginx/sites-enabled/default</code> in your text editor and we will replace the file's contents with the following:</p>

{% codeblock lang:ruby %}
server {
  listen 80 default_server;
  listen [::]:80 default_server ipv6only=on;

  server_name mydomain.com;
  passenger_enabled on;
  rails_env    production;
  root         /home/deploy/myapp/public;

  # redirect server error pages to the static page /50x.html
  error_page   500 502 503 504  /50x.html;
  location = /50x.html {
    root   html;
  }
}
{% endcodeblock %}

<p>This is our Nginx configuration for a server listening on port 80. You need to change the server_name values to match the domain you want to use and in root replace "myapp" with the name of your application.</p>

<h3>Connect The Database</h3>

<p>
  The file <code>config/database.yml</code> needs to be updated for the production database server username, password, and host. You can set host to "localhost" and you will have to create a database on the server with the same name by using command:
</p>

{% codeblock lang:ruby %}
rake db:create RAILS_ENV=production
rake db:migrate RAILS_ENV=production
{% endcodeblock %}

<p>Then run command below to minify or compress JavaScript and CSS assets:</p>

{% codeblock lang:ruby %}
rake assets:precompile RAILS_ENV=production
{% endcodeblock %}

<h3>Setup Secret Key Base</h3>

<p>Go to your rails app directory and run command below to generate secret key base:</p>

{% codeblock lang:ruby %}
rake secret RAILS_ENV=production
{% endcodeblock %}

<p>
  Go to <code>/yourapp/config/secrets.yml</code> and set production secret_key_base. Then reload Nginx using command line: <code>sudo service nginx reload</code>
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
