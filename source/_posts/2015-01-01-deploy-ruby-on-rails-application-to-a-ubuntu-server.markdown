---
layout: post
title: "Deploy Ruby on Rails Application to a Ubuntu Server"
date: 2015-01-01 00:00
comments: true
categories: [Ruby on Rails]
keywords: Deploy Ruby on Rails Application to a Ubuntu Server, Deploy Ruby on Rails Application, Deploy Ruby on Rails Application to a Ubuntu Server Using Unicorn and Capistrano
---

<p>
  <img src="/images/move_to_rails.png" alt="Tools for Monitoring Performance in Ruby on Rails Application" />
</p>

<p>
  Assumes you have Ruby on Rails application already.
</p>

<p>
  <strong>Setup Ruby Version, Unicorn and Capistrano</strong><br/>
  Specifiy a ruby version for your app by creating a new file in the root of your app called ".ruby-version" that includes:
</p>

{% codeblock .ruby-version lang:ruby %}
2.1.4
{% endcodeblock %}

<p>
  Make the following changes to the Gemfile:
</p>

{% codeblock Gemfile lang:ruby %}
ruby '2.1.4'

gem 'unicorn'
gem 'capistrano-rails', group: :development
{% endcodeblock %}

<p>
  Type command below to install gems:
</p>

{% codeblock lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  Type command below to install binstubs for capistrano:
</p>

{% codeblock lang:ruby %}
bundle binstubs capistrano
{% endcodeblock %}

<p>
  <strong>Configure Capistrano</strong><br/>
  Type command below to initialize capistrano:
</p>

{% codeblock lang:ruby %}
bin/cap install
{% endcodeblock %}

<p>
  Add the following below require 'capistrano/deploy' in the Capfile in the root of your app:
</p>

{% codeblock Capfile lang:ruby %}
require 'capistrano/rails'
{% endcodeblock %}

<p>
  Add or Replace this configuration in config/deploy.rb file:
</p>

{% codeblock deploy.rb lang:ruby %}
set :application, 'myapp'
set :repo_url, 'git@github.com:bunlong/myapp.git'
set :deploy_to, '/opt/www/myapp'
set :user, 'deploy'
set :linked_dirs, %w{log tmp/pids tmp/cache tmp/sockets}

namespace :deploy do
  %w[start stop restart].each do |command|
    desc 'Manage Unicorn'

    task command do
      on roles(:app), in: :sequence, wait: 1 do
        execute "/etc/init.d/unicorn_#{fetch(:application)} #{command}"
      end
    end
  end

  after :publishing, :restart
end
{% endcodeblock %}

<p>
  After the configuration in /config/deploy/production.rb with your server ip or domain name:
</p>

{% codeblock production.rb lang:ruby %}
role :app, %w{deploy@0.0.0.0}
role :web, %w{deploy@0.0.0.0}
role :db,  %w{deploy@0.0.0.0}
{% endcodeblock %}

<p>
  <strong>Configure Unicorn</strong><br/>
  Create a new file config/unicorn.rb with the following contents:
</p>

{% codeblock unicorn.rb lang:ruby %}
root = "/opt/www/myapp/current"
working_directory root
pid "#{root}/tmp/pids/unicorn.pid"
stderr_path "#{root}/log/unicorn.log"
stdout_path "#{root}/log/unicorn.log"

listen "/tmp/unicorn.myapp.sock"
worker_processes 1
timeout 30
{% endcodeblock %}

<p>
  Comment out production username and password from config/database.yml:
</p>

{% codeblock database.yml lang:ruby %}
production:
  <<: *default
  database: myapp_production
{% endcodeblock %}

<p>
  Type command below to push changes to git:
</p>

{% codeblock lang:ruby %}
git add .
git commit -m 'Added settings to deploy app'
git push origin master
{% endcodeblock %}

<p>
  Type command below to create a secret to be used on the server:
</p>

{% codeblock lang:ruby %}
bin/rake secret
{% endcodeblock %}

<p>
  On the server setup the secret by modify /home/deploy/.bashrc with the following contents:
</p>

{% codeblock .bashrc lang:ruby %}
export SECRET_KEY_BASE=[REPLACE WITH YOUR SECRET]
{% endcodeblock %}

<p>
  On the server restart nginx, type command below:
</p>

{% codeblock lang:ruby %}
sudo service nginx restart
{% endcodeblock %}

<p>
  <strong>Deploy</strong><br/>
  Type command below to make sure capistrano is connected to the server:
</p>

{% codeblock lang:ruby %}
bin/cap production git:check
{% endcodeblock %}

<p>
  Type command below to make sure can deploy or not:
</p>

{% codeblock lang:ruby %}
bin/cap production deploy:check
{% endcodeblock %}

<p>
  Type command below for deploying:
</p>

{% codeblock lang:ruby %}
bin/cap production deploy
{% endcodeblock %}

<p>
  If you need to run db:seed, log into server as the deploy user and run following:
</p>

{% codeblock lang:ruby %}
cd /opt/www/myapp/current ; bin/rake RAILS_ENV=production db:seed
{% endcodeblock %}

<p>
  If you are having problems, try running a console on the server, log in as deploy user and run the following:
</p>

{% codeblock lang:ruby %}
cd /opt/www/myapp/current ; bin/rails c production
{% endcodeblock %}
