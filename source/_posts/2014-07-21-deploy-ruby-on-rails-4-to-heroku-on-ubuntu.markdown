---
layout: post
title: "Deploy Ruby on Rails 4 to Heroku on Ubuntu"
date: 2014-07-21 21:20
comments: true
categories: [Ruby, Heroku]
keywords: deploy,heroku,ubuntu,ruby,ruby on rails,rails,deploy ruby on rails project to heroku on ubuntu
description: deploy ruby on rails 4 to heroku on ubuntu
---

<p>
  Ruby on Rails is a popular web framework written in Ruby. In this artical covers using Rails 4 on Heroku. The many step for deploying Rails 4 to Heroku below:
</p>

<p>
  <strong>1. Installation heroku environment <a href="https://toolbelt.heroku.com/">(www.toolbelt.heroku.com)</a></strong> -- if you not yet have<br/>
  Open your terminal and type command below:<br/>
</p>

<p>
  1. <code>wget -qO- https://toolbelt.heroku.com/install.sh | sh</code><br/>
  2. <code>gem install heroku foreman</code><br/>
  3. <code>heroku login</code><br/>
</p>

<p>
  <strong>2. Installation git</strong> -- if you not yet have<br/>
  Open your terminal and type command below:<br/>
</p>

<p>
  1. <code>sudo apt-get install git-core</code><br/>
  2. <code>sudo apt-get install expat openssl zlib1g zlib1g-dev</code><br/>
</p>

<p>
  <strong>3. Create Ruby on Rails project<br/></strong>
  Open your terminal and type command below:<br/>
</p>

<p>
  <code>rails new app_name -d postgresql</code><br/>
</p>

<p>
  Go into you project, Open your terminal and type command below:<br/>
</p>

<p>
  <code>cd app_name</code><br/>
</p>

<p>
  We will first create a controller called welcome for our home page to live, Open your terminal and type command below:
</p>

<p>
  <code>rails generate controller welcome</code><br/>
</p>

<p>
   Next then add an index page in directory app/views/welcome/index.html.erb<br/>
</p>

{% codeblock index.html.rb lang:html %}
<h2>Hello World</h2>

<p>
  The time is now: <%= Time.now %>
</p>
{% endcodeblock %}

<p>
  We need to have Rails route to index action. We’ll edit config/routes.rb<br/>
</p>

{% codeblock routes.rb lang:ruby %}
root 'welcome#index'
{% endcodeblock %}

<p>
  Let run the rails app and visiting http://localhost:3000 in your browser, Open your terminal and type command below:
</p>

<p>
  <code>rails server</code><br/>
</p>

<p>
  Then open Gemfile and add gems below at the end:<br/>
</p>

<p>
  <code>gem 'heroku'</code><br/>
  <code>gem 'thin'</code><br/>
  <code>gem 'rails_12factor', group: :production</code></code>
</p>

<p>
  Then open your terminal and type command below:<br/>
</p>

<p>
  <code>bundle install</code>
</p>

<p>
  Rails 4 requires Ruby 1.9.3 or above. Heroku has a recent version of Ruby installed, however you can specify an exact version by using the ruby DSL in your Gemfile by adding ruby "2.1.1" at the end of Gemfile. 
</p>

<p>
  <code>ruby "2.1.1"</code><br/>
</p>

<p>
  <strong>4. Deploy to heroku<br/></strong>
  Open your terminal and type command below:<br/>
</p>

<p>
  1. <code>cd project_name</code><br/>
  2. <code>git init</code><br/>
  3. <code>git add .</code><br/>
  4. <code>git commit -m “my first commit”</code><br/>
  5. <code>heroku create heroku_app_name</code><br/>
  6. <code>heroku git:remote -a heroku_app_name</code><br/>
  7. <code>git push heroku master</code><br/>
</p>

<p>
  <strong>Each time you wish to deploy to Heroku<br/></strong>
  Open your terminal and type command below:<br/>
</p>

<p>
  1. <code>git add -A</code><br/>
  2. <code>git commit -m “commit for deploy to heroku”</code><br/>
  3. <code>git push -f heroku</code><br/><br/>
</p>

<p>
  So far so good, You now have your first application deployed to Heroku. The next step is to deploy your own application. :)
</p>