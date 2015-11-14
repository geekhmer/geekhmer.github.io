---
layout: post
title: "Using reCAPTCHA in Ruby on Rails Application"
date: 2015-07-14 23:44
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Using reCAPTCHA in Ruby on Rails Application, reCAPTCHA in Ruby on Rails Application, reCAPTCHA in Rails Application, reCAPTCHA Rails
---

<p>
  For a site with lots of user generated content, fighting spammers can be a never ending battle. Luckily, we can use a service called reCAPTCHA to help keep spammers off our site. reCAPTCHA works by displaying an image on screen and asking the user to type what they see in the image. This works because, the spammer's spambots are unable to read the image and unable to proceed with filling out the form. reCAPTCHA is just one of a number of tools we can use to prevent spammers from abusing our websites.
</p>

<p>
  <strong>Setup Account</strong><br/>
  First, we need to create a reCAPTCHA account. Visit the <a href="http://www.google.com/recaptcha/intro/index.html" target="_blank">reCAPTCHA</a> website and click the button that says 'Get reCAPTCHA'.
</p>

<p>
  <img src="/images/reCAPTCHA_1.png" width="600" alt="Using reCAPTCHA in Ruby on Rails Application" />
</p>

<p>
  On the next screen enter your label, domain, owners and check the box that says 'Get alerts about this site' then click the 'Register' button.
</p>

<p>
  <img src="/images/reCAPTCHA_2.png" width="600" alt="Using reCAPTCHA in Ruby on Rails Application" />
</p>

<p>
  Make sure you copy the public key and private key and save it somewhere, you will need this information when adding reCAPTCHA to your Rails application.
</p>

<p>
  <strong>Setup Rails Application</strong><br/>
  Now we have an account, it's time to set up our Ruby on Rails application. The first thing we will need to do is include the recaptcha gem in our Gemfile. Add the following lines to your gemfile. Note that the bcrypt-ruby gem is used for our example user signup form. You don't have to include this in your application if you aren't using it for user authentication.
</p>

{% codeblock Gemfile lang:ruby %}
gem 'recaptcha', '~> 0.3.5'
gem 'bcrypt-ruby', '~> 3.1.2'
{% endcodeblock %}

<p>
  Next run a bundle install to install the gem:
</p>

{% codeblock lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  Next create an initializer for recaptcha that will contain the public and private keys we mentioned earlier. Create a new initializer called recaptcha.rb and add the code listed below, make sure to modify it to contain your public and private keys.
</p>

{% codeblock config/initializers.rb lang:ruby %}
Recaptcha.configure do |config|
  config.public_key  = 'Replace with your public key'
  config.private_key = 'Replace with your private key'
end
{% endcodeblock %}

<p>
  Next create a model called User that we will use to test the reCAPTCHA functionality. Run the following commands to create the user model:
</p>

{% codeblock lang:ruby %}
rails g model user name email password_digest
rake db:migrate
{% endcodeblock %}

<p>
  Next, open your User model and modify it:
</p>

{% codeblock app/models/user.rb lang:ruby %}
class User < ActiveRecord::Base
  has_secure_password
  validates_presence_of :password, on: :create
  validates :email, uniqueness: true, presence: true
end
{% endcodeblock %}

<p>
  Next create a couple controllers that will handle our user input. The first controller, Home, will provide a simple landing page that contains a sign up link. The second controller, Users, will process the actual user signup request. Run the commands below to create the controllers.
</p>

{% codeblock lang:ruby %}
rails g controller Home show
rails g controller Users new create
{% endcodeblock %}

<p>
  Now edit our routes file to set up a few routes for the controllers we just created. Modify the routes file so that it looks like the code listed below, being sure not to overwrite your application name on the first line:
</p>

{% codeblock config/routes.rb lang:ruby %}
ReCAPTCHAExample::Application.routes.draw do
  resource :home, only: [:show], controller: :home
  resource :users, only: [:new, :create]
  root to: "home#show"
end
{% endcodeblock %}

<p>
  Next open up your home/show view and modify it so that it looks like the code listed below:
</p>

{% codeblock app/views/home/show.html.erb lang:ruby %}
<h3>Welcome!</h3>
<p>
Click the link below to sign up for an account.
</p>
<%= link_to "Sign Up!", new_users_path %>
{% endcodeblock %}

<p>
  Next open up the users/new view and modify it so that it looks like the code listed below:
</p>

{% codeblock app/views/users/new.html.erb lang:ruby %}
<h3>New User Sign Up</h3>
<% if !@user.errors.empty? %>
  <ul>
    <% @user.errors.full_messages.each do |message| %>
      <li><%= message %></li>
    <% end %>
  </ul>
<% end %>
<%= form_for User.new do |f| %>
  <div>
    <%= f.label :name %>
    <%= f.text_field :name %>
  </div>
  <div>
    <%= f.label :email %>
    <%= f.text_field :email %>
  </div>
  <div>
    <%= f.label :password %>
    <%= f.password_field :password %>
  </div>
  <div>
    <%= f.label :password_confirmation %>
    <%= f.password_field :password_confirmation %>
  </div>
  <div>
    <%= recaptcha_tags %>
  </div>
  <div>
    <%= f.submit "Sign Up" %>
  </div>
<% end %>
{% endcodeblock %}

<p>
  Most of the code is self explanatory, however, you'll notice the recaptcha_tags method being called. This method is responsible for rendering the reCAPTCHA.
</p>

<p>
  Next open up our users controller and add some code to handle the request. Modify your users controller so that it looks like the code listed below.
</p>

{% codeblock app/controllers/users_controller.rb lang:ruby %}
class UsersController < ApplicationController
  def new
    @user = User.new
  end

  def create
    captcha_message = "The data you entered for the CAPTCHA wasn't correct.  Please try again"
    @user = User.new(user_params)
    if !verify_recaptcha(model: @user, message: captcha_message) || !@user.save
      render "new"
    end
  end

  private
  def user_params
    params.require(:user).permit(:name, :email, :password, :password_confirmation)
  end
end
{% endcodeblock %}

<p>
  Almost done! Now lets open up our users/create view and add the following code.
</p>

{% codeblock app/views/users/create.html.erb lang:ruby %}
<h3>Thank You!</h3>
<p>
Thanks for signing up!
</p>
{% endcodeblock %}

<p>
  Now if you start a rails server and navigate to http://localhost:3000 we will see a sign up link. if you click on the sign up link you will be presented with a sign up form, complete with a captcha to fill out. You'll notice that filling out the captcha wrongly results in a validation error message, and filling out the correct information allows the user signup to proceed.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
