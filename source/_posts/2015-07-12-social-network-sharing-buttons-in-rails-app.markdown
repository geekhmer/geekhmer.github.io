---
layout: post
title: "Social Network Sharing Buttons in Rails App"
date: 2015-07-12 22:46
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Social Network Sharing Buttons in Rails App, Social Network Sharing Buttons,  Social Network Sharing,  Social Network
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="Social Network Sharing Buttons in Rails App" />
</p>

<p>
  There are many services that allow you to add social network buttons to your website. The problem with these services is that they are free because they harvest traffic statistics from your site, track your visitors, and in general are a big nuisance. They also have the disadvantage of being slow to load.
</p>

<p>
  In this article we will show you how to add social sharing buttons to your website via a gem called Social Share Button. Unlike most services, Social Share Button has buttons that are hosted locally on your site and don't go through a third party service. As a result, they are quick to load and extremely customizable.
</p>

<p>
  <strong>Setup Application</strong><br/>
  First we need to add the social-share-button gem to our Gemfile. Open up your Gemfile and add in the line listed below:
</p>

{% codeblock Gemfile lang:ruby %}
gem 'social-share-button', '~> 0.1.6'
{% endcodeblock %}

<p>
  Next, we need to run a bundle install to install the gem:
</p>

{% codeblock lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  Now we need to run the <code>rails generate social_share_button:install</code> command to install some additional configuration files that the Social Sharing Button gem uses to display the buttons on your site. This includes an initializer called social_share_button.rb in your initializers folder as well as some localization files:
</p>

{% codeblock lang:ruby %}
rails generate social_share_button:install
{% endcodeblock %}

<p>
  Great, Next we need to do is create a simple Home controller in order to test the social sharing button functionality:
</p>

{% codeblock lang:ruby %}
rails g controller homes show
{% endcodeblock %}

<p>
  Now let's modify our routes file a bit:
</p>

{% codeblock config/routes.rb lang:ruby %}
Rails.application.routes.draw do
  resources :homes, only: [:show]
  root to: "homes#show"
end
{% endcodeblock %}

<p>
  Great, now we need to add some javascript and CSS includes to make things work. Open up your application.js file and modify it so that it looks like the code listed below:
</p>

{% codeblock app/assets/javascripts/application.js lang:ruby %}
//= require jquery
//= require jquery_ujs
//= require turbolinks
//= require social-share-button
//= require_tree .
{% endcodeblock %}

<p>
  Great, now for the application.css file. Open it up and modify it so that it looks like the code listed below:
</p>

{% codeblock app/assets/stylesheets/application.css lang:ruby %}
/*
 *= require_tree .
 *= require social-share-button
 *= require_self
 */
{% endcodeblock %}

<p>
  Excellent, now we can add the social sharing buttons to our site. Open up your homes/show view and modify it so that it looks like the code listed below:
</p>

{% codeblock app/views/homes/show.html.erb lang:ruby %}
<%= social_share_button_tag("My Site") %>
{% endcodeblock %}

<p>
  Great, now if we run rails server and visit http://localhost:3000, we will see the social sharing buttons. However, you'll notice that there is a problem. About 15 or so buttons are listed, but we only care about a couple. Luckily we can easily resolve this. Open up the social_share_button initializer and modify it so that it looks like the code listed below:
</p>

{% codeblock config/initializers/social_share_button.rb lang:ruby %}
SocialShareButton.configure do |config|
  config.allow_sites = %w(twitter facebook google_plus delicious tumblr pinterest)
end
{% endcodeblock %}

<p>
  Now if you restart your rails server, you'll notice that the buttons have been limited to the sites listed above. Note that you can also specify a specific url if needed. Social Share Button detects the current URL by default, but there are times when you may want to customize this. You can do this like so:
</p>

{% codeblock lang:ruby %}
<%= social_share_button_tag("My Home Page", :url => "http://mysite.com/sample") %>
{% endcodeblock %}

<p>
  However, what if we want to customize the button look/feel? Luckily this is pretty easy. If you inspect the elements in chrome developer tools you'll notice that they each have a unique css class. We can override this CSS class with our own code. For example, to modify the facebook button to look different, we'd use something like the following CSS (placed into our application.css file):
</p>

{% codeblock lang:ruby %}
.social-share-button-facebook {
  display: inline-block;
  width: 16px;
  height: 16px;
  background: url("/social/facebook.png") no-repeat;
}
{% endcodeblock %}

<p>
  So far go good, That's it!!! See ya!!! :)
</p>
