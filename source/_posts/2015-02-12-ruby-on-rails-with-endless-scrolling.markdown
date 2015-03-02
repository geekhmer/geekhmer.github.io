---
layout: post
title: "Ruby on Rails with Endless Scrolling"
date: 2015-02-12 23:20
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Ruby on Rails with Endless Scrolling, Ruby on Rails Endless Scrolling, Rails with Endless Scrolling, Rails Endless Scrolling
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails with Endless Scrolling" />
</p>

<p>
  Endless scrolling allows a website to let users avoid having to click page links or pagination in order to load additional pages of content. It is used by a number of sites such as Pinterest in order to enhanced the user experience. This article will show you how to implement endless scrolling in your Rails application. Let’s run through this with me.
</p>

<p>
  <strong>Create Rails Project</strong><br/>
  To create a Rails project; open up your terminal and type commands below:
</p>

{% codeblock lang:ruby %}
rails new endless_scrolling -d mysql
{% endcodeblock %}

<p>
  <strong>Setting up</strong><br/>
  Ruby on Rails endless scrolling uses the will_paginate gem to manage paging. This has a couple of advantages.
</p>

<p>
  First, if your endless scrolling code doesn't work properly or is disabled, the pagination links themselves will still be present and allow the user to page.
</p>

<p>
  Second, the will_paginate gem provides us with the pagination functionality itself so that we do not need to reinvent the wheel.
</p>

<p>
  To get started, add the will_paginate gem to your <code>Gemfile</code> file.
</p>

{% codeblock Gemfile lang:ruby %}
gem 'will_paginate', '~> 3.0.7'
{% endcodeblock %}

<p>
  Then run a <code>bundle install</code> to install the gem:
</p>

{% codeblock lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  we will create a simple Post model with the fields title, and body. In addition, we will create a simple Posts controller with an index method. Run the commands below to create these items:
</p>

{% codeblock lang:ruby %}
rails g model Post title body:text
rake db:migrate
rails g controller Posts index
{% endcodeblock %}

<p>
  Then open up your routes (<code>config/routes.rb</code>) file and modify it so that it looks like the code listed below:
</p>

{% codeblock routes.rb lang:ruby %}
Rails.application.routes.draw do
  root to: "posts#index"
end
{% endcodeblock %}

<p>
  Then we need some seed data to play with. Open up your seeds.rb file and add in the code listed below. This code will create 100 posts for us to play with:
</p>

{% codeblock seeds.rb lang:ruby %}
(1..100).each do |i|
  Post.create!(title: "Lipsum Post #{i}", body: %{
    Lorem ipsum dolor sit amet, consectetur adipiscing elit. In feugiat purus dapibus fermentum sagittis. Fusce in tempus felis. Phasellus a erat ut lorem lacinia bibendum. Vivamus viverra facilisis neque, in scelerisque urna pharetra vel. Donec a est mauris. Integer eget metus quis eros egestas elementum. Integer bibendum risus hendrerit dapibus tempor. Fusce placerat in orci vitae tincidunt.
  })
end
{% endcodeblock %}

<p>
  Then run rake db:seed to create the seed data:
</p>

{% codeblock lang:ruby %}
rake db:seed
{% endcodeblock %}

<p>
  Then open up your Posts controller (<code>app/controllers/posts_controller.rb</code>) and modify it so that it looks like the code listed below:
</p>

{% codeblock lang:ruby %}
class PostsController < ApplicationController
  def index
    @posts = Post.paginate(:page => params[:page], :per_page => 20)
  end
end
{% endcodeblock %}

<p>
  Now modify the index view for your Posts controller so that it looks like the code listed below.
</p>

{% codeblock posts_controller.rb lang:ruby %}
class PostsController < ApplicationController
  def index
    @posts = Post.paginate(:page => params[:page], :per_page => 20)
  end
end
{% endcodeblock %}

<p>
  Then modify the index (<code>app/views/posts/index.html.erb</code>) view for your Posts controller so that it looks like the code listed below:
</p>

{% codeblock index.html.erb lang:ruby %}
<div id="posts">
  <h1>Posts</h1>
  <%= render @posts %>
</div>
<%= will_paginate @posts %>
{% endcodeblock %}

<p>
  Then let's create the post partial. Create a file called _post.html.erb (<code>app/views/posts/_post.html.erb</code>) for your Posts controller and add in the code listed below:
</p>

{% codeblock _post.html.erb lang:ruby %}
<div class="post">
  <h2><%= post.title %></h2>
  <p><%= post.body %></p>
</div>
{% endcodeblock %}

<p>
  If you were to start a rails server at this point, you'd see a typical paginated list of posts. Now it's time to add in the javascript that will make endless scrolling work. Open up your application.js (<code>app/assets/javascripts/application.js</code>) file and add in the code listed below:
</p>

{% codeblock application.js lang:ruby %}
$(document).ready(function() {
  if ($('.pagination').length) {
    $(window).scroll(function() {
      var url = $('.pagination .next_page').attr('href');
      if (url && $(window).scrollTop() > $(document).height() - $(window).height() - 50) {
        $('.pagination').text("Please Wait...");
        return $.getScript(url);
      }
    });
    return $(window).scroll();
  }
});
{% endcodeblock %}

<p>
  Then create a file called index.js.erb (<code>app/views/posts/index.js.erb</code>) for your Posts controller and add in the code listed below:
</p>

{% codeblock index.js.erb lang:ruby %}
$('#posts').append('<%= escape_javascript render(@posts) %>');
$('.pagination').replaceWith('<%= escape_javascript will_paginate(@posts) %>');
{% endcodeblock %}

<p>
  The code works by watching the window's scroll event. When the user scrolls past the specified threshold, more posts are loaded using AJAX. That's it, thank you!. See ya! :)
</p>
