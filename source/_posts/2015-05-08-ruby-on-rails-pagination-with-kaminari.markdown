---
layout: post
title: "Ruby on Rails Pagination with Kaminari"
date: 2015-05-08 14:53
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Ruby on Rails Pagination with Kaminari
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="Ruby on Rails Pagination with Kaminari" />
</p>

<p>
  The <a href="https://github.com/mislav/will_paginate" target="_blank">will_paginate</a> gem has long since been the most popular gem used when it comes to pagination. However it is not the only gem out there. <a href="https://github.com/amatsuda/kaminari" target="_blank">Kaminari</a> is another very popular pagination gem. In this article we will show you how to use it in your application.
</p>

<p>
  <strong>Setup Rails Application</strong><br/>
  To begin using the kaminari gem, we must first add it to our Gemfile. Open up your Gemfile and add in the line listed below.
</p>

{% codeblock Gemfile lang:ruby %}
gem 'kaminari'
{% endcodeblock %}

<p>
  Now run a bundle install to install the gem.
</p>

{% codeblock lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  Next, create a model called Post. The post model in this example will represent a blog post. Run the commands below to create the post model:
</p>

{% codeblock lang:ruby %}
rails g model Post title body:string
rake db:migrate
{% endcodeblock %}

<p>
  We need some seed data. Open your <code>db/seeds.rb</code> file and modify it so that it looks like the code listed below:
</p>

{% codeblock db/seeds.rb lang:ruby %}
(1..100).each do |i|
  Post.create!(title: "Lipsum Post #{i}", body: %{
      Nullam hendrerit iaculis sodales. Curabitur varius nibh arcu, id molestie nibh fermentum vitae. Cras quis semper dui. Cras porttitor urna sit amet risus vehicula tempor. Maecenas quis tempor ligula. Donec et nibh eu leo volutpat placerat. Fusce vulputate elit in nisi pretium, vel fermentum mi fermentum. Mauris scelerisque, lectus non luctus ultricies, urna eros tincidunt risus, at varius sapien diam id erat.
  })
end
{% endcodeblock %}

<p>
  Now run a rake db:seed to add the seed data:
</p>

{% codeblock lang:ruby %}
rake db:seed
{% endcodeblock %}

<p>
  We need to add a controller called Posts that we will use to render the posts. Run the command below to create this controller now:
</p>

{% codeblock lang:ruby %}
rails g controller Posts index
{% endcodeblock %}

<p>
  Let's modify our routes file to set a site root. Open up your config/routes.rb file and modify it so that it looks like the code listed below.
</p>

{% codeblock config/routes.rb lang:ruby %}
Rails.application.routes.draw do
  root to: "posts#index"
end
{% endcodeblock %}

<p>
  Open up your Posts controller and modify it so that it looks like the code listed below.
</p>

{% codeblock app/controllers/posts_controller.rb lang:ruby %}
class PostsController < ApplicationController
  def index
    @posts = Post.page(params[:page]).per(10)
  end
end
{% endcodeblock %}

<p>
  As you can see, the first bit of our pagination code is here. The <code>page</code> method tells kaminari what page we are on. The <code>per</code> method tells kaminari how many items we wish to have per page.
</p>

<p>
  Let's open up the <code>index</code> view for our <code>Posts</code> controller and modify it so that it looks like the code listed below:
</p>

{% codeblock app/views/posts/index.html.erb lang:ruby %}
<h1>Posts</h1>
<hr />
<% @posts.each do |post| %>
  <h2><%= post.title %></h2>
  <p>
  <%= post.body %>
  </p>
<% end %>
<hr />
<%= paginate @posts %>
<%= page_entries_info @posts %>
{% endcodeblock %}

<p>
  The <code>paginate</code> helper is the line actually responsible for rendering the pagination links. The <code>page_entries_info</code> helper displays a line similar to Displaying posts 1 - 10 of 100 in total. This can be extremely helpful in letting the user know how many items there are in the list.
</p>

<p>
  <strong>Global Configuration</strong><br/>
  You can also specify global defaults for Kaminari. First, run the following command to generate an initializer file for Kaminari:
</p>

{% codeblock lang:ruby %}
rails g kaminari:config
{% endcodeblock %}

<p>
  If you open up the <code>config/initializers/kaminari_config.rb</code> file you'll see something similar to the following:
</p>

{% codeblock config/initializers/kaminari_config.rb lang:ruby %}
Kaminari.configure do |config|
  # config.default_per_page = 25
  # config.max_per_page = nil
  # config.window = 4
  # config.outer_window = 0
  # config.left = 0
  # config.right = 0
  # config.page_method_name = :page
  # config.param_name = :page
end
{% endcodeblock %}

<p>
  A description of these options can be found below:
</p>

Config | Description
--- | ---
config.default_per_page | Specifies the default amount of items to display per page.
config.max_per_page | The maximum amount of items to display per page.
config.window | Specifies the inner window size. The inner window is the number of items in the middle of the pagination block. e.g. « First ‹ Prev ... 1 2 3 4 ... Next › Last », in the previous example, 1 2 3 4 would be considered the inner window.
config.outer_window | Specifies how many items to display in the outer window. For example: 1 2 3 4 ... 8 9 10 11, numbers visible on the outside are the outer window.
config.left | Specifies how many items should be displayed in the left outer window.
config.right | Specifies how many items should be displayed in the right outer window.
config.page_method_name | Changes the page method name from page to whatever you want. Can be used to avoid conflict with other gems that use page.
config.param_name | The default parameter to use when looking for the page number.

<br/>

<p>
  <strong>I18N</strong><br/>
  Kaminari can be configured via I18N. The default I18N configuration file looks like the code listed below:
</p>

{% codeblock lang:ruby %}
en:
  views:
    pagination:
      first: "&laquo; First"
      last: "Last &raquo;"
      previous: "&lsaquo; Prev"
      next: "Next &rsaquo;"
      truncate: "&hellip;"
  helpers:
    page_entries_info:
      one_page:
        display_entries:
          zero: "No %{entry_name} found"
          one: "Displaying <b>1</b> %{entry_name}"
          other: "Displaying <b>all %{count}</b> %{entry_name}"
      more_pages:
        display_entries: "Displaying %{entry_name} <b>%{first}&nbsp;-&nbsp;%{last}</b> of <b>%{total}</b> in total"
{% endcodeblock %}

<p>
  To override it, simply modify your locale files under <code>config/locales</code>.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
