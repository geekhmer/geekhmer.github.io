---
layout: post
title: "Rails Tagging"
date: 2016-01-26 15:03
comments: true
categories: [Ruby on Rails]
keywords: Rails Tagging, Tagging
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="Rails Tagging" />
</p>

<p>
  For instance, in a social network, a user might have tags that are called skills, interests, sports, and more. This article will show you how to implement tagging in your Rails application. Let’s run through this with me.
</p>

<p>
  <strong>Installation</strong><br/>
  Add gem <code>acts-as-taggable-on</code> to your Gemfile:
</p>

{% codeblock Gemfile lang:ruby %}
gem 'acts-as-taggable-on'
{% endcodeblock %}

<p>
  and run bundle:
</p>

{% codeblock lang:ruby %}
bundle
{% endcodeblock %}

<p>
  install migrations:
</p>

{% codeblock lang:ruby %}
rails generate acts_as_taggable_on:migration
rake db:migrate
{% endcodeblock %}

<p>
  This creates some tables and doesn't need to know anything specific about your models. The models and concerns for working with these tables (Tag, Tagging, etc).
</p>

<p>
  <strong>Model Integration</strong>
</p>

{% codeblock app/models/post.rb lang:ruby %}
class Post
  acts_as_taggable
end
{% endcodeblock %}

<p>
  Now you can do, eg:
</p>

{% codeblock lang:ruby %}
post = Post.create
post.tag_list = "programming, ruby, rails"
post.tag_list
# => ['programming', 'ruby', 'rails']
{% endcodeblock %}

<p>
  <code>#tag_list=</code> takes a string and splits it up, using the resulting substrings to find_or_create Tags, and associate them with the taggable thing (eg, <code>Post</code>), through <code>Taggings</code>. You don't need to know this.
</p>

<p>
  The important thing is that, with <code>#tag_list=</code> we can manage tags via a comma-separated-list in a text field in a form.
</p>

<p>
  <strong>Controller Integration</strong>
</p>

{% codeblock app/controllers/posts_controller.rb lang:ruby %}
class PostsController < ApplicationController
  def create
    @post = Post.create(post_params)
    redirect_to @post
  end

  def post_params
    params.require(:post).permit(:title, :body, :tag_list)
  end
end
{% endcodeblock %}

<p>
  This is assuming you're trying to tag an existing <code>Post</code> model with <code>title</code> and <code>body</code> fields, web-accessible through an existing <code>PostsController</code>, with <code>index</code>, <code>show</code>, <code>new</code>, etc. methods.
</p>

<p>
  Substitute your own, the important machinery here is whitelisting <code>tag_list</code> from the <code>params</code>.
</p>

<p>
  <strong>Form Integration</strong>
</p>

{% codeblock app/views/posts/_form.html.erb lang:ruby %}
<%= form_for post do |f| %>
  <%= f.text_field :title %>
  <%= f.text_area :body %>
  <%= f.text_field :tag_list %>
  <%= f.submit %>
<% end %>
{% endcodeblock %}

<p>
  Now you can create tags for stuff. How about displaying them? Couple options, I'll go through the most explicit (a <code>TagsController</code> with index and show actions), but they can be rolled up into other controllers/actions.
</p>

<p>
  <strong>Controller</strong>
</p>

{% codeblock config/routes.rb lang:ruby %}
Rails.application.routes.draw do
  resources :posts
  resources :tags, only: [:index, :show]
end
{% endcodeblock %}

{% codeblock app/controllers/tags_controller.rb lang:ruby %}
class TagsController < ApplicationController
  def index
    @tags = ActsAsTaggableOn::Tag.all
  end

  def show
    @tag =  ActsAsTaggableOn::Tag.find(params[:id])
    @posts = Post.tagged_with(@tag.name)
  end
end
{% endcodeblock %}

<p>
  It's unfortunate we have to do this slightly awkward workaround with <code>Post.tagged_with(@tag.name)</code> in <code>tags#show</code>. The <code>ActsAsTaggableOn::Tag</code> model does not have a built-in relationship with its taggable types (this is a necessary consequence of some polymorphism which we're not using here). We could add one for <code>Post</code>, but this way is easier to demonstrate.
</p>

<p>
  <strong>Tags Views</strong>
</p>

{% codeblock app/views/acts_as_taggable_on/tags/_tag.html.erb lang:ruby %}
<%= link_to tag.name, tag_path(tag) %>
{% endcodeblock %}

{% codeblock app/views/tags/index.html.erb lang:ruby %}
<h1>Tags</h1>
<%= render @tags %>
{% endcodeblock %}

{% codeblock app/views/tags/show.html.erb lang:ruby %}
<h1><%= @tag.name %></h1>
<div><%= render @posts %></div>
{% endcodeblock %}

<p>
  Note the partial path is <code>acts_as_taggable_on/tags/tag</code>. This is so we can just say <code>render @tags</code> and let rails do its implicit magic. There are other ways to organize everything, but this is the simplest.
</p>

<p>
  <strong>View Integration</strong>
</p>

{% codeblock app/views/posts/_post.html.erb lang:ruby %}
<h2><%= link_to post.title, post_path(post) %></h2>
<div><%= render post.tags %></div>
{% endcodeblock %}

{% codeblock app/views/posts/index.html.erb lang:ruby %}
<h1>Posts</h1>
<%= render @posts %>
{% endcodeblock %}

{% codeblock app/views/posts/show.html.erb lang:ruby %}
<h1><%= @post.title %></h1>
<div><%= @post.body %></div>
<div><%= render @post.tags %></div>
{% endcodeblock %}

<p>
  And that should be it. It'll look shitty, but I hope you can figure out how to elaborate on this once you have it working. If anything here is assuming familiarity with something you don't have, ask and I will gladly elaborate.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
