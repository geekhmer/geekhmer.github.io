---
layout: post
title: "How to Build a Sitemap for Your Ruby on Rails Application"
date: 2015-04-16 21:40
comments: true
categories: [Ruby, Ruby on Rails]
keywords: How to Build a Sitemap for Your Ruby on Rails Application, How to Build a Sitemap for Your Rails Application, How to Build a Sitemap for Your Rails
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="How to Build a Sitemap for Your Ruby on Rails Application" />
</p>

<p>
  Sitemaps are a valuable tool for telling search engines about the structure of your website. Creating a sitemap and submitting it to the search engines can let Google or Bing know about the pages they have missed when they crawled your site. In addition, submitting a sitemap can speed up crawl times significantly.
</p>

<p>
  In this article we will show you how to build your own sitemap which you can then submit to the various search engines.
</p>

<p>
  <strong>Rails Application Setup</strong><br/>
  We are going to build a very simple blog. First we will build the blog itself, then we will add the functionality needed for the sitemap. Don't worry, this won't take long.
</p>

<p>
  First let's create our models. We only need one model for this example, Post. The Post model is simply an instance of a blog entry. Run the commands below to create the Post model now:
</p>

{% codeblock lang:ruby %}
rails g model Post title body:text
rake db:migrate
{% endcodeblock %}

<p>
  Great, now let's create our controllers. In this example we will have two controllers. The Posts controller lists our posts and lets us view individual posts, and the Sitemap controller actually generates the sitemap. Run the commands below to create these controllers now:
</p>

{% codeblock lang:ruby %}
rails g controller Posts index show
rails g controller Sitemap index
{% endcodeblock %}

<p>
  Now let's add our routes. Open up your routes file and modify it to look like the code listed below. Be sure not to overwrite line 1 of your routes file with line 1 of the example code listed:
</p>

{% codeblock config/routes.rb lang:ruby %}
SitemapExample::Application.routes.draw do
  resources :posts, only: [:index, :show]
  resources :sitemap, only: [:index]
  root to: "posts#index"
end
{% endcodeblock %}

<p>
  Now let's add code to our controllers. First let's add the code necessary to list and display posts to the Posts controller. Open up your Posts controller and modify it so that it looks like the code listed below:
</p>

{% codeblock app/controllers/posts_controller.rb lang:ruby %}
class PostsController < ApplicationController
  def index
    @posts = Post.order("created_at DESC")
  end

  def show
    @post = Post.find(params[:id])
  end
end
{% endcodeblock %}

<p>
  Now lets add code to our Sitemap controller. The only code we need here is code to list the posts. Open up your Sitemap controller and modify it so that it looks like the code listed below.
</p>

{% codeblock app/controllers/sitemap_controller.rb lang:ruby %}
class SitemapController < ApplicationController
  respond_to :xml
  def index
    @posts = Post.order("created_at DESC")
  end
end
{% endcodeblock %}

<p>
  Now let's create the views. First lets create the index view for the Posts controller. Open up the index view and modify it so that i looks like the code listed below.
</p>

{% codeblock app/views/posts/index.html.erb lang:ruby %}
<h1>My Blog</h1>
<%  @posts.each do |post| %>
  <h3><%= link_to post.title, post %></h3>
  <p>
    <%= post.body.html_safe %>
  </p>
  <% if post != @posts.last %>
    <hr />
  <% end %>
<% end %>
{% endcodeblock %}

<p>
  Now let's create the show view. Open up the show view for the Posts controller and modify it so that it looks like the code listed below.
</p>

{% codeblock app/views/posts/show.html.erb lang:ruby %}
<h1><%= @post.title %></h1>
<p>
  <%= @post.body.html_safe %>
</p>
{% endcodeblock %}

<p>
  Now it's time for the sitemap. A sitemap is an XML file that typically consists of a number of url xml elements encapsulated by a urlset xml element. Each url typically has the 4 elements listed below.
</p>

XML | Description
--- | ---
loc | The actual url to the page you wish to list in the sitemap.
changefreq | How often the page changes, can be always, hourly, daily, weekly, monthly, yearly, or never.
priority | How important the page is within the context of your site.
lastmod | The date of the last modification of this page.
<br/>

<p>
  An example sitemap is listed below:
</p>

{% codeblock sitemap.xml lang:ruby %}
<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
  <url>
    <loc>http://localhost:3000/</loc>
    <changefreq>hourly</changefreq>
    <priority>1.0</priority>
  </url>
  <url>
    <loc>http://localhost:3000/posts/2</loc>
    <changefreq>daily</changefreq>
    <priority>0.8</priority>
    <lastmod>2014-03-04T17:01:15.37+00:00</lastmod>
  </url>
  <url>
    <loc>http://localhost:3000/posts/1</loc>
    <changefreq>daily</changefreq>
    <priority>0.8</priority>
    <lastmod>2014-03-04T17:01:15.36+00:00</lastmod>
  </url>
</urlset>
{% endcodeblock %}

<p>
  This example was generated from our application. To do this yourself, create a new view for our Sitemaps controller called index.xml.builder and add in the code listed below.
</p>

{% codeblock app/views/sitemap/index.xml.builder lang:ruby %}
xml.instruct!

xml.urlset(xmlns: "http://www.sitemaps.org/schemas/sitemap/0.9") do
  xml.url do
    xml.loc root_url
    xml.changefreq("hourly")
    xml.priority "1.0"
  end
  @posts.each do |post|  
    xml.url do
      xml.loc post_url(post)
      xml.changefreq("daily")
      xml.priority "0.8"
      xml.lastmod post.updated_at.strftime("%Y-%m-%dT%H:%M:%S.%2N%:z")
    end
  end
end
{% endcodeblock %}

<p>
  The first xml.url block adds an entry for our root url. In a more complex application we would probably want to add static entries for other pages. The next thing the code does is loop through each post and create a xml url element for each.
</p>

<p>
  You can preview your sitemap by starting your rails server and visiting http://localhost:3000/sitemap.xml.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
