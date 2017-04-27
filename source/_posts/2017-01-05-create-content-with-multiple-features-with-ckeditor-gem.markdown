---
layout: post
title: "Create content with multiple features with CKEditor Gem"
date: 2017-01-05 23:13
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Create content with multiple features with CKEditor Gem, CKEditor, CKEditor Gem
---

<p>
  <img src="/images/ckeditor.png" width="600" alt="Create content with multiple features with CKEditor Gem" />
</p>

<p>
  <strong>Setup & Usage</strong><br/>
  First, we need to add the CKEditor gem to our Gemfile. Open up your Gemfile and add the line listed below:
</p>

{% codeblock Gemfile lang:ruby %}
gem "ckeditor"
{% endcodeblock %}

<p>
  Next, open up the terminal and run a bundle install to install the gem:
</p>

{% codeblock lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  Great, now lets create a sample model and accompanying controller that will be used to store our data. Open up and run the command below to create the Magazine model and migrate the database:
</p>

{% codeblock lang:ruby %}
rails g resource Magazine title body:text
rake db:migrate
{% endcodeblock %}

<p>
  Now, open your routes file and add the following line to your routes:
</p>

{% codeblock routes.rb lang:ruby %}
root to: "magazines#index"
{% endcodeblock %}

<p>
  Now, lets add the CKEditor javascript include to our application.js. Modify your application.js file so that it looks like the code listed below:
</p>

{% codeblock application.js lang:ruby %}
// This is a manifest file that'll be compiled into application.js, which will include all the files
// listed below.
//
// Any JavaScript/Coffee file within this directory, lib/assets/javascripts, vendor/assets/javascripts,
// or any plugin's vendor/assets/javascripts directory can be referenced here using a relative path.
//
// It's not advisable to add code directly here, but if you do, it'll appear at the bottom of the
// compiled file. JavaScript code in this file should be added after the last require_* statement.
//
// Read Sprockets README (https://github.com/rails/sprockets#sprockets-directives) for details
// about supported directives.
//
//= require jquery
//= require jquery_ujs
//= require ckeditor/init
//= require turbolinks
//= require_tree .
{% endcodeblock %}

<p>
  Great, now we need to add some code to the magazines controller. Add the following code to the magazines controller:
</p>

{% codeblock magazines_controller.rb lang:ruby %}
class MagazinesController < ApplicationController
  def index
    @magazines = Magazine.order("created_at DESC")
  end

  def show
    @magazine = Magazine.find(params[:id])
  end

  def new
    @magazine = Magazine.new
  end

  def create
    @magazine = Magazine.new(magazine_params)
    if @magazine.save
      redirect_to magazines_path, notice: "The magazines has been successfully created."
    else
      render action: "new"
    end
  end

  def edit
    @magazine = Magazine.find(params[:id])
  end

  def update
    @magazine = Magazine.find(params[:id])
    if @magazine.update_attributes(magazine_params)
      redirect_to magazines_path, notice: "The magazine has been successfully updated."
    else
      render action: "edit"
    end
  end

  private
  def magazine_params
    params.require(:magazine).permit(:title, :body)
  end
end
{% endcodeblock %}

<p>
  This code enables the ability to read, write, and update the magazines in our example. Now for the views, first lets create the index view:
</p>

{% codeblock index.html.erb lang:ruby %}
<%= link_to "New Magazine", new_magazine_path %>
<% @magazines.each do |magazine| %>
  <h3><%= magazine.title.html_safe %></h3>
  <p><%= magazine.body.html_safe %></p>
  <%= link_to "Edit Magazine", edit_magazine_path(magazine) %>
  <% if magazine != @magazines.last %>
  <hr />
  <% end %>
<% end %>
{% endcodeblock %}

<p>
  Now, lets create a partial to store the form. Create a file called app/views/_form.html.erb and add the code listed below:
</p>

{% codeblock _form.html.erb lang:ruby %}
<% if @magazine.errors.any? %>
  <ul>
    <%= @magazine.errors.full_messages.each do |message| %>
        <li><%= message %></li>
    <% end %>
  </ul>
<% end %>
<%= form_for @magazine do |f| %>
  <div>
    <%= f.label :title %>
  </div>
  <div>
    <%= f.text_field :title %>
  </div>
  <div>
    <%= f.label :body %>
  </div>
  <div>
    <%= f.cktext_area :body, rows: 10 %>
  </div>
  <div>
    <%= f.submit %>
  </div>
<% end %>
{% endcodeblock %}

<p>
  Now, lets create the new view. Create the app/views/magazines/new.html.erb file and add the code listed below:
</p>

{% codeblock new.html.erb lang:ruby %}
<h3> New Magazine</h3>
<%= render "form" %>
{% endcodeblock %}

<p>
  Now, if you visit the new magazines page on your development server you will see that CKEditor appears.
</p>

<p>
  Next lets create the edit view. Create the app/views/magazines/edit.html.erb file and add the code listed below:
</p>

{% codeblock edit.html.erb lang:ruby %}
<%= "Editing #{@magazine.title}" %>
<%= render "form" %>
{% endcodeblock %}

<p>
  Great, now when you click 'edit magazine' on any magazine, it will show the CKEditor for editing.
</p>

<p>
  <strong>Images Using Paperclip</strong><br/>
  In order to integrate images via paperclip, a few more steps are required. Note that you must have ImageMagick installed for this to work. First, lets include the paperclip gem:
</p>

{% codeblock Gemfile lang:ruby %}
gem "paperclip"
{% endcodeblock %}

<p>
  Next, we need to run a generator provided by ckeditor. This generator will create the necessary models that will be used to store image data. Run the command below:
</p>

{% codeblock lang:ruby %}
rails generate ckeditor:install --orm=active_record --backend=paperclip
rake db:migrate
{% endcodeblock %}

<p>
  So far so good, if you restart your rails server and refresh the page, you will be able to click the images button, upload an image, and insert it into your articles.
</p>
