---
layout: post
title: "Ruby on Rails Uploads Multiple Files with DropzoneJS and Paperclip Gem"
date: 2015-02-10 21:37
categories: [Ruby on Rails, Ruby]
keywords: Ruby on Rails Uploads Multiple Files with DropzoneJS and Paperclip Gem, Rails Uploads Multiple Files with DropzoneJS and Paperclip Gem, Ruby on Rails Uploads Multiple Files with DropzoneJS, Rails Uploads Multiple Files with DropzoneJS, Ruby on Rails Uploads Files with DropzoneJS and Paperclip Gem, Rails Uploads Files with DropzoneJS and Paperclip Gem, Ruby on Rails Uploads Files with DropzoneJS, Rails Uploads Files with DropzoneJS
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails Uploads Multiple Files with DropzoneJS and Paperclip Gem" />
</p>

<p>
  <a href="http://www.dropzonejs.com/" target="_blank">DropzoneJS</a> is a javascript library for allowing multiple file uploads via AJAX. It features drag and drop support, folder support, and much more on browsers that support these features.
</p>

<p>
  In this article I will show you how to implement multiple images files uploads directly to paperclip using DropzoneJS. Let’s run through this with me.
</p>

<p>
  <strong>Create Rails Project</strong><br/>
  To create a Rails project; open up your terminal and type commands below: 
</p>

{% codeblock lang:ruby %}
rails new dropzone -d mysql
{% endcodeblock %}

<p>
  <strong>Add Gems</strong><br/>
  We will add two gems to our Gemfile. <code>dropzonejs-rails</code> gem is a helper gem that integrates DropzoneJS into our Rails app. <code>paperclip</code> for processing image uploads.
</p>

<p>
  Open up your <code>Gemfile</code> and add in the lines listed below:
</p>

{% codeblock Gemfile lang:ruby %}
gem "paperclip", "~> 4.2"
gem 'dropzonejs-rails'
{% endcodeblock %}

<p>
  Now let's run a bundle install to install the gems:
</p>

{% codeblock lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  <strong>Create a Image Model</strong><br/>
  Now we will create a model to store our image information for Paperclip. Run the command below to create the image model and migrate the database:
</p>

{% codeblock lang:ruby %}
rails g model image avatar:attachment
rake db:migrate
{% endcodeblock %}

<p>
  Then add some code to Image model to tell paperclip we want to have an attachment attached. Open up your image model (<code>app/models/image.rb:</code>) and add the code listed below:
</p>

{% codeblock image.rb lang:ruby %}
class Image < ActiveRecord::Base
  has_attached_file :avatar, :styles => { :medium => "300x300>", :thumb => "100x100>" }, :default_url => "/images/:style/missing.png"
  validates_attachment_content_type :avatar, :content_type => /\Aimage\/.*\Z/
end
{% endcodeblock %}

<p>
  <strong>Create a Images Controller</strong><br/>
  Then create an Images controller which will be used to display and allow the upload of our images. Run the command below to create this controller:
</p>

{% codeblock lang:ruby %}
rails g controller images index create
{% endcodeblock %}

<p>
  Then update our routes file to set up the routes for our images controller. Open up the routes file (<code>config/routes.rb</code>) and modify it:
</p>

{% codeblock routes.rb lang:ruby %}
Rails.application.routes.draw do
  resources :images, only: [:index, :create]
  root to: "images#index"
end
{% endcodeblock %}

<p>
  Then modify our Images controller to add logic to handle the file upload as well as listing each of the images. Open up the Images controller (<code>app/controllers/images_controller.rb</code>) and modify it:
</p>

{% codeblock images_controller.rb lang:ruby %}
class ImagesController < ApplicationController
  def index
    @images = Image.all
    @image = Image.new
  end

  def create
    @image = Image.new(image_params)

    if @image.save
      render json: { message: "success", fileID: @image.id }, status: 200
    else
      render json: { error: @image.errors.full_messages.join(',')}, status: 400
    end     
  end
  
  private
  def image_params
    params.require(:image).permit(:avatar)
  end
end
{% endcodeblock %}

<p>
  DropzoneJS expects a json return, so the create method returns a JSON success or failure based on whether the image was uploaded successfully or not.
</p>

<p>
  Then add Bootstrap to our application. Open up your application layout (<code>app/views/layouts/application.html.erb</code>)and modify it:
</p>

{% codeblock application.html.erb lang:ruby %}
<!DOCTYPE html>
<html>
<head>
  <title>DropzoneJS</title>
  <%= stylesheet_link_tag    'application', media: 'all', 'data-turbolinks-track' => true %>
  <%= javascript_include_tag 'application', 'data-turbolinks-track' => true %>
  <%= stylesheet_link_tag    'http://yandex.st/bootstrap/3.1.1/css/bootstrap.min.css', media: 'all', 'data-turbolinks-track' => true %>
  <%= javascript_include_tag 'http://yandex.st/bootstrap/3.1.1/js/bootstrap.min.js', 'data-turbolinks-track' => true %>
  <%= csrf_meta_tags %>
</head>
<body>
  <%= yield %>
</body>
</html>
{% endcodeblock %}

<p>
  Well, then create our views. First let's create the index view (<code>app/views/images/index.html.erb</code>). Open up your index view for the images controller and modify it:
</p>

{% codeblock index.html.erb lang:ruby %}
<h1>My Images</h1>

<%= form_for(Image.new, html: { multipart: true, class: "dropzone"}) do |f|  %>
  <div class="fallback">
    <%= f.file_field :avatar %><br>
    <%= f.submit "Upload my Avatar" %>
  </div>
<% end %>

<div class="index">
  <%= render "index" %>
</div>
{% endcodeblock %}

<p>
  Then we need to add some JavaScript to tell Rails how to handle the remote ajax file processing that we will do using dropzone. Create a view called <code>app/views/images/index.js.erb</code> for your images controller and add the code listed below:
</p>

{% codeblock index.js.erb lang:ruby %}
$(".index").html("<%= escape_javascript(render('index')) %>")
{% endcodeblock %}

<p>
  Then create the partial that we reference in the previous code. Create a new partial called <code>app/views/images/_index.html.erb</code> for your images controller and add the code listed below:
</p>

{% codeblock _index.html.erb lang:ruby %}
<% @images.each do |image| %>
  <div class="img-thumbnail">
    <%= image_tag image.avatar.url(:thumb), alt: image.avatar.url(:thumb) %>
  </div>
<% end %>
{% endcodeblock %}

<p>
  Then modify our application.css and add the dropzone css require. Open up your <code>app/assets/stylesheets/application.css</code> file and modify it:
</p>

{% codeblock application.css lang:ruby %}
/*
 *= require_tree .
 *= require dropzone/dropzone
 *= require_self
 */
{% endcodeblock %}

<p>
  Then modify our application.js and add the dropzone js require. Open up your <code>app/assets/javascripts/application.js</code> file and modify it:
</p>

{% codeblock application.js lang:ruby %}
/= require dropzone
{% endcodeblock %}

<p>
  Then add a bit more JavaScript to finish things up. Open up your <code>app/assets/javascripts/images.js</code> file and add in the code listed below:
</p>

{% codeblock images.js lang:ruby %}
$(document).ready(function(){
  // disable auto discover
  Dropzone.autoDiscover = false;
 
  var dropzone = new Dropzone (".dropzone", {
    maxFilesize: 256, // set the maximum file size to 256 MB
    paramName: "image[avatar]", // Rails expects the file upload to be something like model[field_name]
    addRemoveLinks: false // don't show remove links on dropzone itself.
  }); 

  dropzone.on("success", function(file) {
    this.removeFile(file);
    $.getScript("/images");
  })
});
{% endcodeblock %}

<p>
  So far so good, if you start your Rails server and navigate to http://localhost:3000 you will notice that you can drag and drop images onto the app. On certain browsers, such as Google Chrome, you can even drag and drop one or more folders of images onto the dropzone placeholder and have them upload. In addition you can also click the dropzone and select a file via the file selection screen. That's it! See ya! :)
</p>
