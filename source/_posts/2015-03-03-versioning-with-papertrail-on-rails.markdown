---
layout: post
title: "Versioning with PaperTrail on Rails"
date: 2015-03-03 21:28
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Versioning with PaperTrail on Rails
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Versioning with PaperTrail on Rails" />
</p>

<p>
  Imagine the situation, You open your website’s admin page to do some clean up, find some old data that hasn’t been viewed by anyone for ages, and delete it. Deletion succeeds and everything is okay, but after a second "No!!! That data contained VERY IMPORTANT INFORMATION that could possibly change the world!", you realize. But, it’s gone and the world remains unchanged (well, there is still a chance for recovery if you have a backup of the Database).
</p>

<p>
  Can we prevent this situation in our Rails application? Yes, for sure we can. Well, in this article, we are going to talk about how to implement a "History" page and an "undo" button (as well as "redo") with the help of the <code>paper_trail</code> gem.
</p>

<p>
  We are going to build a simple blog that allows to add, update, and destroy posts. And would be able to undo every action related to the posts (for example, undoing an unintentional delete). We will also provide a "History" page displaying a list of actions (and some other info) that users performed while working with posts.
</p>

<p>
  <strong>Create Rails Project</strong><br/>
  Run command below to create a new Rails application:
</p>

{% codeblock lang:ruby %}
rails new blog -d mysql
{% endcodeblock %}

<p>
  <strong>Add Gems</strong><br/>
  We will add a gems to our Gemfile. paper_trail, it will help us to create both the “History” page and an “undo” button:
</p>

{% codeblock Gemfile lang:ruby %}
gem 'paper_trail', '~> 3.0.1'
{% endcodeblock %}

<p>
  The blog app has one controller (apart from the ApplicationController), PostsController, that will be used to manage our posts. Initially, it has seven default methods: index, new, create, destroy, edit and update, along with a number of views related to them. I will not go into the details on how to create these methods and views, because they are really basic (you can create them using rails g scaffold Posts).
</p>

<p>
  The blog contains a model: Post. The posts table have the following columns: id, title, body, created_at, updated_at.
</p>

<p>
  And routes file will looks like:
</p>

{% codeblock routes.rb lang:ruby %}
root to: 'posts#index'
resources :posts
{% endcodeblock %}

<p>
  At this point, we are ready to setup paper_trail. First of all, create a special table to store the versions (it’s a cool feature of paper_trail – if we want to clear old versions later, we would need only to access one table).
</p>

<p>
  Run command below to generate and migrate paper_trail:
</p>

{% codeblock lang:ruby %}
bundle exec rails generate paper_trail:install
bundle exec rake db:migrate
{% endcodeblock %}

<p>
  Create and apply the required migration. Add the following line to your Post model (app/models/post.rb):
</p>

{% codeblock post.rb lang:ruby %}
class Post < ActiveRecord::Base
  has_paper_trail
end
{% endcodeblock %}

<p>
  Well! Now, all changes to the posts table will be audited automatically. How cool is that?
</p>

<p>
  You can also specify which changes should not be tracked. For example, if posts had a view_count column in and we did not want to track changes to it, the modification looks like in (app/models/post.rb):
</p>

{% codeblock post.rb lang:ruby %}
has_paper_trail ignore: [:view_count]
{% endcodeblock %}

<p>
  Some fields can be skipped, they will neither be tracked nor included in the serialized version of the object (app/models/post.rb):
</p>

{% codeblock post.rb lang:ruby %}
has_paper_trail skip: [:view_count]
{% endcodeblock %}

<p>
  Or can specified as well (app/models/post.rb):
</p>

{% codeblock post.rb lang:ruby %}
has_paper_trail on: [:update, :create]
{% endcodeblock %}

<p>
  Or provide conditions on whether to track the event:
</p>

{% codeblock post.rb lang:ruby %}
has_paper_trail if: Proc.new { |t| t.title.length > 10 },
                unless: Proc.new { |t| t.body.blank? }
{% endcodeblock %}

<p>
  <strong>Display the Log</strong><br/>
  Now, you probably want to display the audited versions, It’s easy, just, add the following route:
</p>

{% codeblock routes.rb lang:ruby %}
root to: 'posts#index'
resources :posts
get '/posts/history', to: 'posts#history', as: :posts_history
{% endcodeblock %}

<p>
  To the history page for Posts. If your app has many models that are being audited, it’s a good idea to create a separate VersionsController and place all versions-related methods there. However, in our case, only the Post model is being audited, so let’s stick with one controller.
</p>

<p>
  Add a history method to the controller (app/controllers/posts_controller.rb):
</p>

{% codeblock posts_controller.rb lang:ruby %}
def history
  @versions = PaperTrail::Version.order('created_at DESC')
end
{% endcodeblock %}

<p>
  Note that we have to use PaperTrail::Version, not just Version. This line of code extracts all the recorded events from the versions table that we have created earlier and sorts them by the creation date. In a real app, paginating these events by using will_paginate or kaminari gem is advisable.
</p>

<p>
  Rendering the view history (app/views/posts/history.html.erb):
</p>

{% codeblock history.html.erb lang:ruby %}
{% raw %}
<div class="container">
  <h1>History</h1>
 
  <ul>
    <% @versions.each do |version| %>
      <li>
        <%= l(version.created_at, format: "%-d.%m.%Y %H:%M:%S %Z") %><br/>
        Event ID: <%= version.id %><br/>
        <b>Target:</b> <%= version.item_type %>
        <small>(id: <%= version.item_id %>)</small>; <b>action</b> <%= version.event %>;<br/>
        <div>
          More info:
          <pre><%= version.object %></pre>
        </div>
      </li>
    <% end %>
  </ul>
</div>
{% endraw %}
{% endcodeblock %}

<p>
  Here is the data being displayed:
</p>

Data | Description
--- | ---
version.created_at | When this event took place.
version.id | ID of this event.
version.item_type | Model name for the event. In our case, it’s Post.
version.item_id | ID of the resource (Post) that was changed.
version.event | The action applied to the resource (create, update, destroy).version.object, Full dump of the resource that was changed.

<p>
  <br/>
  There are some things that could be improved. For example, which fields were changed (especially for the update action)? Well, that is very easy to implement.
</p>

<p>
  Run command below to create and apply migration:
</p>

{% codeblock lang:ruby %}
rails g migration add_object_changes_to_versions object_changes:text
rake db:migrate
{% endcodeblock %}

<p>
  This can also be done when setting up paper_trail. You just need to provide an appropriate option to the generator below:
</p>

{% codeblock lang:ruby %}
rails generate paper_trail:install --with-changes
{% endcodeblock %}

<p>
  No further action is required, paper_trail will automatically diff the versions.
</p>

<p>
  Now, we can add a new div with version.changeset block to our view (app/views/posts/history.html.erb):
</p>

{% codeblock history.html.erb lang:ruby %}
[...]
<div>
  Changeset:
  <pre><%= version.changeset %></pre>
</div>
[...]
{% endcodeblock %}

<p>
  It will display attribute values before and after the event (if an attribute remained unchanged it will not be displayed).
</p>

<p>
  <strong>Track User-Specific Information</strong><br/>
  Okay, now we know when of our precious blog post deletion. But, we don’t know who, High time to fix this issue.
</p>

<p>
  Let’s track an IP address of the user responsible for the action. Of course, an IP address can be forged, but the main point here is to explain how to store metadata alongside with the event’s data. Go on and create a new migration below:
</p>

{% codeblock lang:ruby %}
rails g migration add_ip_to_versions ip:string
rake db:migrate
{% endcodeblock %}

<p>
  paper_trail will not store anything in the ip column, by default, so we need to help it out a bit. Add this method to the ApplicationController (app/controllers/application_controller.rb):
</p>

{% codeblock application_controller.rb lang:ruby %}
def info_for_paper_trail
  { ip: request.remote_ip }
end
{% endcodeblock %}

<p>
  paper_trail will use this method to fetch some additional info and store it as metadata. If you are using Rails 3 or protected_attributes with Rails 4, you will also need to create an initializer (initializers/paper_trail.rb):
</p>

{% codeblock paper_trail.rb lang:ruby %}
module PaperTrail
  class Version < ActiveRecord::Base
    attr_accessible :ip
  end
end
{% endcodeblock %}

<p>
  Metadata can also be provided in the model (app/models/post.rb) like this (presuming we have a timestamp column):
</p>

{% codeblock post.rb lang:ruby %}
has_paper_trail meta: { timestamp: Time.now }
{% endcodeblock %}

<p>
  The last thing to do is add a line into view (app/views/posts/history.html.erb):
</p>

{% codeblock history.html.erb lang:ruby %}
[...]
<b>Remote address:</b> <%= version.ip %><br/>
[...]
{% endcodeblock %}

<p>
  <strong>Undo an Action</strong><br/>
  Let’s move on, allowing the user to undo their actions. Create undo method in posts controller (app/controllers/posts_controller.rb) that will undo the requested action:
</p>

{% codeblock posts_controller.rb lang:ruby %}
def undo
  @post_version = PaperTrail::Version.find_by_id(params[:id])
 
  begin
    if @post_version.reify
      @post_version.reify.save
    else
      @post_version.item.destroy
    end
    flash[:success] = "Undid that..."
  rescue
    flash[:alert] = "Failed undoing the action..."
  ensure
    redirect_to root_path
  end
end
{% endcodeblock %}

<p>
  The above code, find a version by id and then check if there are previous versions available for the resource using the reify method, this method will return nil if the resource was just created in the current version (obviously, if the resource was just created it does not have any previous versions.) Either rollback to the previous version using @post_version.reify.save or destroy the newly created resource using @post_version.item.destroy (the @post_version.item returns the actual resource).
</p>

<p>
  And routes file will looks like:
</p>

{% codeblock routes.rb lang:ruby %}
root to: 'posts#index'
resources :posts
get '/posts/history', to: 'posts#history', as: :posts_history
post '/posts/:id/undo', to: 'posts#undo', as: :undo
{% endcodeblock %}

<p>
  Then put this undo link into the flash message, so make sure to render it somewhere in your layout (app/views/layouts/application.html.erb):
</p>

{% codeblock application.html.erb lang:ruby %}
{% raw %}
<div class="container">
  <% flash.each do |key, value| %>
    <div class="alert alert-<%= key %>">
      <button type="button" class="close" data-dismiss="alert">&times;</button>
      <%= value.html_safe %>
    </div>
  <% end %>
</div>
{% endraw %}
{% endcodeblock %}

<p>
  Then create a private method in the PostsController that generates an undo link:
</p>

{% codeblock posts_controller.rb lang:ruby %}
[...]
private
def make_undo_link
  view_context.link_to 'Undo that plz!', undo_path(@post.versions.last), method: :post
end
{% endcodeblock %}

<p>
  We cannot use link_to inside the controller, so the reference to view_context points to the actual view. @post.versions fetches all the versions for the Post resource and @post.versions.last gets the latest one. This method can be used like this in PostsController (app/controllers/posts_controller.rb):
</p>

{% codeblock posts_controller.rb lang:ruby %}
[...]
def update
  @post = Post.find_by_id(params[:id])
  if @post.update_attributes(post_params)
    flash[:success] = "Post was updated! #{make_undo_link}"
    redirect_to post_path(@post)
  else
    render 'edit'
  end
end
[...]
{% endcodeblock %}

<p>
  Make sure to add it to the create and destroy methods as well.
</p>

<p>
  Okay, I have undone an action… but now I want to redo it. Here, we should introduce a redo link that reverts the undo. There are only few modifications needed.
</p>

<p>
  Then create another private method in PostsController (app/controllers/posts_controller.rb):
</p>

{% codeblock posts_controller.rb lang:ruby %}
[...]
private
def make_redo_link
  params[:redo] == "true" ? link = "Undo that plz!" : link = "Redo that plz!"
  view_context.link_to link, undo_path(@post_version.next, redo: !params[:redo]), method: :post
end
[...]
{% endcodeblock %}

<p>
  This method is very similar to make_undo_link. The main difference is the params[:redo] which is true of false. Based on this parameter, change the text of the link – the URL actually remains unchanged. This is because redoing basically means reverting to the previous version, which is absolutely the same as the undo action.
</p>

<p>
  Then alter the flash message inside the undo method in PostsController:
</p>

{% codeblock posts_controller.rb lang:ruby %}
[...]
def undo
[...]
  flash[:success] = "Undid that! #{make_redo_link}"
[...]
{% endcodeblock %}

<p>
  That’s it, users can undo and redo their actions as many times and they want, every time being recorded by paper_trail.
</p>

<p>
  The only one problem is that the versions table can become fat very quickly. This should probably be handled with some background process to remove old entries. The job would use something like:
</p>

{% codeblock lang:ruby %}
PaperTrail::Version.delete_all ["created_at < ?", 1.week.ago]
{% endcodeblock %}

<p>
  You also can limit the number of created versions per object by adding this line into an initializer:
</p>

{% codeblock lang:ruby %}
PaperTrail.config.version_limit = 3
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>