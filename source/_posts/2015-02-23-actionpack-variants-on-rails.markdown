---
layout: post
title: "ActionPack Variants on Rails"
date: 2015-02-23 21:48
comments: true
categories: [Ruby on Rails, Ruby]
keywords: ActionPack Variants on Rails, Ruby on Rails ActionPack Variants, Rails ActionPack Variants
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="ActionPack Variants on Rails" />
</p>

<p>
  Rails 4.1 released a little while ago and came out with a fantastic new feature I think a lot of responsive web developers are going to flex. It's called Variants. ActionPack Variants.
</p>

<p>
  <strong>What does it do?</strong><br/>
  It allows you to create different views for different device formats - such as mobile 
  phones, tablets and desktops.
</p>

{% codeblock lang:ruby %}
# /app/views/home/index.html.erb
# /app/views/home/index.html+phone.erb
# /app/views/home/index.html+tablet.erb
{% endcodeblock %}

<p>
  <strong>Why is this useful?</strong><br/>
  This is tremendously useful because you can now serve a much slimmer version of your view to mobile devices, instead of just hiding the elements using CSS using media queries.
</p>

<p>
  Hidden elements still load and need to come down 'the wire' - using Variants it's much easier than before to serve up lean and mean views to mobile devices.
</p>

<p>
  <strong>Setup</strong><br/>
  Create a homes controller that will enable you to play around with ActionPack Variants, run the commands below to create the home controller:
</p>

{% codeblock lang:ruby %}
rails g controller homes show
{% endcodeblock %}

<p>
  Then apply a few simple routes to your application to finish setting things up. Open up your routes file (<code>config/routes.rb</code>).
</p>

{% codeblock lang:ruby %}
root to: "homes#show"
resource :home, only: [:show]
{% endcodeblock %}

<p>
  Well, Now we are ready to play around with ActionPack Variants. ActionPack Variants works by using a <code>before_action</code> in your application controller. Open up your Application Controller (<code>app/controllers/application_controller.rb</code>) and modify so that it looks sometings like:
</p>

{% codeblock application_controller.rb lang:ruby %}
class ApplicationController < ActionController::Base
  # Prevent CSRF attacks by raising an exception.
  # For APIs, you may want to use :null_session instead.
  protect_from_forgery with: :exception

  before_action :detect_browser

  private
    def detect_browser
      case request.user_agent
        when /iPad/i
          request.variant = :tablet
        when /iPhone/i
          request.variant = :phone
        when /Android/i && /mobile/i
          request.variant = :phone
        when /Android/i
          request.variant = :tablet
        when /Windows Phone/i
          request.variant = :phone
        else
          request.variant = :desktop
      end
    end
end
{% endcodeblock %}

<p>
  How does it work? Before an action is called, The <code>detect_browser</code> is called and the <code>request.variant</code> is set to whatever you want it to be. In the above example I use it determine whether we are running a mobile, tablet, or desktop device.
</p>

<p>
  Then create a view called show.html+phone.erb (<code>app/views/homes/show.html+phone.erb</code>) for your homes controller and add in the code listed below:
</p>

{% codeblock show.html+phone.erb lang:ruby %}
<h1>Phone</h1>
<p>
  You are running on a smart phone.
</p>
{% endcodeblock %}

<p>
  Then create a view for our tablet users. Create a new view called show.html+tablet.erb (<code>app/views/homes/show.html+tablet.erb</code>) and add in the code listed below:
</p>

{% codeblock show.html+tablet.erb lang:ruby %}
<h1>Tablet</h1>
<p>
  You are running on a tablet.  Whoohoo!
</p>
{% endcodeblock %}

<p>
  Then modify your default show.html.erb (<code>app/views/homes/show.html.erb</code>) view. Open it up now and modify it so that it looks something like:
</p>

{% codeblock show.html.erb lang:ruby %}
<h1>Desktop</h1>
<p>
  All other users get redirected here.
</p>
{% endcodeblock %}

<p>
   There are still a few pieces of the puzzle left. What if you need to detect the device type prior to rendering the view? For example, setting paging size/options for our queries. Fortunately, this is pretty easy. Open up your homes controller (<code>app/views/controllers/homes_controller.rb</code>) and modify it so that it looks like the code listed below:
</p>

{% codeblock homes_controller.rb lang:ruby %}
class HomesController < ApplicationController
  def show
    @device = "others"
    respond_to do |format|

        format.html.phone do # phone variant
          # add code here for phones
        end

        format.html.tablet do # tablet variant
          # add code here for tablets
        end

        format.html.desktop do 
          # add code here for desktops and other devices
        end
    end
  end
end
{% endcodeblock %}

<p>
  There is one more thing to do. What happens if you need to detect the variant in your view? Fortunately this is very easy and be shown,open up your application layout (<code>app/views/layouts/application.html.erb</code>) and modify it so that it looks like the code listed below:
</p>

{% codeblock application.html.erb lang:ruby %}
<!DOCTYPE html>
<html>
<head>
  <title>ActionPackVariantsExample</title>
  <%= stylesheet_link_tag    'application', media: 'all', 'data-turbolinks-track' => true %>
  <%= javascript_include_tag 'application', 'data-turbolinks-track' => true %>
  <%= csrf_meta_tags %>
</head>
<body>

<%= yield %>

<% if request.variant.include? :tablet %>
  <small>Welcome Tablet User</small>
<% elsif request.variant.include? :phone %>
  <small>Welcome Phone User</small>
<% end %>

</body>
</html>
{% endcodeblock %}

<p>
  Note the if statement starting with <code>if request.variant.include? :tablet</code>. Why do we use <code>.include?</code> The <code>request.variant</code> is actually an array. Therefore it is necessary to use .include?.
</p>

<p>
  So far so good, That's it! See ya! :)
</p>
