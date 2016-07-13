---
layout: post
title: "Liquid on Rails"
date: 2015-02-21 22:59
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Liquid on Rails, Liquid Templating Engine on Rails, Ruby on Rails Liquid Templating Engine, Rails Liquid Templating Engine
---

<p>
  <img src="/images/move_to_rails.png" alt="Liquid on Rails" />
</p>

<p>
  Now you have a basic grasp on the Liquid template engine itself, let's learn how to utilize it in our Ruby on Rails application. First you need to add the gem to the Gemfile. Add the following line to the Gemfile:
</p>

{% codeblock lang:ruby %}
gem 'liquid', '~> 2.6.1'
{% endcodeblock %}

<p>
  Then run <code>bundle install</code> to install the Liquid gem:
</p>

{% codeblock lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  Then create a model called Page. The Page model will be used to represent a page owned by a specific user. Run the commands below to create this model:
</p>

{% codeblock lang:ruby %}
rails g model Page user:string template:text
rake db:migrate
{% endcodeblock %}

<p>
  Then open up your seeds (<code>db/seeds.rb</code>) file and modify it so that it looks somethings like below:
</p>

{% codeblock seeds lang:ruby %}
{% raw %}
Page.delete_all

Page.create(id: 1, user: "JingLong", template: %{
  Hello {{ user }}, here is your shopping list.
  <ul>
    {% for item in list %}
      <li>{{ item.name }}</li>
    {% endfor %}
  </ul>
}

)

Page.create(id: 2, user: "Bunlong", template: %{
  What is up my man? Here is your shopping list.
  <ul>
    {% for item in list %}
      <li>{{ item.name }}</li>
    {% endfor %}
  </ul>
}
)

Page.create(id: 3, user: "Rubyist", template: %{
  HTTP 200:  Shopping List Found

  Items in your list:
  <ul>
    {% for item in list %}
      <li>{{ item.name }}</li>
    {% endfor %}
  </ul>
}

)
{% endraw %}
{% endcodeblock %}

<p>
  Then run <code>rake db:seed</code> command to seed the database with your sample data:
</p>

{% codeblock lang:ruby %}
rake db:seed
{% endcodeblock %}

<p>
  Then create a homes controller that will allow you to play with liquid. Run the command below to create the homes controller:
</p>

{% codeblock lang:ruby %}
rails g controller Homes show
{% endcodeblock %}

<p>
  Then create a pages controller. This controller will be used to display a user's page. Run the commands below to create the pages controller:
</p>

{% codeblock lang:ruby %}
rails g controller Pages show
{% endcodeblock %}

<p>
  Then modify your routes (<code>config/routes.rb</code>) file so that it looks somethings like below:
</p>

{% codeblock routes.rb lang:ruby %}
root to: "homes#show"
resource :home, only: [:show]
resources :pages, only: [:show]
{% endcodeblock %}

<p>
  Then modify your homes controller (<code>app/controllers/homes_controller.rb</code>) to pull down a list of all the user's pages. Open up your homes controller and modify it so that it looks somethings like below:
</p>

{% codeblock homes_controller.rb lang:ruby %}
class HomesController < ApplicationController
  def show
    @pages = Page.all
  end
end
{% endcodeblock %}

<p>
  Then modify your pages controller (<code>app/controllers/pages_controller.rb</code>) to pull down the correct page for the specified user. Open up your pages controller and modify it so that it looks somethings like below:
</p>

{% codeblock pages_controller.rb lang:ruby %}
class PagesController < ApplicationController
  def show
    @page = Page.find(params[:id])
  end
end
{% endcodeblock %}

<p>
  Then create a few helper (<code>app/helpers/pages_helper.rb</code>) methods for use with this example. Open up your pages helper and modify it so that it looks somethings like below:
</p>

{% codeblock pages_helper.rb lang:ruby %}
module PagesHelper
  def shopping_list(user)
    {"user" => user, "list" => shopping_list_items}
  end

  def shopping_list_items
    [
      { "name" => 'Apple', "quantity_needed" => "2"},
      { "name" => 'Stoberry', "quantity_needed" => "1"},
      { "name" => 'Cherry', "quantity_needed" => "3"},
    ]
  end
end
{% endcodeblock %}

<p>
  The codes above gives you some sample data to play around with.
</p>

<p>
  Then modify your pages show view (<code>pp/views/pages/show.html.erb</code>) to look somethings like below:
</p>

{% codeblock show.html.erb lang:ruby %}
<h1><%= @page.user %>'s Personal Page</h1>

<% template = Liquid::Template.parse(@page.template) %>

<%= template.render(shopping_list(@page.user)).html_safe %>
{% endcodeblock %}

<p>
  The codes above tells Rails to render your template using the Liquid Template Engine. 
</p>

<p>
  Finally, open up the show view for your home controller (<code>app/views/homes/show.html.erb</code>) and add in the code listed below:
</p>

{% codeblock show.html.erb lang:ruby %}
<h1>User Pages</h1>
<ul>
  <% @pages.each do |page| %>
    <li><%= link_to "#{page.user}'s page", page_path(page) %></li>
  <% end %>
</ul>
{% endcodeblock %}

<p>
  If you fire up a rails server and visit your development site, you'll notice that you can browse each user's pages and get a customized version of each one. 
</p>

<p>
  However, what if you want to use the Liquid templating engine as a replacement for erb itself? take it easy, first, create a new file in the lib folder called liquid_view.rb (<code>lib/liquid_view.rb</code>) and add in the codes listed below:
</p>

{% codeblock liquid_view.erb lang:ruby %}
class LiquidView
  def self.call(template)
    "LiquidView.new(self).render(#{template.source.inspect}, local_assigns)"
  end

  def initialize(view)
    @view = view
  end

  def render(template, local_assigns = {})
    @view.controller.headers["Content-Type"] ||= 'text/html; charset=utf-8'

    assigns = @view.assigns

    if @view.content_for?(:layout)
      assigns["content_for_layout"] = @view.content_for(:layout)
    end
    assigns.merge!(local_assigns.stringify_keys)

    controller = @view.controller
    filters = if controller.respond_to?(:liquid_filters, true)
                controller.send(:liquid_filters)
              elsif controller.respond_to?(:master_helper_module)
                [controller.master_helper_module]
              else
                [controller._helpers]
              end

    liquid = Liquid::Template.parse(template)
    liquid.render(assigns, :filters => filters, :registers => {:action_view => @view, :controller => @view.controller})
  end

  def compilable?
    false
  end
end
{% endcodeblock %}

<p>
  Create an initializer called liquid_template_handler (<code>config/initializers/liquid_template_handler.rb</code>) and add the code listed below:
</p>

{% codeblock liquid_template_handler.erb lang:ruby %}
require 'liquid_view'
ActionView::Template.register_template_handler :liquid, LiquidView
{% endcodeblock %}

<p>
  Let restarting your Rails server you will be able to create actual views with the .liquid extension that uses the liquid templating engine.
</p>

<p>
  So far so good, That's it! See ya! :)
</p>