---
layout: post
title: "IP Geolocation in Ruby on Rails Application"
date: 2015-05-16 23:47
comments: true
categories: [Ruby, Ruby on Rails]
keywords: IP Geolocation in Ruby on Rails Application
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="IP Geolocation in Ruby on Rails Application" />
</p>

<p>
  Sometimes it can be useful to find a user's physical location in our Rails application. For instance, maybe we want to send a special Happy Holidays to users of a particular country. IP address geolocation lets us do exactly that. With IP address geolocation you can get a pretty good idea of where the customer is accessing your site from as long as they aren't using a proxy server or some other means of obscuring their IP address. In this article we will show you how to utilize the Maxmind GeoIP database to look up the location of just about any IP address. Let's get started.
</p>

<p>
  <strong>Setup Rails Application</strong><br/>
  To utilize the IP address geolocation functionality, we first need to add the geoip gem to our gemfile. Open up your gemfile and add in the code listed below:
</p>

{% codeblock Gemfile lang:ruby %}
gem 'geoip', '~> 1.4.0'
{% endcodeblock %}

<p>
  Great, now let's run a bundle install to install the gem.
</p>

{% codeblock lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  Now we need to download the GeoIP database for use with the geoip gem. A free one <a href="http://geolite.maxmind.com/download/geoip/database/GeoLiteCity.dat.gz" target="_blank"> can be downloaded at this link</a>, or you can use the paid version if you have a subscription. Once downloaded, extract the compressed archive and place the .dat folder contained within in the root directory of your Rails app.
</p>

<p>
  Great, now let's create a controller so that we can play around with the GeoIP functionality. Run the commands below to create a controller called GeoIpRequest:
</p>

{% codeblock lang:ruby %}
rails g controller geo_ip_request new create
{% endcodeblock %}

<p>
  Next, open up your routes file and modify it so that it looks like the code listed below:
</p>

{% codeblock config/routes.rb lang:ruby %}
Rails.application.routes.draw do
  resource :geo_ip_request, controller: :geo_ip_request

  root to: "geo_ip_request#new"
end
{% endcodeblock %}

<p>
  Great, now open up your GeoIpRequest controller and modify it so that it looks like the code listed below.
</p>

{% codeblock app/controllers/geo_ip_request_controller.rb lang:ruby %}
class GeoIpRequestController < ApplicationController
  def new
  end

  def create
    require 'geoip'
    @info = GeoIP.new(Rails.root.join("GeoLiteCity.dat")).city(ip_request_params[:host])
  end

  private
  def ip_request_params
    params.require(:request).permit(:host)
  end
end
{% endcodeblock %}

<p>
  In the code above, we tell the geoip gem to load our database file and do a search for the host parameter.
</p>

<p>
  Now let's create our views. Open up the new view for the GeoIpRequest controller and modify it so that it looks like the code listed below:
</p>

{% codeblock app/views/geo_ip_request/new.html.erb lang:ruby %}
<h1>GeoIP Example</h1>
<p>Get the country for any ip address or hostname by typing it below and pressing the lookup button.</p>

<%= form_for :request, url: geo_ip_request_index_path do |f| %>
  <%= f.text_field :host %>
  <%= f.submit "Lookup" %>
<% end %>
{% endcodeblock %}

<p>
  Great, now finally let's do the create view. Open up the create view for the GeoIpRequest controller and modify it so that it looks like the code listed below.
</p>

{% codeblock app/views/geo_ip_request/create.html.erb lang:ruby %}
<h1>IP Address Info</h1>
<b>IP:&nbsp;&nbsp;</b><%= @info.request %><br />
<b>Country:&nbsp;&nbsp;</b><%= @info.country_name %><br />
<b>City:&nbsp;&nbsp;</b><%= @info.city_name %><br />
<b>Region:&nbsp;&nbsp;</b><%= @info.real_region_name %>
{% endcodeblock %}

<p>
  Excellent, now if you fire up your rails development server and and navigate to http://localhost:3000 you will see a form requesting your ip address or hostname. Entering any ip address or host name will show the country, city, and region that ip is from.
</p>

<p>
  So far so good, that's it!!! that's all there is to it!!! :)
</p>
