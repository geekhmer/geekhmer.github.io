---
layout: post
title: "Respond with JSON in Ruby on Rails Application"
date: 2015-04-15 15:06
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Respond with JSON in Ruby on Rails Application
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Translate Your Ruby on Rails Application" />
</p>

<p>
  When building a RESTful API in Rails application, there are many different options and gems you can use to format your JSON responses. This isn't a post about how to build an API, but rather about some of the different popular options on how to define and structure JSON.
</p>

<p>
  <strong>RABL</strong><br/>
  <a href="http://www.leighhalliday.com/responding-with-json-in-rails" target="_blank">RABL</a> is a DSL for generating both JSON and XML. In my mind it has a similar feel to Jbuilder, which I'll discuss below. It's works by creating a view with the extension .rabl, and defining which attributes, nodes, and relations you wish to include in the JSON response.
</p>

<p>
  Here's an example:
</p>

{% codeblock app/views/posts/index.rabl lang:ruby %}
collection @posts
attributes :id, :title, :subject
child(:user) { attributes :full_name }
node(:read) { |post| post.read_by?(@user) }
{% endcodeblock %}

<p>
  <strong>Active Model Serializer</strong><br/>
  <a href="https://github.com/rails-api/active_model_serializers" target="_blank">Active Model Serializer</a> is a great way to build JSON responses using an object oriented approach. The objects have a very similar feel to how your ActiveModel object is set up in terms of attributes and relationships. It also allows you to choose your adapter-to decide what type of JSON structure is produced-or to build your own. Popular supported formats are JSON-API and JSON-HAL.
</p>

{% codeblock lang:ruby %}
class PostSerializer < ActiveModel::Serializer
  attributes :title, :body
  has_many :comments
  url :post
end
{% endcodeblock %}

<p>
  <strong>Jbuilder</strong><br/>
  <a href="https://github.com/rails/jbuilder" target="_blank">jbuilder</a> provides a very similar DSL to RABL. Jbuilder is included with Rails, so it is used quite a bit. Rails Casts has a free episode which goes into greater detail about Jbuilder. It's very easy to use and provides a lot of flexibility in defining exactly what attributes are included in and how the response is formatted and nested.
</p>

{% codeblock app/views/message/show.json.jbuilder lang:ruby %}
json.content format_content(@message.content)
json.(@message, :created_at, :updated_at)

json.author do
  json.name @message.creator.name.familiar
  json.email_address @message.creator.email_address_with_name
  json.url url_for(@message.creator, format: :json)
end
{% endcodeblock %}

<p>
  <strong>Grape Entity</strong><br/>
  <a href="https://github.com/intridea/grape-entity" target="_blank">Grape Entity</a> was extracted from Grape, which is a popular gem used for building RESTful APIs. Similarly to RABL and Jbuilder, it provides a DSL for defining entities which are the structure of your JSON response.
</p>

{% codeblock lang:ruby %}
module API
  module Entities
    class Status < Grape::Entity
      expose :user_name
      expose :text, documentation: { type: "String", desc: "Status update text." }
      expose :ip, if: { type: :full }
      expose :user_type, :user_id, if: lambda { |status, options| status.user.public? }
      expose :contact_info do
        expose :phone
        expose :address, using: API::Address
      end
    end
  end
end
{% endcodeblock %}

<p>
  <strong>ROAR</strong><br/>
  <a href="https://github.com/apotonick/roar" target="_blank">ROAR</a> allows you to build presenter classes to represent your data. It comes with support for JSON, JSON-HAL, JSON-API, and XML.
</p>

{% codeblock lang:ruby %}
require 'roar/json'

module SongRepresenter
  include Roar::JSON
  property :title
end
{% endcodeblock %}

<p>
  <strong>ActiveModel or Hash</strong><br/>
  This may seem like a strange thing to point out, but for very simple cases, you can simply call the to_json method on either an ActiveModel object or a native Ruby Hash.
</p>

{% codeblock lang:ruby %}
# Using an @organization model
respond_to do |format|
  format.json do
    render json: @organization.to_json
  end
end
{% endcodeblock %}

{% codeblock lang:ruby %}
# Using a plain Ruby Hash
respond_to do |format|
  format.json do
    render json: {
      name: @user.name,
      email: @user.email
    }.to_json
  end
end
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
