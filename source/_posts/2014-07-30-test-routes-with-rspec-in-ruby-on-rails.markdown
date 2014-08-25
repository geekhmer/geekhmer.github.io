---
layout: post
title: "Test Routes with RSpec in Ruby on Rails"
date: 2014-07-30 22:47
comments: true
categories: [testing, Ruby on Rails]

keywords: test routes, testing routes, test routes with rspec in ruby on rails, rails
description: Test Routes with RSpec in Ruby on Rails
---

<p>
  Most Ruby on Rails developers don’t test their routes, they focus on Model testing, Controller testing, Features or View testing, Helper testing, and try to catch every possible scenarios.<br/>I would like to show how to test route separately. 
</p>

<p>
  Sample routes.rb:
</p>

{% codeblock routes.rb lang:ruby %}
resources :products
{% endcodeblock %}

<p>
  Here is routes lists thats have been created:
</p>

{% codeblock lang:ruby %}
     root        /                            products#index
    posts GET    /products(.:format)          products#index
          POST   /products(.:format)          products#create
 new_post GET    /products/new(.:format)      products#new
edit_post GET    /products/:id/edit(.:format) products#edit
     post GET    /products/:id(.:format)      products#show
          PUT    /products/:id(.:format)      products#update
          DELETE /products/:id(.:format)      products#destroy
{% endcodeblock %}

<p>
  Testing for each routes in routing/products_routing_spec.rb:
</p>

{% codeblock products_routing_spec.rb lang:ruby %}
require 'spec_helper'

describe "routing to products" do
  it "routes /products to products#index" do
    expect(get: "/products").to route_to(
      controller: "products",
      action: "index"
    )
  end

  it "routes /products/1 to products#show" do
    expect(get: "/products/1").to route_to(
      controller: "products",
      action: "show",
      id: "1"
    )
  end

  it "routes /products/new to products#new" do
    expect(get: "/products/new").to route_to(
      controller: "products",
      action: "new"
    )
  end

  it "routes /products to products#create" do
    expect(post: "/products").to route_to(
      controller: "products",
      action: "create"
    )
  end

  it "routes /products/1/edit to products#edit" do
    expect(get: "/products/1/edit").to route_to(
      controller: "products",
      action: "edit",
      id: "1"
    )
  end

  it "routes /products/1 to products#update" do
    expect(put: "/products/1").to route_to(
      controller: "products",
      action: "update",
      id: "1"
    )
  end

  it "routes /products/1 to products#destroy" do
    expect(delete: "/products/1").to route_to(
      controller: "products",
      action: "destroy",
      id: "1"
    )
  end
end
{% endcodeblock %}

<p>
  Testing unroutable:
</p>

{% codeblock routes.rb lang:ruby %}
resources :products, except: [:show]
{% endcodeblock %}

{% codeblock products_routing_spec.rb lang:ruby %}
it "does not routes /products/1 to products#show" do
  expect(:get => "posts/1").not_to be_routable
end
{% endcodeblock %}

<p>
  So far so good, Let enjoy the routes testing in your Ruby on Rails application. :)
</p>