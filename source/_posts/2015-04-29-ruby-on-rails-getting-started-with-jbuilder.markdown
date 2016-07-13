---
layout: post
title: "Ruby on Rails Getting Started with JBuilder"
date: 2015-04-29 20:20
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Ruby on Rails Getting Started with JBuilder
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails Getting Started with JBuilder" />
</p>

<p>
  In the past, it could be really cumbersome to build proper JSON data feeds. Fortunately Ruby on Rails makes dealing with JSON much easier. Today we will learn how to use the JBuilder gem, which will allow us to easily build complex data feeds.
</p>

<p>
  First, we need to include the gem in our gemfile. Recent versions of Ruby on Rails include this gem commented out towards the bottom of the file. In addition, Rails 4 already includes this gem so you don't need to do anything. If it is in your gemfile and it is commented out, uncomment it, otherwise, add the gem using the line shown below.
</p>

{% codeblock Gemfile lang:ruby %}
gem 'jbuilder', '~> 1.2'
{% endcodeblock %}

<p>
  Then run a bundle install:
</p>

{% codeblock lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  Next, we will need a model and some sample data. In this sample project we will have 2 models. A product model, and a review model, which will be associated with products. Lets generate these 2 models now. Run the commands below to create the models and migrate the database:
</p>

{% codeblock lang:ruby %}
rails g model Product name price:decimal{12,2} active:boolean
rails g model Review product:references user rating:integer body:text
rake db:migrate
{% endcodeblock %}

<p>
  Excellent, Now lets add some seed data. Copy and paste the following seed data into your seeds.rb file:
</p>

{% codeblock db/seeds.rb lang:ruby %}
Product.delete_all
Review.delete_all

Product.create!([
  {id: 1, name: "Nintendo Wii U Premium", price: 250, active: true},
  {id: 2, name: "XBox 360 250GB", price: 250, active: true},
  {id: 3, name: "Playstation 3 500 GB", price: 239.95, active: true},
  {id: 4, name: "Nintendo Wii", price: 99.95, active: true},
  {id: 5, name: "Nintendo 3DS", price: 174.95, active: true}
])

Review.create!([
  {id: 1, product_id: 1, user: "Bob", rating: 3, body: "dated graphics.  Overpriced.  However, the games are awesome."},
  {id: 2, product_id: 1, user: "Rich", rating: 4, body: "MARIO!  'nuff Said"},
  {id: 3, product_id: 2, user: "James", rating: 5, body: "Excellent value for the money."},
  {id: 4, product_id: 2, user: "Alison", rating: 5, body: "Love it!"},
  {id: 5, product_id: 3, user: "James", rating: 4, body: "Bigger hard drive then my XBox 360.  Weak user interface though."},
  {id: 6, product_id: 4, user: "Kay", rating: 1, body: "Extremely dated.  Don't buy.  Will be discontinued soon."},
  {id: 7, product_id: 5, user: "Jed", rating: 4, body: "Awesome handheld system, but a bit overpriced."}
])
{% endcodeblock %}

<p>
  Next, run the seed command to populate your database:
</p>

{% codeblock lang:ruby %}
rake db:seed
{% endcodeblock %}

<p>
  Well, we now have a database populated with products and reviews. Next we will need to set up the association between our products and our reviews. First, open up the Product model and modify it so that it looks like the code listed below:
</p>

{% codeblock app/models/product.rb lang:ruby %}
class Product < ActiveRecord::Base
  has_many :reviews
end
{% endcodeblock %}

<p>
  Great, now lets add the reverse association. Open up your Review model and verify that it looks like the code listed below. Rails should auto populate this, but double check just to be sure.
</p>

{% codeblock app/models/reviews.rb lang:ruby %}
class Review < ActiveRecord::Base
  belongs_to :product
end
{% endcodeblock %}

<p>
  With the database model created, our data populated, and our associations set up, now it's time to create a controller. Run the command below to create products controller.
</p>

{% codeblock lang:ruby %}
rails g controller products index
{% endcodeblock %}

<p>
  Now, lets make a change to our routes.rb file so that our controller becomes a resourceful controller. Replace the line that says get "products/index" with the code listed below.
</p>

{% codeblock config/routes.rb lang:ruby %}
# get "products/index"
resources :products, only: [:index]
{% endcodeblock %}

<p>
  This sets up the products controller as a resourceful controller. Now, in order to create a json feed using jbuilder, we must create a jbuilder view. Create a new file in app/views/products called index.json.jbuilder and add the code listed below.
</p>

{% codeblock app/views/products/index.json.jbuilder lang:ruby %}
json.products do
end
{% endcodeblock %}

<p>
  Excellent. Now if you navigate to http://localhost:3000/products.json You will see an empty json data feed. Now, lets add some content to that data feed. Now, open up the products controller and modify it so that it looks like the code listed below.
</p>

{% codeblock app/controllers/products_controller.rb lang:ruby %}
class ProductsController < ApplicationController
  def index
    @products = Product.all
  end
end
{% endcodeblock %}

<p>
  Next, Open the jbuilder file again and modify it so that it looks like the code listed below.
</p>

{% codeblock app/views/products/index.json.jbuilder lang:ruby %}
json.products @products do |product|
  json.name product.name
  json.price number_to_currency product.price
  json.active product.active
end
{% endcodeblock %}

<p>
  Excellent, now if we refresh the page we will see a listing of products in json format. Now lets add the reviews. Modify your code so that it looks like the code listed below.
</p>

{% codeblock app/views/products/index.json.jbuilder lang:ruby %}
json.products @products do |product|
  json.name product.name
  json.price number_to_currency product.price
  json.active product.active

  json.reviews product.reviews do |review|
    json.user review.user
    json.rating review.rating
    json.body review.body
  end
end
{% endcodeblock %}

<p>
  Excellent! Now if we refresh the page we'll see the review listing for each product. That's it folks! That's all there is to using the jbuilder gem.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
