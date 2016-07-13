---
layout: post
title: "Ruby on Rails Build a Simple Rake Task"
date: 2015-04-20 10:46
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Ruby on Rails Build a Simple Rake Task
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails Build a Simple Rake Task" />
</p>

<p>
  There are many tasks that need to be performed outside of your website that are related to your web application. An example would be cleaning up temporary files. You wouldn't want to have this code running in a web page. Fortunately rails includes a mechanism for doing this. Rake tasks make it easy to automate various aspects of your application.
</p>

<p>
  First, we need to create a couple models for our sample. In this sample we will have 2 models. Our first model, Product, will store product information. Our second model, Review, will be a review of the product. Run the commands below to generate these 2 models:
</p>

{% codeblock lang:ruby %}
rails g model Product name average_rating:float price:decimal{12,2} active:boolean
rails g model Review product:references user rating:integer body:text
rake db:migrate
{% endcodeblock %}

<p>
  Next, we need some seed data. Open up your seeds.rb file and add the following code:
</p>

{% codeblock db/seeds.rb lang:ruby %}
Product.delete_all
Review.delete_all

Product.create!([
  {id: 1, name: "Die Hard - Blu-Ray", price: 9.95, active: true},
  {id: 2, name: "Iron Man 3 - Blu-Ray", price: 24.95, active: true},
  {id: 3, name: "Star Trek - Into Darkness - Blu-Ray", price: 19.95, active: true},
  {id: 4, name: "The Little Mermaid - Blu-Ray", price: 29.95, active: true},
  {id: 5, name: "This is the End - Blu-Ray", price: 17.95, active: true}
])

Review.create!([
  {id: 1, product_id: 1, user: "Dan", rating: 5, body: "Epic Action Flick"},
  {id: 2, product_id: 1, user: "Will", rating: 4, body: "The Stunts were AMAZING!"},
  {id: 3, product_id: 2, user: "James", rating: 2, body: "I didn't like it as much as the first one."},
  {id: 4, product_id: 2, user: "Lisa", rating: 5, body: "Epic!"},
  {id: 5, product_id: 3, user: "Linda", rating: 5, body: "A classic revived!  Well worth watching again."},
  {id: 6, product_id: 4, user: "Kathy", rating: 5, body: "This movie is hilarious!"},
  {id: 7, product_id: 5, user: "Jim", rating: 3, body: "Really cheesy."}
])
{% endcodeblock %}

<p>
  Then run rake db:seed
</p>

{% codeblock lang:ruby %}
rake db:seed
{% endcodeblock %}

<p>
  Now, lets open up the Product model so that we can add an association to reviews. Modify the Product model so that it looks like the code listed below:
</p>

{% codeblock app/models/product.rb lang:ruby %}
class Product < ActiveRecord::Base
  has_many :reviews
end
{% endcodeblock %}

<p>
  Then create a file called calculate_averages.rake in the lib/tasks folder and add the following code:
</p>

{% codeblock lib/tasks/calculate_averages.rake lang:ruby %}
require 'rake'

task :calculate_averages => :environment do
  products = Product.all

  products.each do |product|
    puts "Calculating average rating for #{product.name}..."
    product.update_attribute(:average_rating, product.reviews.average("rating"))
  end
end
{% endcodeblock %}

<p>
  Now, if we run our rake command we will see that the averages are updated in the database.
</p>

{% codeblock lang:ruby %}
rake calculate_averages
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
