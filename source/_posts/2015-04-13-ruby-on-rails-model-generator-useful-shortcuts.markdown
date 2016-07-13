---
layout: post
title: "Ruby on Rails Model Generator Useful Shortcuts"
date: 2015-04-13 13:11
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Ruby on Rails Renaming a Database Column
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails Renaming a Database Column" />
</p>

<p>
  <code>rails generate</code> command provides a lot of useful functionality, however some of it may not be immediately known to you as a developer. In this article we will explore a number of useful shortcuts available in the rails model generator.
</p>

<h3>Basics</h3>

<p>
  Let's start with the basic command line usage.
</p>

{% codeblock lang:ruby %}
rails g model User
{% endcodeblock %}

<p>
  <code>rails g</code> is the same thing as rails generate. Both commands do the same thing. We will use this shortcut throughout this article.
</p>

{% codeblock lang:ruby %}
rails g model Product name quantity:integer
{% endcodeblock %}

<p>
  This command generates a model named product with 2 fields, name, which is a string, and quantity, which is an integer. By not specifying the type for name, rails defaults to using string. Below is a complete list of types that you can use with the modal generator.
</p>

<p>
  <strong>Field Type Lists</strong><br/>
  - integer<br/>
  - primary_key<br/>
  - decimal<br/>
  - float<br/>
  - boolean<br/>
  - binary<br/>
  - string<br/>
  - text<br/>
  - date<br/>
  - time<br/>
  - datetime<br/>
  - timestamp
</p>

<p>
  You can also specify the size of a field as seen below.
</p>

{% codeblock lang:ruby %}
rails g model Client name:string{ 100 }
{% endcodeblock %}

<p>
  This will create a name field with a limit of 100 characters. For the decimal type, you can specify a precision and scale value as well.
</p>

{% codeblock lang:ruby %}
rails g model Product name price:decimal{ 12, 2 }
{% endcodeblock %}

<p>
  <strong>Namespaced Models</strong><br/>
  You can create namespaced models as well. This is useful for example, in creating a special set of administrative users that are separate from your regular users. Running the command below will place the user model in the Admin namespace, which will have a prefixed table name of admin_ in the database.
</p>

{% codeblock lang:ruby %}
rails g model admin/user
{% endcodeblock %}

<p>
  As you can see from the code listed below, the user belongs to the admin namespace like mentioned earlier.
</p>

{% codeblock app/models/admin/user.rb lang:ruby %}
class Admin::User < ActiveRecord::Base
end
{% endcodeblock %}

<p>
  <strong>Adding an Index</strong><br/>
  You can also add a database index right from the command line.
</p>

{% codeblock lang:ruby %}
rails g model Site name:string:index
{% endcodeblock %}

<p>
  In addition, you can make the index unique.
</p>

{% codeblock lang:ruby %}
rails g model Client name:string:uniq
{% endcodeblock %}

<h3>Model Relationships</h3>

<p>
  You can specify a basic relationship between models.
</p>

{% codeblock lang:ruby %}
rails g model User client:references
{% endcodeblock %}

<p>
  This will create a user with a column named client_id, add an index, and automatically add a belongs_to relationship to the User model.
</p>

<p>
  You can also make the relationship polymorphic.
</p>

{% codeblock lang:ruby %}
rails g model picture imageable:references{ polymorphic }
{% endcodeblock %}

<p>
  This will set up a polymorphic relationship for pictures. Polymorphic relationships allow you to 'share' a table between many different models. For instance, Products and People can both have pictures.
</p>

<p>
  The rails model generator exposes a lot of useful functionality that can save time if used properly. Thanks for reading!
</p>
