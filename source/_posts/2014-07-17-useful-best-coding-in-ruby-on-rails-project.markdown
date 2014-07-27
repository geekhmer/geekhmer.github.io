---
layout: post
title: "Useful Best Coding in Ruby on Rails Project"
date: 2014-07-17 22:24
comments: true
categories: [Ruby on Rails]
keywords: Useful Best Coding in Ruby on Rails Project
---

<p>
  While a lot of techniques and libraries have come and gone as the community's preferred way of doing something, there are some best practices that remain, and can lead to writing the clean code, most secure and maintainable Ruby on Rails code possible.<br/><br/>
  Listed here there are the most popular and useful best coding you can use as a Ruby on Rails developer:
</p>

<p>
  <strong>Fat Model - Thin Controller</strong><br/>
  The most important ways to write clear and concise cod in Ruby on Rails is "Fat Model - Thin Controller". Model and Controller are parts of MVC(Model - View - Controller), many logic should go in the Model, and Controller is a nice interface between the view and Model.<br/><br/>
  In priactice, We should move any logic that isn't about the response(setting a flash message, redirect, render view) to the model instead of the controller, anyway we can reuse it where possible and make it possible for testing outside of the Controller.
</p>

<p>
  Let look at sample example we have code in brands_controller.rb:
</p>

{% codeblock brands_controller.rb lang:ruby %}
def index
  @brands = Brand.joins(:products).where('products.category_uuid = ?', 'AA43D840-C70B-11E3-9C51-B888E33867FC').uniq
end
{% endcodeblock %}

<p>
  We can refactor it like this:
</p>

{% codeblock brands_controller.rb lang:ruby %}
def index
  @brands = Brand.find_brands_by_category('AA43D840-C70B-11E3-9C51-B888E33867FC')
end
{% endcodeblock %}

<p>
  Then we can move the logic into model brand.rb:
</p>

{% codeblock brand.rb lang:ruby %}
def self.find_brands_by_category(uuid)
  Brand.joins(:products).where('products.category_uuid = ?', uuid).uniq
end
{% endcodeblock %}

<p>
  With the methods Brand.find_brands_by_category, we’ve not only made it simpler to test our code, we’ve also made it possible to reuse that same set of conditions in another location. But as we’ll see shortly, even this is still not ideal.
</p>

<p>
  <strong>Reusable Scopes and Relations</strong><br/>
  Ruby on Rails provides a better way -- scopes to avoid duplication condition in another methods.
</p>

<p>
  Let look at sample example we have code in brands.rb:
</p>

{% codeblock brand.rb lang:ruby %}
scope :under_portal, lambda{ |portal_uuid| where(portal_uuid: portal_uuid) unless portal_uuid.blank? }
scope :under_listing, lambda{ |alias_id| where(listing_alias_id: alias_id) unless alias_id.blank? }
scope :under_category, lambda{ |category_uuid| where(category_uuid: category_uuid) unless category_uuid.blank? }
scope :under_brand, lambda{ |brand_uuid| where(brand_uuid: brand_uuid) unless brand_uuid.blank? }

def self.set_scope(portal_uuid, listing_alias_id, category_uuid=nil, brand_uuid=nil)
  self.under_portal(portal_uuid).under_listing(listing_alias_id).under_category(category_uuid).under_brand(brand_uuid)
end
{% endcodeblock %}

<p>
  <strong>Virtual Attributes</strong><br/>
  If you find that you’re manipulating data before passing it to a model (for example, converting the type of an object), it’s likely time you started structuring your code to take advantage of virtual attributes.
</p>

<p>
  Virtual attributes are a very simple idea—essentially, all you’re doing is defining your own getter and setter methods.
</p>

<p>
  Let look at sample example we have code:
</p>

{% codeblock brand.rb lang:ruby %}
@user = User.new(params[:user])
@user.first_name, @user.last_name = params[:user][:full_name].split(" ", 2)
{% endcodeblock %}

<p>
  We could remove the second line, and instead add the following to our User model:
</p>

{% codeblock brand.rb lang:ruby %}
def full_name=(value)
  self.first_name, self.last_name = value.to_s.split(" ", 2)
end
{% endcodeblock %}

<p>
  <strong>Use the Built-in Ruby Duck Typing Methods</strong><br/>
  Ruby uses several conventions that can make development easier like implementing a to_s instance method on an object will give you a standard way of getting a string representation of your object.
</p>

<p>
  By implementing these standard type conversions—in addition to to_s, there’s also to_i for integers, let have a look at the following string interpolation:
</p>

{% codeblock brand.rb lang:ruby %}
"Hello there, #{user.name}"
{% endcodeblock %}

<p>
  <strong>Use Non-database-backed Models</strong><br/>
  Although models in Rails are mostly based on ActiveRecord::Base or some other type of object mapper for a database, it’s important to remember that in MVC, the M isn’t restricted to database-backed models.
</p>

<p>
  Using non-database-backed models can help to organize logic which might otherwise become muddy. For example, there are libraries that give you anActiveRecord-like interface for contact form emails.
</p>

<p>
  When it comes time to interact with these models in your controller code, your code will be that much cleaner, as you can use the exact same approach as with database-backed models.
</p>

<p>
  <strong>Package Your Code into Gems</strong><br/>
  I you've used Ruby on Rails, you've noticed the wealth of rubygems available to Rails developers.
  When you write code you think is general enough—which usually just means you’ve written it more than once before in another application, let extract it into a gem suitable for a wider range of purposes.
</p>

<p>
  So far so good, there are hundreds of coding practices or techniques that can make your life as a Ruby on Rails developer easier, but I’ve tried to pick out some. :)
</p>