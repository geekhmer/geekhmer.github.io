---
layout: post
title: "Using UUID as Primary Key in Ruby on Rails with MySQL Guide"
date: 2014-12-06 22:12
comments: true
categories: [Ruby on Rails]
keywords: Using UUID as Primary Key in Ruby on Rails with MySQL Guide, Ruby on Rails MySQL UUID Primary Key Guide, Using UUIDs as Primary Keys on Rails, Use UUIDs in Rails with MySQL, Ruby, Ruby on Rails, Rails 4, Ruby on Rails 4
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails MySQL UUID Primary Key Guide" />
</p>

<p>
  A UUID (universally unique identifier) is an identifier standard used in software construction. A UUID is simply a 128-bit value. The meaning of each bit is defined by any of several variants.
</p>

<p>
  For human-readable display, many systems use a canonical format using hexadecimal text with inserted hyphen characters. For example: de305d54-75b4-431b-adb2-eb6b9e546013.
</p>

<p>
  The intent of UUIDs is to enable distributed systems to uniquely identify information without significant central coordination.
</p>

<p>
  <strong>Installation</strong><br/>
  To use UUID as Primary Key you need to add uuidtools gem to your app's Gemfile:
</p>

{% codeblock Gemfile lang:ruby %}
gem 'uuidtools'
{% endcodeblock %}

<p>
  <strong>Setting up UUID as Primary Key</strong><br/>
  To set UUID as Primary Key, you need to set id to false and the new UUID column as Primary Key in migration file:
</p>

{% codeblock *_create_products.rb lang:ruby %}
class CreateProducts < ActiveRecord::Migration
  def change
    create_table :products, id: false do |t|
      t.string :uuid, limit: 36, primary: true, null: false
      t.string :name, limit: 50, null: false

      t.timestamps
    end
  end
end
{% endcodeblock %}

<p>
  <strong>Inserting UUID Value into UUID Column</strong><br/>
  Create UUID helper library in app/lib directory:
</p>

{% codeblock uuid_helper.rb lang:ruby %}
module UuidHelper
  def self.included(base)
    base.primary_key = 'uuid'
    base.before_create :assign_uuid
  end
  
  private
  def assign_uuid
    self.uuid = UUIDTools::UUID.timestamp_create().to_s.upcase if uuid.blank?
  end
end
{% endcodeblock %}

<p>
  And then include UuidHelper library in Model file:
</p>

{% codeblock product.rb lang:ruby %}
class Product < ActiveRecord::Base
  include UuidHelper
end
{% endcodeblock %}
