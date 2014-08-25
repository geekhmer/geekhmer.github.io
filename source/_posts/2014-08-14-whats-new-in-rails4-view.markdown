---
layout: post
title: "What's New in Rails4 View?"
date: 2014-08-14 08:37
comments: true
categories: [Ruby on Rails, Ruby]

keywords: What's New in Rails4 View?, Rails4 View, What's New in Rails4?
description: What's New in Rails4 View?
---

<p>
  <img src="/images/what_is_new_in_rails4.png" width="500" />
</p>

<p>
  Well, previouse article I had talked about <a href="http://geekhmer.github.io/blog/2014/08/09/whats-new-in-rails4-activemodel/">What's New in Rails4 ActiveModel?</a>.
  Today We are looking at view:
</p>

<p>
  Assume we have an owner class which has many items and each items are usualy belongs to an owner:
</p>

{% codeblock lang:ruby %}
class Owner < ActiveRecord::Base
  has_many :items
end
{% endcodeblock %}

{% codeblock lang:ruby %}
class Item < ActiveRecord::Base
  belongs_to :owner
end
{% endcodeblock %}

<p>
  <strong>Select box</strong><br/>
  - Rails3 & 4<br/>
  In Rails3 & 4 if we want to build a select box with owner we could do it with a single method called:
</p>

{% codeblock lang:ruby %}
collection_select(:item, :owner_id, Owner.all, :id, :name)
{% endcodeblock %}

<p>
  <strong>Radio button & checkbox</strong><br/>
  - Rails3<br/>
  In Rails3 we need do with the loops and builds each of the elements:
</p>

{% codeblock lang:ruby %}
<% @owners.each do |owner| %>
  <%= radio_button_tag :owner_id, owner.id %>
  <%= owner.name %>
<% end %>
{% endcodeblock %}

<p>
  HTML output: 
</p>

{% codeblock lang:ruby %}
<input id="owner_id" name="owner_id" type="radio" value="1" /> Slow-draw
<input id="owner_id" name="owner_id" type="radio" value="2" /> Sheriff
{% endcodeblock %}

<p>
  - Rails4<br/>
  Now in Rails4 we have <code>collection_radio_buttons & collection_check_boxes</code> method which builds all elements from a collection:
</p>

{% codeblock lang:ruby %}
collection_radio_buttons(:item, :owner_id, Owner.all, :id, :name)
collection_check_boxes(:item, :owner_id, Owner.all, :id, :name)
{% endcodeblock %}

<p>
  <strong>Date field</strong><br/>
  - Rails3<br/>
  At some points we must use <code>date_select</code> form helper:
</p>

{% codeblock lang:ruby %}
<%= f.date_select :return_date %>
{% endcodeblock %}

<p>
  HTML output:
</p>

{% codeblock lang:ruby %}
<select id="item_return_date_1i" name="item[return_date(1i)]">
  <option value="2008">2008</option>
  ...
</select>
<select id="item_return_date_2li" name ="item[return_date(2i)]">
  <option selected="selected" value="1">January</option>
  ...
</select>
<select id="item_return_date_3i" name="item[return_date(3i)]">
  ...
</select>
{% endcodeblock %}

<p>
  - Rails4<br/>
  Rails4 now there is a date_field:
</p>

{% codeblock lang:ruby %}
<%= f.date_field :return_date %>
{% endcodeblock %}

<p>
  HTML output:
</p>

{% codeblock lang:ruby %}
<input id="item_return_date" name="item[return_date]" type="date">
{% endcodeblock %}

