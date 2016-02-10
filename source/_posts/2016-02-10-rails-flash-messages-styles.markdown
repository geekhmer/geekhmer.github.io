---
layout: post
title: "Rails Flash Messages Styles"
date: 2016-02-10 10:57
comments: true
categories: [Rails]
keywords: Rails Flash Messages Styles
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="Rails Flash Messages Styles" />
</p>

<p>
  If you're using Ruby on Rails with Twitter Bootstrap or other css framworks, then you may want to display flash messages with the alert styles. Here is a quick and easy way of doing so.
</p>

<p>
  You just need to quickly extend <code>application_helper.rb</code> with the following:
</p>

{% codeblock application_helper.rb lang:ruby %}
module ApplicationHelper
  def bootstrap_class_for(flash_type)
    case flash_type
      when "success"
        "alert-success"
      when "error"
        "alert-danger"
      when "alert"
        "alert-warning"
      when "notice"
        "alert-info"
      else
        flash_type.to_s
      end
  end
end
{% endcodeblock %}

<p>
  Now when you call a flash message, you can use the following in your view:
</p>

{% codeblock lang:ruby %}
<% flash.each do |type, message| %>
  <div class="alert <%= bootstrap_class_for(type) %> alert-dismissible" role="alert">
    <button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>
    <%= message %>
  </div>
<% end %>
{% endcodeblock %}

<p>
  And just use:
</p>

{% codeblock lang:ruby %}
flash[:success] = "Credit card type saved successfully!"
{% endcodeblock %}

<p>
  As success message:
</p>

{% codeblock lang:ruby %}
flash[:alert] = "Alerting you to the monkey on your car!"
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
