---
layout: post
title: "Good Rails3 ActiveRecord Finder Vs Very Good Rails4 ActiveRecord Finder"
date: 2014-08-05 23:39
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Rails3 ActiveRecord Finder, Rails4 ActiveRecord Finder, Very Good Rails4 ActiveRecord Finder, Good Rails3 ActiveRecord Finder Vs Very Good Rails4 ActiveRecord Finder
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" />
</p>

<p>
  Well, previouse article I had talked about "Keyword Arguments Feature in Ruby 2.0" now we can apply it with Rails4.
</p>

<p>
  <strong>FINDERS</strong>
</p>

<p>  
  <strong>old-style finders are deprecated</strong><br/>
  - Rails3: 
</p>

{% codeblock Rails3 lang:ruby %}
Post.find(:all, conditions: { author: 'admin'})
{% endcodeblock %}

<p>
  Warning: Calling #find(:all) is deprecated. Please call #all directly instead. You have also used finder options. Please build s scope instead of using finder options.
</p>

<p>
  - Rails4:
</p>

{% codeblock Rails4 lang:ruby %}
Post.where(author: 'admin')
{% endcodeblock %}

<p>
  <strong>Dynamic finder that return collections are deprecated</strong><br/>
  - Rails3:
</p>

{% codeblock Rails3 lang:ruby %}
Post.find_all_by_title('Rails 4')
Post.find_last_by_author('admin')
{% endcodeblock %}

<p>
  Warning: This dynamic method is deprecated. Please use e.g Post.where(...).all instead.
</p>

<p>
  - Rails4:
</p>

{% codeblock Rails4 lang:ruby %}
Post.where(title: 'Rails 4')
Post.where(author: 'admin').last
{% endcodeblock %}

<p>
  <strong>FIND_BY</strong>
</p>

<p>
- Rails3:
</p>

{% codeblock Rails3 lang:ruby %}
Post.find_by_title('Rails 4') # Dynamic find_by finders that take a single argument are not deprecated.
Post.find_by_title('Rails 4', conditions: { author: 'admin' }) # Dynamic find_by finders with conditions are deprecated.
{% endcodeblock %}

<p>
  - Rails4:
</p>

{% codeblock Rails4 lang:ruby %}
Post.find_by_title('Rails 4')
Post.find_by_title('Rails 4', conditions: { author: 'admin' })
{% endcodeblock %}

<p>
  <strong>FIND_BY WITH HASH</strong>
</p>

<p>
  <strong>allows dynamic input more easily</strong><br/>
  - Rails4:
</p>

{% codeblock Rails4 lang:ruby %}
post_params = { title: 'Rails 4', author: 'admin' }
Post.find_by(post_params) 

Post.find_by("published_on < ?", 2.weeks.ago)
{% endcodeblock %}

<p>
  <strong>FIND_OR_*</strong>
</p>

<p>
  <strong>dynamic finders that create new objects are deprecated</strong><br/>
  - Rails3:
</p>

{% codeblock Rails3 lang:ruby %}
Post.find_or_initialize_by_title('Rails 4')
Post.find_or_create_by_title('Rails 4')
{% endcodeblock %}

<p>
  Warning: This dinamic method is deprecated. Please use e.g Post.find_or_initialize_by(name: 'foo') instead.<br/>
  Warning: This dinamic method is deprecated. Please use e.g Post.find_or_create_by(name: 'foo') instead.
</p>

<p>
  - Rails4:
</p>

{% codeblock Rails4 lang:ruby %}
Post.find_or_initialize_by(title: 'Rails 4')
Post.find_or_create_by(title: 'Rails 4')
{% endcodeblock %}

<p>
  So far so good, let upgrade to Rails4 then refactor ActiveRecord Finder together. :)
</p>