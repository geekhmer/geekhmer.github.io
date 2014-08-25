---
layout: post
title: "What's New in Rails4 ActiveModel?"
date: 2014-08-09 00:16
comments: true
categories: [Ruby on Rails, Ruby]

keywords: ActiveModel, Rails4 ActiveModel, What's New in Rails4 ActiveModel?, What's New in Rails4?
description: What's New in Rails4 ActiveModel?
---

<p>
  <img src="/images/what_is_new_in_rails4.png" width="500" />
</p>

<p>
  Well, previouse article I had talked about <a href="http://geekhmer.github.io/blog/2014/08/05/good-rails3-activerecord-finder-vs-very-good-rails4-activerecord-finder/">What's New in Rails4 ActiveRecord Finder?</a>.
  Today please keep going to take a look "What's New in Rails4 ActiveModel?":
</p>

<p>
  <strong>SCOPES</strong><br/>
</p>
<p>
  <strong>eager-evaluated scopes are deprecated</strong><br/>
  - Rails3:
</p>

{% codeblock Rails3 lang:ruby %}
scope :sold, where(state: 'sold')
default_scope where(state: 'available')
{% endcodeblock %}

<p>
  Warning:<br/>
  - Useing #scope without passing a callable object is deprecated.<br/>
  - Calling #default_scope without a block is deprecated.
</p>

<p>
  - Rails4:<br/>
   Scopes should take a proc object:
</p>

{% codeblock Rails4 lang:ruby %}
scope :sold, ->{ where(state: 'sold') }
{% endcodeblock %}

<p>
  Defaults scopes should take proc object or a block:
</p>

{% codeblock Rails4 lang:ruby %}
default_scope { where(state: 'available') }
default_schop ->{ where(state: 'available') }
{% endcodeblock %}

<p>
  <strong>RELATION#NONE</strong><br/>
</p>

<p>
  - Rails3:
</p>

{% codeblock Rails3 lang:ruby %}
class User < ActiveRecord::Base
  def visible_posts 
    case role
    when 'Country Manager'
      Post.where(country: country)
    when 'Reviewer'
      Post.published
    when 'Bad User'
      [] #represents empty collection
    end
  end
end
{% endcodeblock %}

{% codeblock lang:ruby %}
  @posts = current_user.visible_posts
  @posts.recent
{% endcodeblock %}

<p>
  @posts.recent error when 'Bad User' because NoMethodError: undefined method 'recent' for []:Array.<br/>
  One way we can fix this:
</p>

{% codeblock lang:ruby %}
@posts = current_user.visible_posts
if @posts.any? # must check for presence
  @posts.recent
else
  [] # must return empty collection to caller
end
{% endcodeblock %}

<p>
  Rails4:
</p>

{% codeblock Rails4 lang:ruby %}
class User < ActiveRecord::Base
  def visible_posts
    case role
    when 'Country Manager'
      Post.where(country: country)
    when 'Reviewer'
      Post.published
    when 'Bad User'
      Post.none # returns ActiveRecord:Relation and never hits the database
    end
  end
end
{% endcodeblock %}

{% codeblock lang:ruby %}
@posts = current_user.visible_posts
@posts.recent # no need to check for presence
{% endcodeblock %}

<p>
Post.none returns ActiveRecord:Relation and never hits the database and @posts.recent no need to check for presence.
</p>

<p>
  <strong>RELATION#NOT</strong>
</p>

<p>
  Rails3: 
</p>

{% codeblock Rails3 lang:ruby %}
Post.where('author != ?', author)
{% endcodeblock %}

<p>
  When author is nil it's going to generate incorrect SQL syntax: <code>SELECT "posts".* FROM "posts" WHERE (author != NULL)</code><br/>
  One way we can fix this:
</p>

{% codeblock Rails3 lang:ruby %}
if author
  Post.where('author != ?', author)
else
  Post.where('author IS NOT NULL')
end
{% endcodeblock %}

<p>
  Rails4: 
</p>

{% codeblock Rails4 lang:ruby %}
Post.where.not(author: author)
{% endcodeblock %}

<p>
  When author is nill it's goint to generate correct SQL syntax: <code>SELECT "posts".* FROM "posts" WHERE (author IS NOT NULL)</code>
</p>

<p>
  <strong>RELATION#ORDER</strong>
</p>

<p>
  <strong>case1</strong>
</p>

<p>
  Rails3:
</p>

{% codeblock Rails3 lang:ruby %}
class User < ActiveRecord:Base
  default_scope { order(:name) }
end

User.order("created_at DESC")
{% endcodeblock %}

<p>
  It's going to generate SQL: <code>SELECT * FROM users ORDER BY name asc, created_at desc</code>, new calls to order are appended.
</p>

<p>
  Rails4:
</p>

{% codeblock Rails4 lang:ruby %}
class User < ActiveRecord:Base
  default_scope ->{ order(:name) }
end

User.order(created_at: :desc)
{% endcodeblock %}

<p>
  It's going to generate SQL: <code>SELECT * FROM users ORDER BY created_at desc</code>, name asc New calls to order are prepend.
</p>

<p>
  <strong>case2</strong>
</p>

<p>
  Rails3:
</p>

{% codeblock Rails3 lang:ruby %}
User.order(:name, 'created_at DESC')
{% endcodeblock %}

<p>
  Rails4:
</p>

{% codeblock Rails4 lang:ruby %}
User.order(:name, created_at: :desc)
{% endcodeblock %}

<p>
It's going to generate SQL: <code>SELECT * FROM users ORDER BY name asc, created_at desc</code>
</p>

<p>
  Rails3:
</p>

{% codeblock Rails3 lang:ruby %}
User.order('created_at DESC')
{% endcodeblock %}

<p>
  Rails4:
</p>

{% codeblock Rails4 lang:ruby %}
User.order(created_at: :desc)
{% endcodeblock %}

<p>
It's going to generate SQL: <code>SELECT * FROM users ORDER BY created_at desc</code>
</p>

<p>
  So far so good, ActiveModel in Rails4 is better. :)
</p>