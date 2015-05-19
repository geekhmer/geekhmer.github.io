---
layout: post
title: "Preload Active Record Associations in Rails Application"
date: 2015-05-05 16:26
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Preload Active Record Associations in Rails Application
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="Preload Active Record Associations in Rails Application" />
</p>

<p>
  Active Record makes database querying so simple. Chain together a few methods and bam, you’ve saved your self lines and lines of T-SQL. The problem is this simplicity masks the underlying operation and it’s very easy to not realize how inefficient your database calls are.
</p>

<p>
  For example:
</p>

{% codeblock lang:ruby %}
class User < ActiveRecord::Base
  has_many :posts
end
 
class Post < ActiveRecord::Base
  has_many :comments
  belongs_to :user
end
 
class Comment < ActiveRecord::Base
  belongs_to :post
end
{% endcodeblock %}

<p>
  Let’s say on a users profile page we would like to show a listing of comments from this user.
</p>

{% codeblock lang:ruby %}
@user = User.find(id)
 
@user.posts.each do |post|
   post.comments.each do |comment|
      <%= comment.message %>
   end
end
{% endcodeblock %}

<p>
  What you end up with is something like:
</p>

{% codeblock lang:ruby %}
User Load (1.1ms)  SELECT `users`.* FROM `users` WHERE 'id' = 1 LIMIT 1
 Post Load (0.7ms)  SELECT `posts`.* FROM `blogs` WHERE `blogs`.`user_id` = 2
 Comment Load (0.7ms)  SELECT `comments`.* FROM `comments` WHERE `comments`.`post_id` = 43
 Comment Load (1.7ms)  SELECT `comments`.* FROM `comments` WHERE `comments`.`post_id` = 55
 Comment Load (2.2ms)  SELECT `comments`.* FROM `comments` WHERE `comments`.`post_id` = 32
 Comment Load (0.9ms)  SELECT `comments`.* FROM `comments` WHERE `comments`.`post_id` = 66
 Comment Load (2.2ms)  SELECT `comments`.* FROM `comments` WHERE `comments`.`post_id` = 56
 Comment Load (4.8ms)  SELECT `comments`.* FROM `comments` WHERE `comments`.`post_id` = 65
 Comment Load (1.8ms)  SELECT `comments`.* FROM `comments` WHERE `comments`.`post_id` = 68
 Comment Load (0.8ms)  SELECT `comments`.* FROM `comments` WHERE `comments`.`post_id` = 71
{% endcodeblock %}

<p>
  The user has commented in 9 different posts, which results in 9 separate queries to the DB. This is a small scale example, but you can see how this can compound into something nasty.
</p>

<p>
  he solution is to user includes method:
</p>

{% codeblock lang:ruby %}
@user = User.find(id).includes(:posts => :comments)
{% endcodeblock %}

<p>
  When using includes method, Active Record ensures that all of the specified associations are loaded using the minimum possible number of queries. The actual query executed is converted into a more optimal singular query and it is that result set that is iterated over instead of the replicated find(id) style queries.
</p>

<p>
  Our query stack list now looks like:
</p>

{% codeblock lang:ruby %}
User Load (1.0ms)  SELECT `users`.* FROM `users` WHERE 'id' = 1 LIMIT 1
Post Load (0.4ms)  SELECT `posts`.* FROM `posts` WHERE `posts`.`user_id` IN (1)
Comment Load (0.5ms)  SELECT `comments`.* FROM `comments` WHERE `comments`.`post_id` IN (43, 55, 32, 66, 56, 65, 68, 71)
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
