---
layout: post
title: "Ruby/Ruby on Rails Mistakes"
date: 2015-05-07 21:16
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Ruby/Ruby on Rails Mistakes
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="Ruby/Ruby on Rails Mistakes" />
</p>

<p>
  Software Engineer make mistakes. Some of them are just annoying (for others to read) and some are really dangerous. Here is my selection of 10 mistakes done by Ruby / Ruby on Rails developers. These tips are easy to follow and can save you much time of later debugging.
</p>

<p>
  <strong>Double Negative and Complex Conditionals</strong><br/>
</p>

{% codeblock lang:ruby %}
if !user.nil?
  # ...
end

unless user.blank?
  # ...
end

unless user.active? || address.confirmed?
  # ...
end
{% endcodeblock %}

<p>
  Double negative is hard to read. Every time I encounter it, I spend a couple of seconds on parsing the condition. Use the API which Rails gives you  <code>user.present?</code> instead of <code>!user.blank?</code>.
</p>

<p>
  I also rarely see any usage for unless, especially with complex conditionals connected by && and ||. How fast can you decide when unless <code>user.active? || address.confirmed?</code> will fire?
</p>

<p>
  <strong>Using Save Instead of save! and Not Checking Return Value</strong><br/>
</p>

{% codeblock lang:ruby %}
user = User.new
user.name = "John"
user.save
{% endcodeblock %}

<p>
  What is wrong with this piece of code? It will fail silently when user cannot be saved. There will be no single trace of this failure in your logs and you will spend time wondering: “why there are no users in the database”. If you expect that data is valid and model should be always saved successfully, then use bang versions – <code>save!</code>, <code>create!</code> and so on. Use save only when you handle the return value:
</p>

{% codeblock lang:ruby %}
if user.save
  # ...
else
  # ...
end
{% endcodeblock %}

<p>
  <strong>self When It’s Not Needed</strong><br/>
</p>

{% codeblock lang:ruby %}
class User
  attr_accessor :first_name
  attr_accessor :last_name

  def display_name
    "#{self.first_name} #{self.last_name}"
  end
end
{% endcodeblock %}

<p>
  In this case writing self.first_name is completely unnecessary, because <code>first_name</code> will do. This is of course just matter of style and has no other negative consequences than overly verbose code. Please mind that you need <code>self</code> in assignments: <code>self.first_name = "John"</code>.
</p>

<p>
  <strong>N + 1 Queries</strong><br/>
  This is a vast topic, but I will try to give the simplest example. You want to display a list of posts with names of authors. Post model <code>belongs_to :user</code>. If you just do <code>Post.limit(10)</code> and then call <code>post.user.name</code> in your views, you will make a separate database query for each user. That’s because Rails has no single chance to guess that you need users when you make the first query in the controller.
</p>

<p>
  It’s easy to spot N + 1 queries problem just by looking at server’s logs:
</p>

{% codeblock lang:ruby %}
Template Load (0.4ms)  SELECT  "templates".* FROM "templates"   ORDER BY "templates"."id" desc LIMIT 30 OFFSET 0
  Collection Load (0.2ms)  SELECT  "collections".* FROM "collections"  WHERE "collections"."id" = ? LIMIT 1  [["id", 1]]
  Collection Load (0.1ms)  SELECT  "collections".* FROM "collections"  WHERE "collections"."id" = ? LIMIT 1  [["id", 6]]
  CACHE (0.0ms)  SELECT  "collections".* FROM "collections"  WHERE "collections"."id" = ? LIMIT 1  [["id", 6]]
  CACHE (0.0ms)  SELECT  "collections".* FROM "collections"  WHERE "collections"."id" = ? LIMIT 1  [["id", 6]]
  Collection Load (0.1ms)  SELECT  "collections".* FROM "collections"  WHERE "collections"."id" = ? LIMIT 1  [["id", 3]]
  CACHE (0.0ms)  SELECT  "collections".* FROM "collections"  WHERE "collections"."id" = ? LIMIT 1  [["id", 3]]
  Collection Load (0.1ms)  SELECT  "collections".* FROM "collections"  WHERE "collections"."id" = ? LIMIT 1  [["id", 2]]
  CACHE (0.0ms)  SELECT  "collections".* FROM "collections"  WHERE "collections"."id" = ? LIMIT 1  [["id", 2]]
  CACHE (0.0ms)  SELECT  "collections".* FROM "collections"  WHERE "collections"."id" = ? LIMIT 1  [["id", 2]]
  CACHE (0.0ms)  SELECT  "collections".* FROM "collections"  WHERE "collections"."id" = ? LIMIT 1  [["id", 1]]
  CACHE (0.1ms)  SELECT  "collections".* FROM "collections"  WHERE "collections"."id" = ? LIMIT 1  [["id", 1]]
  Collection Load (0.1ms)  SELECT  "collections".* FROM "collections"  WHERE "collections"."id" = ? LIMIT 1  [["id", 4]]
{% endcodeblock %}

<p>
  You have to be explicit at telling what you need from the database. In the easy cases Rails includes method will do. You can read more about it in Rails guides - <a href="http://guides.rubyonrails.org/active_record_querying.html#eager-loading-associations" target="_blank">Eager Loading</a>
</p>

<p>
  <strong>Boolean Field with Three Possible Values</strong><br/>
  Boolean is supposed to have two possible values – <code>true</code> and <code>false</code>, right? And how about <code>nil</code>? If you do not specify default value and null: false in your migrations, you end up with boolean field with three possible values – <code>true</code>, <code>false</code> and <code>nil</code>. This leads to nasty code like:
</p>

{% codeblock lang:ruby %}
# post is new, not published, not rejected
if post.published.nil?
  # ...
end

# post is published
if post.published
  # ...
end

# post is new or rejected
unless post.published
  # ...
end
{% endcodeblock %}

<p>
  If you need three possible states – use string field with three <code>well-defined</code> values.
</p>

<p>
  <strong>Orphaned Records After Destroy</strong><br/>
  When you destroy a model and it is required by associated records, you should handle it. It’s easy to find such cases:
</p>

{% codeblock lang:ruby %}
class Post < ActiveRecord::Base
  belongs_to :user
  validates_presence_of :user
end
{% endcodeblock %}

<p>
  User is required for post. Hence, we have to write:
</p>

{% codeblock lang:ruby %}
class User < ActiveRecord::Base
  has_many :posts, dependent: :destroy
end
{% endcodeblock %}

<p>
  <strong>Leaving puts</strong><br/>
  Leaving <code>puts</code> in the code after some debugging session pollutes server logs and output of tests. Use <code>Rails.logger.debug</code> so it’s later possible to adjust the desired log level.
</p>

<p>
  <strong>Not Using map</strong><br/>
  I’ve seen such code many times:
</p>

{% codeblock lang:ruby %}
users = []

posts.each do |post|
  users << post.user
end
{% endcodeblock %}

<p>
  This is exactly the case for using <code>map</code>, which is shorter and more idiomatic:
</p>

{% codeblock lang:ruby %}
users = posts.map do |post|
  post.user
end
{% endcodeblock %}

<p>
  <strong>Not Using Hash#fetch</strong><br/>
</p>

{% codeblock lang:ruby %}
name = params[:user][:name]
{% endcodeblock %}

<p>
  What’s wrong with this code? It will throw <code>NoMethodError: undefined method `[]' for nil:NilClass</code> if there is no user key in the hash. If you expect the key to always be present, use <code>Hash#fetch</code>:
</p>

{% codeblock lang:ruby %}
name = params.fetch(:user)[:name]
{% endcodeblock %}

<p>
  This will give you a meaningful <code>exception – KeyError: key not found: :user</code>.
</p>

<p>
  <strong>Using Code from app/ in Migrations</strong><br/>
  Let’s say you have the following model:
</p>

{% codeblock lang:ruby %}
class User < ActiveRecord::Base
  ACTIVE = "after_registration"
end
{% endcodeblock %}

<p>
  and you want to add points field to it. So you create a migration. But you would also like to handle existing users: 10 points for active and 0 for the rest. You add to your migration:
</p>

{% codeblock lang:ruby %}
User.where(status: User::ACTIVE).update_all(points: 10)
{% endcodeblock %}

<p>
  It works and you are happy. Time passes by and you decide to remove <code>User::ACTIVE</code> constant. Your migrations are now broken, you cannot run them from scratch, because <code>User::ACTIVE</code> is undefined.
</p>

<p>
  Never use code from app/ directory in migrations. If you need to update existing data and do it in a few environments (development, staging, production) create a Rake task and delete it once it’s executed in every environment.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
