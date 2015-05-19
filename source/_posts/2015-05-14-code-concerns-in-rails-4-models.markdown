---
layout: post
title: "Code Concerns in Rails 4 Models"
date: 2015-05-14 12:48
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Code Concerns in Rails 4 Models
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="Code Concerns in Rails 4 Models" />
</p>

<p>
  You may have noticed that Rails 4 creates a new folder called concerns. Concerns are pieces of code that allow you to better organize the code that you write. This feature has actually been around for a long time prior to Rails 4, Rails 4 merely creates the folders for you and sets up the environment. In this article, we focus primarily on Rails 4.0, but it should apply equally to 3.2.x or even 3.0.x and earlier.
</p>

<p>
  Here is a simple example, lets say we have a model called user. In this model we would typically have something like this:
</p>

{% codeblock lang:ruby %}
class User < ActiveRecord::Base
  has_secure_password

  def self.authenticate(email, password)
    user = find_by_email(email)
    user if !user.nil? && user.authenticate(password)
  end

  def create_password_reset_token
    logger.warn "Create password reset token code goes here."
    false
  end
end
{% endcodeblock %}

<p>
  We can create a a file in the app/models/concerns folder called authentication.rb (app/models/concerns/authentication.rb) and place the following code in it:
</p>

{% codeblock lang:ruby %}
module Authentication
  extend ActiveSupport::Concern

  included do
    has_secure_password
  end

  module ClassMethods
    def authenticate(email, password)
      user = find_by_email(email)
      user if user && user.authenticate(password)
    end
  end

  def create_password_reset_token
    logger.warn "Create password reset token code goes here."
    false
  end
end
{% endcodeblock %}

<p>
  Now, lets refactor the User model to use the new code. Change the users model (app/models/user.rb) so that it looks like this:
</p>

{% codeblock lang:ruby %}
class User < ActiveRecord::Base
  include Authentication
end
{% endcodeblock %}

<p>
  Now if you start the rails server and attempt to authenticate, you'll notice that the functionality hasn't changed at all. That's the idea! You can use this method for organizing and DRYing up your code.
</p>

<p>
  The extend <code>ActiveSupport::Concern</code> tells rails that we are creating a concern.
</p>

<p>
  The code contained within the included block will be executed within the context of the class that is including the module. This is perfect for including functionality provided by 3rd party gems, etc.
</p>

<p>
  Next you will notice the <code>module ClassMethods</code> block. The code contained within this block will be added to the Class itself. For example, the code above adds an authenticate function to the User class. This allows you to do User.authenticate(email, password) instead of User.find_by_email(email).authenticate(password).
</p>

<p>
  Finally you will see the last bit of code, the <code>create_password_reset_token</code> function. Code not included in the <code>ClassMethods</code> block or the <code>included</code> block will be included as instance methods. For example, You could do <code>@user = User.find(params[:id])</code> and then do <code>@user.create_password_reset_token</code> to create a password reset token for the specified user.
</p>

<p>
  Now, all of this is great, but what benefit do you get from organizing your code in this fashion? Well, lets look at a good example of how utilizing this functionality of Rails can save you time and make your code much cleaner. Lets say we have a number of different models, BlogPosts, Articles, Comments, etc and we want to add tagging functionality that will allow the user to tag each object as something. Utilizing concerns we can quickly and easily do this:
</p>

{% codeblock lang:ruby %}
module Taggable
  extend ActiveSupport::Concern

  included do
    has_many :taggings, as: :taggable, dependent: :destroy
    has_many :tags, through: :taggings 
  end

  def tag_names
    tags.map(&:name)
  end
end
{% endcodeblock %}

<p>
  In this simple example, you'll see that simply including Taggable on your modules will (with the database structure in place of course) make your models taggable. This code can quickly and easily be spread upon as many models as needed. Well that's it! Concerns are a great way to keep your code organized and DRY.
</p>

<p>
  So far so good, That it!!! See ya!!! :)
</p>
