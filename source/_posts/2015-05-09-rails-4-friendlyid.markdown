---
layout: post
title: "Rails 4 FriendlyId"
date: 2015-05-09 22:13
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Rails 4 FriendlyId
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="Rails 4 FriendlyId" />
</p>

<p>
  In this article we will go over using <a href="https://github.com/norman/friendly_id" target="_blank">FriendlyId</a> in Rails 4. It is important to note that the current version of FriendlyId (4.0) does not work with Rails 4. Therefore we will need to use FriendlyId 5.x, which is currently in release candidate status.
</p>

<p>
  First, let's include the FriendlyId gem in our gemfile.
</p>

{% codeblock Gemfile lang:ruby %}
gem "friendly_id", "~> 5.0.1"
{% endcodeblock %}

<p>
  FriendlyId no longer overrides finder code by default. Because of this we will need to change the way our controller code works. FriendlyId now exposes it's functionality via <code>.friendly</code>. For example, to find a post by it's friendly id, we use something like <code>Post.friendly.find(params[:id])</code>. To get FriendlyId working, you need to modify your controllers to use this behavior. An example is shown below.
</p>

{% codeblock app/controllers/posts_controller.rb lang:ruby %}
class PostsController < ApplicationController
  def index
    @posts = Post.all
  end

  def show
    @post = Post.friendly.find(params[:id])
  end
end
{% endcodeblock %}

<p>
  If you have a large site making extensive use of FriendlyId, you can also restore the old finder override behavior. This isn't recommended though. An example of how to do this is listed below.
</p>

{% codeblock app/models/post.rb lang:ruby %}
class Post < ActiveRecord::Base
  extend FriendlyId
  friendly_id :title, use: [:slugged, :finders]
end
{% endcodeblock %}

<p>
  Once you've added this, finder override functionality should now be restored. For example, the code listed below will now work as before.
</p>

{% codeblock app/controllers/posts_controller.rb lang:ruby %}
class PostsController < ApplicationController
  def index
    @posts = Post.all
  end

  def show
    @post = Post.find(params[:id])
  end
end
{% endcodeblock %}

<p>
  FriendlyId 5 introduces a new slug candidates feature. This feature allows you to tell friendly id what to do in the case of duplicate slugs. See the code listed below for an example of how to utilize this functionality.
</p>

{% codeblock app/models/post.rb lang:ruby %}
class Post < ActiveRecord::Base
  extend FriendlyId
  friendly_id :slug_candidates, use: [:slugged, :finders]

  def slug_candidates
    [
      :title,
      [:title, :id]
    ]
  end
end
{% endcodeblock %}

<p>
  The code listed above tells FriendlyId you want to use the slug_candidates method to make the slug unique. It will try the list beginning to last, so in the example above, it will try to generate the slug first using title, and then if a post by that title already exists, it will try again by appending the id.
</p>

<p>
  Some other quick things to note when upgrading to FriendlyId 5:<br/>
  - The default sequence separator has been changed from two dashes (--) to one dash (-). If you have a url scheme in place using the old way of doing things, you might want to find a workaround.<br/>
  - FriendlyId no longer uses a numeric sequence to differentiate a conflicting slug. Instead it uses a UUID (example: 2bc08962-b3dd-4f29-b2e6-244710c86106). If you dislike this functionality, you can utilize the new slug candidates feature explained above.
</p>
