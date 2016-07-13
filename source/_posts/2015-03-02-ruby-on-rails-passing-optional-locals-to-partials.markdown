---
layout: post
title: "Ruby on Rails Passing Optional Locals to Partials"
date: 2015-03-02 23:32
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Ruby on Rails Passing Optional Locals to Partials, Ruby on Rails Passing Optional Locals to Partial
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails Passing Optional Locals to Partials" />
</p>

<p>
  When passing locals to Rails partials, you might run into cases where you need to pass optional locals in some places, but don’t want to pass it in every other place where you use the partial.
</p>

<p>
  As an example, you have a <code>_post</code> partial which you render like this:
</p>

{% codeblock lang:ruby %}
<%= render 'post', post: post %>
{% endcodeblock %}

<p>
  And now you want to render the same partial from another view, but this time you want to pass a boolean flag to tell the partial to render the author bio as well:
</p>

{% codeblock lang:ruby %}
<%= render 'post', post: post, show_author_bio: true %>
{% endcodeblock %}

<p>
  If we used the <code>show_author_bio</code> local in the partial, it would break the other view which does not know this local. To use it safely, we can use the <code>local_assigns</code> hash:
</p>

{% codeblock lang:ruby %}
<h1><%= post.title %></h1>

<% if local_assigns[:show_author_bio] %>
  <%= render 'author_bio', author: post.author %>
<% end %>

<%= post.body %>
{% endcodeblock %}

<p>
  We’re using it for passing a boolean value here, we could pass in any other object as well. For instance, we could pass in an optional author object:
</p>

{% codeblock lang:ruby %}
<% if local_assigns.has_key?(:author) %>
  <%= render 'author_bio', author: author %>
<% end %>
{% endcodeblock %}

<p>
  DHH (David Heinemeier Hansson) opened an issue (<a href="https://github.com/rails/rails/issues/18962" target="_blank">#18962</a>) about this as well, and had an interesting comment to make about this feature.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
