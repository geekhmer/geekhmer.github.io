---
layout: post
title: "How to Use link_to in Ruby on Rails?"
date: 2015-06-06 21:39
comments: true
categories: [Ruby, Ruby on Rails]
keywords: How to Use link_to in Ruby on Rails?
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="How to Use link_to in Ruby on Rails?" />
</p>

<p>
  Even after many years of using Ruby on Rails I still have trouble remembering how to properly use all those options available for the link_to helper. Three out of five times I have to fire up the docs and search for the examples (cause that's how I like to read the docs).
</p>

<p>
  <strong>Having Documentation in Hand</strong><br/>
  Using documentation locally is helpful, especially if you can integrate it into your editor. Personally I'm using the <code>ri</code> docs with Vim but I guess all those other editors can be configured to access it. If not you can always use ri in your terminal of choice.
</p>

<p>
  Another favourite of mine is the awesome ApiDock. You can even see other people's comments on the docs and in many cases there you can find examples that are not present in the official docs.
</p>

<p>
  Another very good option (if you're on a mac) is to use Dash. It's easy to integrate in your editor or you can just fire it up with a system shortcut whenever you need to access the docs.
</p>

<p>
  That being said, I'm gonna list out a few of the more common examples I use in hopes that they will be useful for both you and me whenever I need to revisit <code>link_to</code>'s docs again.
</p>

<p>
  <strong>The Simplest Form</strong><br/>
  The most common and straightforward way of using <code>link_to</code> is to create a barebones (no magic) link. So, assuming your root route points to '/', the way you would use the helper is:
</p>

{% codeblock lang:ruby %}
<%= link_to "Home", root_path %>
# => <a href="/">Home</a>
{% endcodeblock %}

<p>
  <strong>Link-ing to a Resource</strong><br/>
  Another common variant is linking to a resource (a user's profile for example). Let's say the user's model is called User and that a @user ivar points to the user record who's id is 1. Creating a link to the user's profile will look like the following:
</p>

{% codeblock lang:ruby %}
<%= link_to "Profile", user_path(@user) %>
# => <a href="/users/1">Profile</a>
{% endcodeblock %}

<p>
  There's also a shorter version that will do the same thing. It's got some magic to it (like most things in Rails) but it also looks prettier and there's less to type (two stones with one bird).
</p>

{% codeblock lang:ruby %}
<%= link_to "Profile", @user %>
# => <a href="/users/1">Profile</a>
{% endcodeblock %}

<p>
  <strong>Using link_to with a Block</strong><br/>
  This is somewhat of a lesser known/used option of <code>link_to</code> but it's useful nonetheless and it also makes the code more readable. So in those cases where the link text is long and/or ugly, or it doesn't really fit on a 80 chars line, you can pass the link text inside a block.
</p>

<p>
  To make the example more obvious, I'm gonna do a before and after kind of thing.
</p>

<p>
  Before:
</p>

{% codeblock lang:ruby %}
<%= link_to "<span class='home-link'>Home</span>".html_safe, root_path %>
# => <a href="/"><span class="home-link">Home</span></a>
{% endcodeblock %}

<p>
  After:
</p>

{% codeblock lang:ruby %}
<%= link_to root_path do %>
  <%= content_tag :span, "Home", :class => "home-link" %>
<% end %>
# => <a href="/"><span class="home-link">Home</span></a>
{% endcodeblock %}

<p>
  In this case I've purposely chosen a less uglier link text, but usually the link text will be something like an image tag or a span with an icon inside it (or any other ugly html code you can think of).
</p>

<p>
  <strong>Adding html classes and/or id to Your Link</strong><br/>
  Another very common task you'll use is to add a html class or id to your links.
</p>

{% codeblock lang:ruby %}
<%= link_to "Section", root_path, :class => "my-class", :id => "my-id" %>
# => <a href="/" class="my-class" id="my-id">Section</a>
{% endcodeblock %}

<p>
  <strong>How to Delete a Record with link_to</strong><br/>
  Calling the <code>destroy</code> action of a REST-ful controller requires a <code>DELETE</code> request and that can be easily achieved by passing the <code>:method => :delete</code> hash as an option to the <code>link_to</code> helper.
</p>

{% codeblock lang:ruby %}
<%= link_to "Remove", @user, :method => :delete %>
# => <a rel="nofollow" data-method="delete" href="/users/1">Remove</a>
{% endcodeblock %}

<p>
  Note that the <code>rel="nofollow"</code> is auto-magically added by Rails as an SEO bonus.
</p>

<p>
  <strong>Require Confirmation for Deleting a Record</strong><br/>
  You will probably want some sort of confirmation when removing objects to prevent accidental deletes. The easiest way to add that is with a simple javascript alert box that will ask the user to confirm his delete request.
</p>

{% codeblock lang:ruby %}
<%= link_to "Remove", @user, :method => :delete, :data => {:confirm => "You Sure?"} %>
# => <a data-confirm="You Sure?" rel="nofollow" data-method="delete" href="/users/1">Remove</a>
{% endcodeblock %}

<p>
  <strong>Link-ing to an Image with link_to</strong><br/>
  It might be that you want to make your links prettier or that you want to have some nice buttons, or even a logo click-able or whatever the reason for using click-able images is, you'll want to add your image inside the link. Making an image link-able is pretty straight forward. Just add the <code>image_tag</code> where the link text would go and you're done.
</p>

{% codeblock lang:ruby %}
<%= link_to image_tag('logo.png'), root_path %>
# => <a href="/"><img src="/assets/logo-c88948e05e11587af2c23747862ca433.png" alt="Logo"></a>
{% endcodeblock %}

<p>
  You can also pass the image in a block if you like that style better.
</p>

{% codeblock lang:ruby %}
<%= link_to root_path do %>
  <%= image_tag('logo.png') %>
<% end %>
# => <a href="/"><img src="/assets/logo-c88948e05e11587af2c23747862ca433.png" alt="Logo"></a>
{% endcodeblock %}

<p>
  A nice side-effect of using the <code>image_tag</code> helper is that it will add the asset digest to your image.
</p>

<p>
  <strong>Adding an alt Attribute to the Image</strong><br/>
  As you've seen in the previous example, I didn't specify an alt attribute but the link_to helper generated one. The generated alt tag is just the name of the image file, capitalized. In case you want (and you should want) to override the alt attribute, it's very easy to do; just add your own alt attribute like so:
</p>

{% codeblock lang:ruby %}
<%= link_to image_tag('logo.png'), root_path, :alt => "MyLogo" %>
# => <a href="/"><img src="/assets/logo-c88948e05e11587af2c23747862ca433.png" alt="MyLogo"></a>
{% endcodeblock %}

<p>
  Alt attributes are beneficial for SEO purposes and they also aid those visitors who use text readers or non graphical browsers.
</p>

<p>
  <strong>Link-ing to an Image</strong><br/>
  There are times when you might want to link to an image (not necessarily with an image). This can be confusing because you need your image to contain the image digest generated by the asset pipeline. There's a helper that provides just that and it's called <code>image_path</code>.
</p>

{% codeblock lang:ruby %}
<%= link_to "Logo", image_path('logo.png') %>
# => <a href="/assets/logo-c88948e05e11587af2c23747862ca433.png">Logo</a>
{% endcodeblock %}

<p>
  <strong>Anchors with link_to</strong><br/>
  You might need to point to a specific section (anchor) in the page which you can identify by it's dom ID. So let's say on the target page we have a section that has the <code>id="interesting-section"</code>. In order to point our link to that section, we'll need to add the anchor to the generated link.
</p>

{% codeblock lang:ruby %}
<%= link_to "Section", root_path(:anchor => "interesting-section") %>
# => <a href="/#interesting-section">Section</a>
{% endcodeblock %}

<p>
  <strong>Ajax Links with link_to Remote</strong><br/>
  You can add the <code>:remote => true</code> option to the link to tell Rails that you want to handle the link via javascript. This option will automatically send an ajax request (handled via the jQuery UJS adapter).
</p>

{% codeblock lang:ruby %}
<%= link_to "Ajax", root_path, :remote => true %>
# => <a data-remote="true" href="/">Ajax</a>
{% endcodeblock %}

<p>
  <strong>Opening the link in a new tab or window</strong><br/>
  For a good user experience and because you'll want your user not to leave your website if possible, you should make all your external links open in a separate tab or window. You can achieve this by using the <code>target="_blank"</code> html attribute which in Rails speak will look like this:
</p>

{% codeblock lang:ruby %}
<%= link_to "Google", "http://google.com", :target => "_blank" %>
# => <a target="_blank" href="http://google.com">Google</a>
{% endcodeblock %}

<p>
  <strong>POST-ing Using link_to</strong><br/>
  Sending a post request via a link is something that the html cannot do. You can only use it to make <code>GET</code> requests, not <code>POST</code>. That being said, Rails has some magic tricks for you.
</p>

<p>
  By providing the <code>:method => :post</code> option, Rails will create a form and submit it via javascript. Note that you need the <code>jquery-rails</code> gem for this to work, if you don't have it, there won't be any magic happening and your links will default to a GET request.
</p>

{% codeblock lang:ruby %}
<%= link_to "Post", root_path, :method => :post %>
# => <a rel="nofollow" data-method="post" href="/">Post</a>
{% endcodeblock %}

<p>
  <strong>Adding More Params to the POST Request</strong><br/>
</p>

{% codeblock lang:ruby %}
<%= link_to "Create User", users_path(:email => "jdoe@email.com", :password => "secret"), :method => :post %>
# => <a rel="nofollow" data-method="post" href="/users?email=jdoe%40email.com&amp;password=secret">Create User</a>
{% endcodeblock %}

<p>
  These are some of the more common ways I've used link_to but I'm sure there are many others. So if you have any other examples I could add to the article.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
