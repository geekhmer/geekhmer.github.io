---
layout: post
title: "Ruby on Rails Get a User's Gravatar"
date: 2015-05-09 21:02
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Ruby on Rails Get a User's Gravatar
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="Ruby on Rails Get a User's Gravatar" />
</p>

<p>
  <a href="https://en.gravatar.com/" target="_blank">Gravatar</a> is a Fantastic service that provides a universal avatar for users. Gravatar has designed their service in such a way that the user's avatar can be rendered on any site free of charge. Gravatar is used on millions of sites across the globe including GitHub, Stack Overflow, Word Press, Disqus, and many more sites. You can easily use gravatar without much effort. Simply add the code below to your application helper:
</p>

{% codeblock lang:ruby %}
module ApplicationHelper
  def gravatar_url(email, size)
    gravatar = Digest::MD5::hexdigest(email).downcase
    url = "http://gravatar.com/avatar/#{gravatar}.png?s=#{size}"
  end
end
{% endcodeblock %}

<p>
  The first parameter, email, is the email address you wish to obtain the gravatar for. The second parameter, size, is the size of the gravatar you wish to render. To actually get the link to the gravatar, we obtain an MD5 hash of the email and then lowercase it and append it to the end of the gravatar url. The size is specified using the s parameter. To use this function, simple pass the newly generated Gravatar url to an image tag like this:
</p>

{% codeblock lang:ruby %}
<%= image_tag gravatar_url("you@youremail.com", 64), alt: "" %>
{% endcodeblock %}

<p>
  This will render the gravatar for that email address or a default image if the avatar doesn't exist. If you wish to specify your default image, you can modify your helper to look like the code listed below, changing the default url to whatever you wish:
</p>

{% codeblock lang:ruby %}
def gravatar_url(email, size)
  gravatar_id = Digest::MD5::hexdigest(email).downcase
  default_url = "http://mysite.com/myavatar.png"
  url = "http://gravatar.com/avatar/#{gravatar_id}.png?s=#{size}&d=#{CGI::escape(default_url)}"
end
{% endcodeblock %}

<p>
  This will use whatever default URL you provide for the default url variable.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
