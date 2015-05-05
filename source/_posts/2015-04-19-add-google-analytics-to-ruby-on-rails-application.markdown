---
layout: post
title: "Add Google Analytics to Ruby on Rails Application"
date: 2015-04-19 12:30
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Add Google Analytics to Ruby on Rails Application
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Add Google Analytics to Ruby on Rails Application" />
</p>

<p>
  Google Analytics is used by the vast majority of the internet to monitor traffic. You can quickly and easily add Google Analytics to your Rails application, however there are a few things you should pay attention to. First, make sure that the Analytics code only loads up in a production environment. It's best to do this by first creating a partial called google_analytics and placing the following code inside of it:
</p>

{% codeblock app/views/layouts/_google_analytics.html.erb lang:ruby %}
<% if Rails.env == "production"  %>
  <script type="text/javascript">

    var _gaq = _gaq || [];
    _gaq.push(['_setAccount', 'UA-00000000-1']);
    _gaq.push(['_trackPageview']);

    (function() {
      var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
      ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
      var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
    })();

  </script>
<% end %>
{% endcodeblock %}

<p>
  This code will tell Rails to only include the Analytics code in a production environment.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>