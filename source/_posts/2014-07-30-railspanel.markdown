---
layout: post
title: "RailsPanel"
date: 2014-07-30 08:30
comments: true
categories: [Other]

keywords: rails, railspanel, rails panel
description: RailsPanel
---

<p>
  RailsPanel is a Chrome extension for Rails development that will end your tailing of development.log. Have all information about your Rails app requests in the browser - in the Developer Tools panel. Provides insight to db/rendering/total times, parameter list, rendered views and more.
</p>

<p>
  <a class="fancybox" href="/images/rails_panel.png"><img src="/images/rails_panel.png" width="680" /></a>
</p>

<p>
  <strong>Installation</strong><br/>
  To use this extension you need to add meta_request gem to your app's Gemfile:
</p>

{% codeblock Gemfile lang:ruby %}
group :development do
  gem 'meta_request'
end
{% endcodeblock %}

<p>
  Install <a href="https://chrome.google.com/webstore/detail/railspanel/gjpfobpafnhjhbajcjgccbbdofdckggg" target="_blank">RailsPanel extension</a> from the Chrome WebStore. This is recommended way of installing extension, since it will auto-update on every new version. Note that you still need to update meta_request gem yourself.
</p>

<p>
  So far so good, Let play around with RailsPanel and enjoy your productivity. :)
</p>