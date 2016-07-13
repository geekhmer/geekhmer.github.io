---
layout: post
title: "Sessions Expiring"
date: 2016-02-23 14:54
comments: true
categories: [Ruby on Rails]
keywords: Sessions Expiring
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="Sessions Expiring" />
</p>

<p>
  To improve user security, we may wish to expire the user's session after they are inactive for a given amount of time. In order to do this, we simply set the <code>expire_after</code> parameter when setting up our session store. For example:
</p>

{% codeblock config/initializers/session_store.rb lang:ruby %}
Rails.application.config.session_store :cookie_store, key: '_example_session', expire_after: 15.minutes
{% endcodeblock %}

<p>
  The line above would expire the session after 15 minutes of inactivity. This means that the user's session variables would get wiped once time reaches 15 minutes since the user last accessed the Rails application. If the user refreshes the page, the timer gets refreshed.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
