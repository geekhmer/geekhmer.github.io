---
layout: post
title: "Secrets.yml File in Ruby on Rails"
date: 2015-02-20 00:40
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Secrets.yml File in Ruby on Rails, Secrets.yml File in Rails
---

<p>
  <img src="/images/move_to_rails.png" alt="Secrets.yml File in Ruby on Rails" />
</p>

<p>
  You maybe have noticed a file called <code>secrets.yml</code> in the config directory of a Ruby on Rails 4.1 project. This feature was added as part of Rails 4.1 in order to have a common storage location for the keys and credentials for various services. You can use the secrets.yml for everything from AWS credentials to your secret_key_base (the default in Rails 4.1).
</p>

{% codeblock secrets.yml lang:ruby %}
development:
  secret_key_base: super_long_secret_key_for_development
  active_merchant_login: 112233
  active_merchant_password: super_secret_password

test:
  secret_key_base: super_long_secret_key_for_test
  active_merchant_login: 112233
  active_merchant_password: super_secret_password

production:
  secret_key_base: <%= ENV["SECRET_KEY_BASE"] %>
  active_merchant_login: <%= ENV["AM_LOGIN"] %>
  active_merchant_password: <%= ENV["AM_PASSWORD"] %>
{% endcodeblock %}

<p>
  You have add this file to your <code>.gitignore</code> file to avoid accidently pushing your keys to git. You can also store your production keys in this file if you wish.
</p>

<p>
  To access the various keys in the secrets.yml file:
</p>

{% codeblock lang:ruby %}
Rails.application.secrets.key_name
{% endcodeblock %}

<p>
  Example: The following code will returns the merchant login.
</p>

{% codeblock lang:ruby %}
Rails.application.secrets.active_merchant_login # returns 112233 on development/test
{% endcodeblock %}

<p>
  So far so good, That's it! See ya! :)
</p>
