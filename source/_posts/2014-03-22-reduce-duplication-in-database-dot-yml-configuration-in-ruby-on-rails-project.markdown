---
layout: post
title: "Reduce duplication in database.yml configuration in Ruby on Rails Project"
date: 2014-03-22 00:03
comments: true
categories: [Ruby on Rails]
keywords: Reduce duplication in database.yml, database.yml, Reduce duplication in database.yml configuration in Ruby on Rails Project
---

<p>
  By default rails generate database.yml that have the same attributes in each environment. So to reduce duplication we use aliasing to essentially perform a hash merge on the fly in the code.<br/>
  So far so good please take a look the below code.
</p>

<p>
  <strong>Before refactor</strong>
</p>

{% codeblock database.yml lang:javascript %}
development:
  database: blog_development
  host: localhost
  username: your_database_username
  password: your_database_password
  adapter: mysql2
  encoding: utf8
  collation: utf8_unicode_ci

test:
  database: blog_test
  host: localhost
  username: your_database_username
  password: your_database_password
  adapter: mysql2
  encoding: utf8
  collation: utf8_unicode_ci

staging:
  database: blog_staging
  host: localhost
  username: your_database_username
  password: your_database_password
  adapter: mysql2
  encoding: utf8
  collation: utf8_unicode_ci

production:
  database: blog_production
  host: localhost
  username: your_database_username
  password: your_database_password
  adapter: mysql2
  encoding: utf8
  collation: utf8_unicode_ci
{% endcodeblock %}

<p>
  <strong>After refactor</strong>
</p>

{% codeblock database.yml lang:javascript %}
defaults: &defaults
  username: your_database_username
  password: your_database_password
  adapter: mysql2
  encoding: utf8
  collation: utf8_unicode_ci

development:
  database: blog_development
  host: localhost
  <<: *defaults

test:
  database: blog_test
  host: localhost
  <<: *defaults

staging:
  database: blog_staging
  host: localhost
  <<: *defaults

production:
  database: blog_production
  host: localhost
  <<: *defaults
{% endcodeblock %}