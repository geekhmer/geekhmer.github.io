---
layout: post
title: "Ruby on Rails connect to Multiple Databases And Migrations"
date: 2015-02-07 23:32
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Ruby on Rails connect to Multiple Databases And Migrations, Multiple Databases And Migrations, Rails connect to Multiple Databases And Migrations, Rails connect to Multiple Databases, Ruby on Rails connect to Multiple Databases
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails connect to Multiple Databases And Migrations" />
</p>

<p>
  Ruby on Rails connect to Multiple Databases and using ActiveRecord with multiple databases, it’s really simple take it easy. Let’s run through this.
</p>

<p>
  <strong>Rake Tasks</strong><br/>
   Well, I want to handle migrations for two databases, so I need two separate Rake tasks to handle that:
</p>

{% codeblock lang:ruby %}
desc "Migrate the database through scripts in db/migrate directory."

namespace :db do
  task :migrate do
    Rake::Task["db:migrate_db1"].invoke
    Rake::Task["db:migrate_db2"].invoke
  end

  task :migrate_db1 do
    ActiveRecord::Base.establish_connection DB1_CONF
    ActiveRecord::Migrator.migrate("db/migrate/db1/")
  end

  task :migrate_db2 do
    ActiveRecord::Base.establish_connection DB2_CONF
    ActiveRecord::Migrator.migrate("db/migrate/db2/")
  end
end
{% endcodeblock %}

<p>
  My first task is <code>db:migrate</code> that delegates out to <code>db:migrate_db1</code> & <code>db:migrate_db2</code>.
</p>

<p>
  Each of those establish a connection to the database and then runs the migrations from their own separate folders. This allows you to store migrations in separate folders so you can easily manage them.
</p>

<p>
  The migrations are exactly the same as normal.
</p>

<p>
  <strong>Database Connections</strong><br/>
  In order to get those migrations to work, I need to configure the database connections. I'm going to define everything in the <code>database.yml</code> just like normal, but with a different naming convention:
</p>

{% codeblock database.yml lang:ruby %}
defaults: &defaults
  username: root
  password: 1234567
  adapter: mysql2
  encoding: utf8
  collation: utf8_unicode_ci

db1:
  development:
    database: db1_development
    host: localhost
    <<: *defaults

  test:
    database: db1_test
    host: localhost
    <<: *defaults

  staging:
    database: db1_staging
    host: localhost
    <<: *defaults

  production:
    database: db1_production
    host: localhost
    <<: *defaults

db2:
  development:
    database: db2_development
    host: localhost
    <<: *defaults

  test:
    database: db2_test
    host: localhost
    <<: *defaults

  staging:
    database: db2_staging
    host: localhost
    <<: *defaults

  production:
    database: db2_production
    host: localhost
    <<: *defaults
{% endcodeblock %}

<p>
  I configure two separate databases db1 & db2.
</p>

<p>
  Then I need to configure the app to load these now. I open <code>application.rb</code> or environment file(s):
</p>

{% codeblock application.rb lang:ruby %}
ENV['ENV'] ||= 'development'

db_conf = YAML::load(File.open(File.join(APP_PATH,'config','database.yml')))

DB1_CONF = db_conf["db1"][ENV['ENV']]
DB2_CONF = db_conf["db2"][ENV['ENV']]
{% endcodeblock %}

<p>
  Take a look at what's going on:<br/>
  - I set the database configuration to use. You can just use Rails.env here instead of ENV['ENV'].<br/>
  - I load up the database.yml config and parse it with YAML.<br/>
  - I grab the configuration from the file for each db and the correct environment that I'm running in.<br/>
</p>

<p>
  <strong>Connecting Models</strong><br/>
  When I'm working with multiple databases, I like to explicitly setup the connections inside the models themselves instead of inheriting from ActiveRecord::Base and using subclasses.
</p>

{% codeblock user.rb lang:ruby %}
class User < ActiveRecord::Base
  establish_connection DB1_CONF
end
{% endcodeblock %}

{% codeblock product.rb lang:ruby %}
class Product < ActiveRecord::Base
  establish_connection DB2_CONF
end
{% endcodeblock %}

<p>
  Well, All you really need to do is load the configurations, establish the database connections, and setup the migrations to load from a specific directory for each database.
</p>

<p>
  So far so good, See ya! :)
</p>