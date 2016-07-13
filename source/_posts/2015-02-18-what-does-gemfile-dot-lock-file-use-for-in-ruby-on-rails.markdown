---
layout: post
title: "What does Gemfile.lock File use for in Ruby on Rails?"
date: 2015-02-18 20:35
comments: true
categories: [Ruby on Rails, Ruby]
keywords: What does Gemfile.lock File use for in Ruby on Rails?, What does Gemfile.lock File use for in Rails?, How does the Gemfile.lock fle work?, How does the Gemfile.lock fle work in Ruby on Rails?, How does the Gemfile.lock fle work in Rails?
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="What does Gemfile.lock File use for in Ruby on Rails?" />
</p>

<p>
  You have noticed a file called Gemfile.lock in your Ruby on Rails application and have wondered what is it, how it works. The file may have even given you trouble when you attempted to push changes to source control. In this article I will discuss what exactly this Gemfile.lock file is and how it helps your Ruby on Rails application.
</p>

<p>
  <strong>Gemfile.lock</strong><br/>
  If you actually opened the Gemfile.lock file you probably saw a bunch of text as below:
</p>

{% codeblock Gemfile.lock lang:ruby %}
GEM
  remote: https://rubygems.org/
  specs:
    actionmailer (4.0.2)
      actionpack (= 4.0.2)
      mail (~> 2.5.4)
    actionpack (4.0.2)
      activesupport (= 4.0.2)
      builder (~> 3.1.0)
      erubis (~> 2.7.0)
      rack (~> 1.5.2)
      rack-test (~> 0.6.2)
    activemodel (4.0.2)
      activesupport (= 4.0.2)
      builder (~> 3.1.0)
    activenavbar (1.0.0)
    activerecord (4.0.2)
      activemodel (= 4.0.2)
      activerecord-deprecated_finders (~> 1.0.2)
      activesupport (= 4.0.2)
      arel (~> 4.0.0)
    activerecord-deprecated_finders (1.0.3)
    activesupport (4.0.2)
      i18n (~> 0.6, >= 0.6.4)
      minitest (~> 4.2)
      multi_json (~> 1.3)
      thread_safe (~> 0.1)
      tzinfo (~> 0.3.37)
    arel (4.0.2)
    atomic (1.1.16)
    bootstrap-sass (3.0.3.0)
      sass (~> 3.2)
    builder (3.1.4)
    capybara (2.2.1)
      mime-types (>= 1.16)
      nokogiri (>= 1.3.3)
      rack (>= 1.0.0)
      rack-test (>= 0.5.4)
      xpath (~> 2.0)
    coderay (1.1.0)
    coffee-rails (4.0.1)
      coffee-script (>= 2.2.0)
      railties (>= 4.0.0, < 5.0)
    coffee-script (2.2.0)
      coffee-script-source
      execjs
    coffee-script-source (1.7.0)
    columnize (0.3.6)
    database_cleaner (0.9.1)
    debugger (1.6.6)
      columnize (>= 0.3.1)
      debugger-linecache (~> 1.2.0)
      debugger-ruby_core_source (~> 1.3.2)
    debugger-linecache (1.2.0)
    debugger-ruby_core_source (1.3.2)
    diff-lcs (1.2.5)
    erubis (2.7.0)
    execjs (2.0.2)
    factory_girl (4.2.0)
      activesupport (>= 3.0.0)
    factory_girl_rails (4.2.1)
      factory_girl (~> 4.2.0)
      railties (>= 3.0.0)
    haml (3.1.8)
    haml-rails (0.3.5)
      actionpack (>= 3.1, < 4.1)
      activesupport (>= 3.1, < 4.1)
      haml (~> 3.1)
      railties (>= 3.1, < 4.1)
    hike (1.2.3)
    i18n (0.6.9)
    jbuilder (1.5.3)
      activesupport (>= 3.0.0)
      multi_json (>= 1.2.0)
    jquery-rails (3.1.0)
      railties (>= 3.0, < 5.0)
      thor (>= 0.14, < 2.0)
    json (1.8.1)
    mail (2.5.4)
      mime-types (~> 1.16)
      treetop (~> 1.4.8)
    method_source (0.8.2)
    mime-types (1.25.1)
    mini_portile (0.5.2)
    minitest (4.7.5)
    multi_json (1.9.0)
    mysql2 (0.3.15)
    nokogiri (1.6.1)
      mini_portile (~> 0.5.0)
    olive (0.0.1)
    polyglot (0.3.4)
    pry (0.9.12.6)
      coderay (~> 1.0)
      method_source (~> 0.8)
      slop (~> 3.4)
    pry-debugger (0.2.2)
      debugger (~> 1.3)
      pry (~> 0.9.10)
    quiet_assets (1.0.2)
      railties (>= 3.1, < 5.0)
    rack (1.5.2)
    rack-test (0.6.2)
      rack (>= 1.0)
    rails (4.0.2)
      actionmailer (= 4.0.2)
      actionpack (= 4.0.2)
      activerecord (= 4.0.2)
      activesupport (= 4.0.2)
      bundler (>= 1.3.0, < 2.0)
      railties (= 4.0.2)
      sprockets-rails (~> 2.0.0)
    railties (4.0.2)
      actionpack (= 4.0.2)
      activesupport (= 4.0.2)
      rake (>= 0.8.7)
      thor (>= 0.18.1, < 2.0)
    rake (10.1.1)
    rdoc (4.1.1)
      json (~> 1.4)
    rspec-core (2.14.8)
    rspec-expectations (2.14.5)
      diff-lcs (>= 1.1.3, < 2.0)
    rspec-mocks (2.14.6)
    rspec-rails (2.14.1)
      actionpack (>= 3.0)
      activemodel (>= 3.0)
      activesupport (>= 3.0)
      railties (>= 3.0)
      rspec-core (~> 2.14.0)
      rspec-expectations (~> 2.14.0)
      rspec-mocks (~> 2.14.0)
    sass (3.2.15)
    sass-rails (4.0.2)
      railties (>= 4.0.0, < 5.0)
      sass (~> 3.2.0)
      sprockets (~> 2.8, <= 2.11.0)
      sprockets-rails (~> 2.0.0)
    sdoc (0.4.0)
      json (~> 1.8)
      rdoc (~> 4.0, < 5.0)
    shoulda-matchers (2.2.0)
      activesupport (>= 3.0.0)
    slop (3.5.0)
    sprockets (2.11.0)
      hike (~> 1.2)
      multi_json (~> 1.0)
      rack (~> 1.0)
      tilt (~> 1.1, != 1.3.0)
    sprockets-rails (2.0.1)
      actionpack (>= 3.0)
      activesupport (>= 3.0)
      sprockets (~> 2.8)
    thor (0.18.1)
    thread_safe (0.2.0)
      atomic (>= 1.1.7, < 2)
    tilt (1.4.1)
    treetop (1.4.15)
      polyglot
      polyglot (>= 0.3.1)
    turbolinks (2.2.1)
      coffee-rails
    tzinfo (0.3.39)
    uglifier (2.5.0)
      execjs (>= 0.3.0)
      json (>= 1.8.0)
    xpath (2.0.0)
      nokogiri (~> 1.3)

PLATFORMS
  ruby

DEPENDENCIES
  activenavbar (~> 1.0.0)
  bootstrap-sass (~> 3.0.3.0)
  capybara (~> 2.2.1)
  coffee-rails (~> 4.0.0)
  database_cleaner (~> 0.9.1)
  factory_girl_rails (= 4.2.1)
  haml-rails (~> 0.3.4)
  jbuilder (~> 1.2)
  jquery-rails
  json (~> 1.8.1)
  mysql2
  olive (~> 0.0.1)
  pry
  pry-debugger
  quiet_assets
  rails (= 4.0.2)
  rspec-rails (~> 2.14.1)
  sass-rails (~> 4.0.0)
  sdoc
  shoulda-matchers
  turbolinks
  uglifier (>= 1.3.0)
{% endcodeblock %}

<p>
  The Gemfile.lock file stores a complete snapshot of every version of every gem your Ruby on Rails application uses. The reason for this is simple. Let's say you are using Rails 4.1.6 and Rails 5.0 comes out. You don't want this new version to be pushed to your application during the next update. Why? You developed your application using the old version, and the new version may not be compatible with your code. That is why it is also important to check your Gemfile.lock into source control with the rest of your application.
</p>

<p>
  The Gemfile.lock file not only stores exact version information, but bundler USES that version information to rebuild the snapshot on production. If you take a look at your Gemfile (not Gemfile.lock) for example you will see the following line:
</p>

{% codeblock lang:ruby %}
gem 'coffee-rails', '~> 4.0.0'
{% endcodeblock %}

<p>
  Bundler knows that you used version 4.0.1 during development. When this file is pushed to production and you run a bundle install --deployment, bundler will recreate a snapshot of all of the gems that you were using on your development machine.
</p>

<p>
  When does this file get updated? Any time you add a new gem to your gemfile (and run a bundle install) or type bundle update [gem name] your Gemfile.lock will get updated. If you attempt to update the version of a Gem in your Gemfile, bundler will warn you to do a bundle update the next time you try to run a bundle install.
</p>

{% codeblock lang:ruby %}
You have requested:
  coffee-rails ~> 4.0.2

The bundle currently has coffee-rails locked at 4.0.1.
Try running 'bundle update coffee-rails'
{% endcodeblock %}

<p>
  In this case, you would be unable to proceed until you run bundle update coffee-rails, which would then update your Gemfile.lock to include the new version of coffee-rails. This is also why it's disastrous to run a bundle update without specifying a gem. Bundle update rebuilds the entire Gemfile.lock file from scratch, blowing away all of the old versions and replacing them with the latest ones allowed by the Gemfile.
</p>

<p>
  So far so good, the Gemfile.lock file is designed to save headache and frustration when deploying your application both across development machines as well as to production. It's always a good idea to make sure that you check this file into source control and be aware of how it works. That's it! See ya!
</p>