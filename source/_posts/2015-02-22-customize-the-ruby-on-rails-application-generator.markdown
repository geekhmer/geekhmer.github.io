---
layout: post
title: "Customize the Ruby on Rails Application Generator"
date: 2015-02-22 13:05
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Customize the Ruby on Rails Application Generator, Customize the Rails Application Generator, Customize the Ruby on Rails App Generator, Customize the Rails App Generator
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Customize the Ruby on Rails Application Generator" />
</p>

<p>
  So far if you create a Rails app, you know that it can quickly to do things like:<br/>
  - Add RSpec and Cucumber.<br/>
  - Disable/turn off Test Unit.<br/>
  - Switch the default database engine to PostgreSQL.<br/>
  - Add other Gems such as HAML, CoffeeScript, ...
</p>

<p>
  Luckily we can customize what the Rails app generator generates. In this article, I will use Rails app templates to customize the default behavior of the Rails application generator. Let’s run through this with me.
</p>

<p>
  <strong>Rails App Templates</strong><br/>
  Rails app templates allow you to run actual commands during the generation process. For example, I want to install rspec and haml, create a readme.md file, and commit the whole thing to git. I can easily do this by creating an Rails application template. Let create a file called app_template.rb and add the codes below:
</p>

{% codeblock app_template.rb lang:ruby %}
remove_file 'README.doc'
create_file 'README.md'

gem 'rspec-rails'
gem 'haml-rails'
run 'bundle install'
generate 'rspec:install'

git :init
git add: '--all', commit: '-m "Initialize Commit"'
{% endcodeblock %}

<p>
  If you generate a new app with the parameters <code>-m app_template.rb</code> it will run the instructions you listed in the template. The code listed above will get executed when you run the following command:
</p>

{% codeblock lang:ruby %}
rails new blog -m app_template.rb
{% endcodeblock %}

<p>
  If you browse your Rails project you'll see that rspec, haml got installed and the default readme file was removed and a readme.md file was added, and finally everything was committed to git.
</p>

<p>
  In addition, you can have the application template prompt the user for more information. The code listed below allows you to do like this:
</p>

{% codeblock app_template.rb lang:ruby %}
remove_file 'README.doc'
create_file 'README.md'

gem 'rspec-rails'
gem 'haml-rails'
run 'bundle install'
generate 'rspec:install'

if yes? 'Do you wish to generate a root controller? (y/n)'
  name = ask('What do you want to call it?').underscore 
  generate :controller, "#{name} show"
  route "root to: '#{name}\#show'"
  route "resource :#{name}, only: [:show]"
end

git :init
git add: '--all', commit: '-m "Initial Commit"'
{% endcodeblock %}

<p>
  In the code above, the yes? function will prompt the user with a yes/no question. The ask function will prompt the user for text input. In this case, we ask the user if they wish to generate a root controller, and if they say yes, we prompt them for a name.
</p>

<p>
  So far so good, That's it! See ya!
</p>