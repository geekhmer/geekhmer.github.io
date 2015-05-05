---
layout: post
title: "Ruby on Rails Generate Options"
date: 2015-04-19 09:48
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Ruby on Rails Generate Options
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails Generate Options" />
</p>

<p>
  There are many useful rails generate commands. I've listed some of them below for your reference:
</p>

{% codeblock lang:ruby %}
# Creates a model and controller, then adds a resource route to your routes.rb file.
rails g resource <attribute>:<type>  <attribute>:<type> 

# Just like rails g scaffold, but doesn't create the model.
rails g scaffold_controller <name>

# Creates a coffeescript/javascript and corresponding (s)css file.
rails g assets <name>

# Creates a jbuilder file
rails g jbuilder <name> <field>:<type> <field>:<type>

# Creates a custom helper.
rails g helper <name>

# Allows you to create your own custom generates to be used with rails generate.
rails g generator <name> 

# Creates a rake task.
rails g <namespace> <task_name> <task_name>
{% endcodeblock %}

<p>
  For a complete list of all generate commands, simply run rails g or rails generate from within a project.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
