---
layout: post
title: "Use Git with Ruby on Rails Project"
date: 2015-02-09 23:31
comments: true
categories: [Other]
keywords: Use Git with Ruby on Rails Project, Git on Rails, Git on Ruby on Rails, Use Git with Rails Project, Create Ruby on Rails project and using Git
---

<p>
  <img src="/images/logo_git.png" alt="Install Git on Linux/Ubuntu" />
</p>

<p>
  Well, in the previous article I've shown you "<a href="http://geekhmer.github.io/blog/2015/02/09/install-git-on-linux-slash-ubuntu/">Install Git on Linux/Ubuntu</a>", and in this article I want to show you "Create Ruby on Rails project and using Git". Let’s run through this with me.
</p>

<p>
  <strong>Create Ruby on Rails Project</strong><br/>
  Type command below to create a Rails project:
</p>


{% codeblock lang:ruby %}
rails new todo -d mysql
{% endcodeblock %}

<p>
  <strong>Initialize Git</strong><br/>
  Type command below to initialize git:
</p>

{% codeblock lang:ruby %}
cd todo
git init
{% endcodeblock %}

<p>
  <strong>Ignore File/Dir</strong><br/>
  Git uses file <code>.gitignore</code> to determine which files and directories to ignore, before you make a commit.
</p>

<p>
  <code>.gitignore</code> file should be committed into your repository, in order to share the ignore rules with any other users that clone the repository.
</p>

<p>
  Type command below to create a <code>.gitignore</code> file:
</p>

{% codeblock lang:ruby %}
vim .gitignore
{% endcodeblock %}

<p>
  Then type file/dir or content that you want git to ignore:
</p>

{% codeblock .gitignore lang:ruby %}
log/*.log
tmp/**/*
config/database.yml
db/*.mysql
/public/assets/**
/vendor/bundle
{% endcodeblock %}

<p>
  Type command below to add Rails project into git:
</p>

{% codeblock lang:ruby %}
git add .
{% endcodeblock %}

<p>
  Type command below to commit into git:
</p>

{% codeblock lang:ruby %}
git commit -m "initialize income"
{% endcodeblock %}

<p>
  And type command below to push to the repository branch master:
</p>

{% codeblock lang:ruby %}
git push origin master
{% endcodeblock %}
