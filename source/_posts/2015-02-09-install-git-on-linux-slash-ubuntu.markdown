---
layout: post
title: "Install Git on Linux/Ubuntu"
date: 2015-02-09 22:00
comments: true
categories: [Other]
keywords: Install Git on Linux/Ubuntu
---

<p>
  <img src="/images/logo_git.png" alt="Install Git on Linux/Ubuntu" />
</p>

<p>
  <strong>Overview</strong><br/>
  Git is a free and open source distributed version control system with an emphasis on speed, data integrity, and support for distributed, non-linear workflows.
</p>

<p>
  <strong>Installation</strong><br/>
  Use command line below to update local package index and then install the packages:
</p>

{% codeblock lang:ruby %}
sudo apt-get update
sudo apt-get install build-essential libssl-dev libcurl4-gnutls-dev libexpat1-dev gettext unzip
{% endcodeblock %}

<p>
  <strong>Setting Up Git</strong><br/>
  Now you have git installed, you need to do a few things so that the commit messages that will be generated for you will contain your correct information.
</p>

<p>
  You need to provide your name and email address by using <code>git config</code> because git embeds this information into each commit you do. You can go ahead and add this information by typing:
</p>

{% codeblock lang:ruby %}
git config --global user.name "Your Name"
git config --global user.email "youremail@domain.com"
{% endcodeblock %}

<p>
  You can see all of the configuration items that you have been set by typing command below:
</p>

{% codeblock lang:ruby %}
git config --list
{% endcodeblock %}

<p>
  What you will see:
</p>

{% codeblock lang:ruby %}
user.name=Your Name
user.email=youremail@domain.com
{% endcodeblock %}

<p>
  The information is stored in the configuration file, which you can optionally edit by hand with your text editor like this:
</p>

{% codeblock lang:ruby %}
vim ~/.gitconfig
{% endcodeblock %}

{% codeblock lang:ruby %}
[user]
  name = Your Name
  email = youremail@domain.com
{% endcodeblock %}

<p>
  So far so good, now you should have git installed and ready to use on your system. To learn more about how to use Git, check out these: <a href="https://try.github.io/levels/1/challenges/1" target="_blank">www.try.github.io</a>
</p>