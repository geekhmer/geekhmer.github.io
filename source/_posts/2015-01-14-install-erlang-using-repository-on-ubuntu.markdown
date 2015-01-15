---
layout: post
title: "Install Erlang Using Repository on Ubuntu"
date: 2015-01-14 21:56
comments: true
categories: [Erlang]
keywords: Install Erlang Using Repository on Ubuntu, Install Erlang on Ubuntu, Install Erlang Environment Using Repository on Ubuntu, Install Erlang Using Environment on Ubuntu, Installing Erlang Using Repository on Ubuntu, Installing Erlang on Ubuntu, Installing Erlang Environment Using Repository on Ubuntu, Installing Erlang Using Environment on Ubuntu
---

<p>
  <img src="/images/logo_erlang.png" alt="Install Erlang on Ubuntu" />
</p>

<p>
  <strong>1. Adding Repository Entry</strong><br/>
  To add Erlang repository to your system, pls call the following commands:
</p>

{% codeblock lang:ruby %}
wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
sudo dpkg -i erlang-solutions_1.0_all.deb
{% endcodeblock %}

<p>
  <strong>Or Adding the Repository Entry Manually</strong><br/>
  Add one of the following lines to your /etc/apt/sources.list:
</p>

{% codeblock lang:ruby %}
deb http://packages.erlang-solutions.com/ubuntu trusty contrib
deb http://packages.erlang-solutions.com/ubuntu saucy contrib
deb http://packages.erlang-solutions.com/ubuntu precise contrib
{% endcodeblock %}

<p>
  And next, add the Erlang public key for apt-secure using following commands:
</p>

{% codeblock lang:ruby %}
wget http://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc
sudo apt-key add erlang_solutions.asc
{% endcodeblock %}

<p>
  <strong>2. Install Erlang</strong><br/>
  To install Erlang to your system, pls call the following commands:
</p>

{% codeblock lang:ruby %}
sudo apt-get update
sudo apt-get install erlang
{% endcodeblock %}

<p>
  So far so good, hope you enjoyed the article. see ya! :)
</p>
