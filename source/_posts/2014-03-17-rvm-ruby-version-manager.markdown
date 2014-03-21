---
layout: post
title: "RVM - Ruby Version Manager"
date: 2014-03-17 23:48
comments: true
categories: [Ruby]
keywords: ruby, Ruby, RVM, Ruby Version Manager, RVM - Ruby Version Manager, Installing RVM, Installing Rails, Install RVM, Install Rails, Gemset, gemset, Gemsets, gemsets, Creating Gemset for Rails Project, Creating Gemset for Specific Rails Project
---

<img src="/images/rvm.png" width="300" />
<p>
  Ruby is a very popular programming language that has Ruby on Rails or RoR is a popular development framework that allows you to easily get your application up and running with minimal hassle.<br/>
</p>
<p>
  Developing applications often times requires that you emulate different environments. Different versions of Ruby may be necessary for different projects. With conventional installations, this would impede your ability to be flexible.<br/>
</p>
<p>
  Luckily, the Ruby Version Manager, known more widely as RVM, allows you to easily install multiple, contained versions of Ruby and easily switch between them.
</p>
<p>
  <strong>Installing RVM</strong><br/>
  Run a quick update to make sure that all of the packages we download to our VPS are up to date:<br/>
  <code>sudo apt-get update</code><br/><br/>
  If you do not have curl on your system, you can start by installing it:<br/>
  <code>sudo apt-get install curl</code><br/><br/>
  To install RVM:<br/>
  <code>bash -s stable < <(curl -s https://raw.github.com/wayneeseguin/rvm/master/binscripts/rvm-installer)</code><br/><br/>
  To inject RVM into your environment .bashrc add the bit of bash it mentions at the end of the installation:<br/>
  <code>echo '[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"' >> .bashrc</code>
</p>
<p>
  <strong>Installing Ruby</strong><br/>
  Let’s go ahead and install Ruby MRI 1.9.3 (this is the default interpreter originally developed by Matz) first, and set that as our default interpreter. Afterwards, we’ll install Ruby 1.8.7, use RVM to easily switch between these two Rubies.<br/>
  Intall ruby version 1.9.3:<br/>
  <code>rvm install 1.9.3</code><br/><br/>
  To verify it works:<br/>
  <code>ruby -v</code><br/><br/>
  To make ruby version 1.9.3 as default version:<br/>
  <code>rvm --default 1.9.3</code><br/><br/>
  Let’s install ruby version 1.8.7:<br/>
  <code>rvm install 1.8.7</code><br/><br/>
  To switch ruby version:<br/>
  <code>rvm use 1.8.7</code> or <code>rvm 1.8.7</code>
</p>
<p>
  <strong>Installing Rails</strong><br/>
  To install the latest Rails:<br/>
  <code>gem install rails</code><br/>
</p>
<p>
  <strong>Gemsets</strong><br/>
  One common way to distribute code in Ruby is to use a format called gems. Gems can be installed to extend the capabilities of the core Ruby distribution, and there are often gems that are required to be installed to get certain programs to function correctly.<br/><br/>
  In keeping with RVM's mission of providing contained Ruby environments, it is also possible to install gems that are only associated with a single Ruby installation. RVM calls this functionality gemsets.<br/><br/>
  This means that you can have two different versions of the same gem, or you can make gems unaware of other gems on the system.<br/><br/>
  To see the available gemsets for the current Ruby:<br/>
  <code>rvm gemset list</code><br/><br/>
  If you have more than one Ruby version installed, you can see all of the gemsets:<br/>
  <code>rvm gemset list_all</code><br/><br/>
  By default, you should have two gemsets configured:<br/>
  - default: the gemset that is applied if no other gemset is specified.<br/>
  - global: this gemset is inherited by every other gemset that is used. This set generally does not need to be selected because it will be included automatically. You should install shared gems here.<br/>
</p>
<p>
  <strong>Creating Gemset for Rails Project</strong><br/>
  1. Create a Rails project:<br/>
  <code>rails new blog</code><br/><br/>
  2. Get in the project:<br/>
  <code>cd blog</code><br/><br/>
  3. Create gemset for the project:<br/>
  <code>rvm --rvmrc --create ruby_version@gemset_name</code><br/>
  <code>rvm --rvmrc --create 1.9.3@blog</code><br/><br/>
  4. Get out the project:<br/>
  <code>cd ..</code><br/><br/>
  5. Get in the project:<br/>
  <code>cd blog</code><br/><br/>
  6. To see the gemset that we have created and stand on:<br/>
  <code>rvm gemset list</code><br/><br/>
  So far so good whenever we cd into the project it will automatically switch to project's gemset.
</p>