---
layout: post
title: "ChicagoBoss Installation"
date: 2015-01-28 22:52
comments: true
categories: [Erlang]
keywords: ChicagoBoss Installation, Install ChicagoBoss, ChicagoBoss Installing, Installing ChicagoBoss, Chicago Boss Installation, Install Chicago Boss, Chicago Boss Installing, Installing Chicago Boss
---

<p>
  <img src="/images/logo_chicagoboss.svg" alt="ChicagoBoss Installation" />
</p>

<p>
  <h3>Overview</h3>
</p>

<p>
  In software development, using a framework is almost a rule for fast, clean, easy readable and standardized coding. Chicago Boss (<a href="http://www.chicagoboss.org/" target="_blank">http://www.chicagoboss.org</a>)is a framework that is heavily inspired by Rails. Set up and use Chicago Boss is easy as falling off a log. Chicago Boss allows you to code with the aforementioned standards in Erlang. Plus, offers conveniences of modern web development, including WebSocket and Comet. Basic features of Chicago Boss listed below:<br/>
  - 100% asynchronous I/O<br/>
  - Support for Erlang and Elixir code<br/>
  - BossDB: Database connection layer with an advanced ORM which with built-in support for Mnesia, MongoDB, MySQL, PostgreSQL, Riak and Tokyo Tyrant<br/>
  - BossCache: Database caching layer<br/>
  - BossMQ: Cluster–wide, channel–based message queue<br/>
  - BossRouter: URL router<br/>
  - BossSession: Session storage layer<br/>
  - BossNews: Event listener, model event system<br/>
  - BossMail: Built-in email server<br/>
  - Django and Jade template support<br/>
  - Very clean controllers as result of pattern matching<br/>
  - Auto document generation for models<br/>
  - An useful admin interface<br/>
  - Automatic code reloading
</p>

<p>
  <img src="/images/hand_chicagoboss.png" alt="ChicagoBoss Installation" width="200" />
</p>

<p>
  <h3>Installation</h3>
</p>

<p>
  This short guide will help you get Chicago Boss installed and working on your system. Chicago Boss generally isn’t installed to global system path. Typically, Chicago Boss will be copied to a development directory in your path.
</p>

<p>
  <strong>Requirement</strong><br/>
  Chicago Boss is written in the Erlang programming language, so naturally you will need to install Erlang on your system. You may remember my previous article post about <a href="http://geekhmer.github.io/blog/2015/01/14/install-erlang-using-repository-on-ubuntu/">Install Erlang Using Repository on Ubuntu</a>.
</p>

<p>
  <strong>Download a Chicago Boss</strong><br/>
  Download the Chicago Boss source code from here: <a href="http://www.chicagoboss.org/" target="_blank">http://www.chicagoboss.org</a>.
</p>

<p>
  <strong>Open the Archive</strong><br/>
</p>

{% codeblock lang:ruby %}
tar xzf ChicagoBoss-*.*.*.tar.gz
{% endcodeblock %}

<p>
  <strong>Compile the Code</strong><br/>
</p>

{% codeblock lang:ruby %}
cd ChicagoBoss-*.*.*
make
{% endcodeblock %}

<p>
  <strong>Create a New Project</strong><br/>
</p>

{% codeblock lang:ruby %}
make app PROJECT=project_name
{% endcodeblock %}

<p>
  <strong>Enter the New Project Dir and Run the Development Server</strong><br/>
</p>

{% codeblock lang:ruby %}
cd ../project_name
./init-dev.sh
{% endcodeblock %}

<p>
  <strong>Enjoya</strong><br/>
  Point your browser to http://localhost:8001<br/>
  If all is well you will see a forbidding error message about the requested template — not to worry, the new project is empty so there is nothing to serve.
</p>

<p>
  So far so good, see you in the next articles. :)
</p>
