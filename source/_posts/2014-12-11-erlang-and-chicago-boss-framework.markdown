---
layout: post
title: "Erlang &amp; Chicago Boss framework"
date: 2014-12-11 21:19
comments: true
categories: Erlang
keywords: Erlang and Chicago Boss framework, Erlang, Chicago Boss framework, ChicagoBoss framework
---

<h3>Erlang</h3><br/>

<p>
  <img src="/images/logo_erlang.png" />
</p>

<p>
  You may remember <a href="http://geekhmer.github.io/blog/2014/06/26/why-erlang/">my first article post about Erlang</a>. I've shared a short introduction to the Erlang programming language and it's concurrency philosophy in the article. There are some highlights of that Erlang already include:
</p>

<p>
  <strong>High Availability and Reliability</strong><br/>
  - Simple and consistent error recovery and supervision hierarchiess.<br/>
  - Built-in fault tolerance (Let it fail/crash! - Do not program defensively).<br/>
  - Hot code loading during runtime (software upgrades with zero downtime).
</p>

<p>
  <strong>Scalability and Heterogeneity</strong><br/>
  - Run on multiple platforms, HetNet support.<br/>
  - Network aware runtime, out-of-the-box distributed architectures.<br/>
  - Very light-weight processes, highly scalable transparent or explicit concurrency.<br/>
  - Awesome transparent multi-core support:
</p>

<p>
  <img src="/images/concurrent.jpg" />
</p>

<p>
  <strong>Less Effort</strong><br/>
  - Functional programming language, high abstraction level, concise readable programs.<br/>
  - When compared with any imperative language, 4–20 times less code written for same application.<br/>
  - Suitable for rapid prototyping.<br/>
  - Impressive and powerful libraries and middleware (Open Telecom Platform - OTP).
</p>

<p>
  So, what is the "dark side" of Erlang for most developers? The correct answer is "The Erlang syntax!".
</p>

<p>
  In my first week with Erlang, I had no idea what I've been doing while coding something. Believe me, if you fall into the Erlang world from any imperative language, you'd feel like me. The problem could be good to discuss, but that's beside the point.
</p>

<h3>Chicago Boss framwork: Start small, dream big</h3>

<p>
  <img src="/images/logo_chicagoboss.svg" />
</p>

<p>
  In software development, using a framework is almost a rule for fast, clean, easy readable and standardized coding. Chicago Boss (<a href="http://www.chicagoboss.org/">http://www.chicagoboss.org/</a>) is a framework that is heavily inspired by Rails. Set up and use Chicago Boss is easy as falling off a log. Chicago Boss allows you to code with the aforementioned standards in Erlang. Plus, offers conveniences of modern web development, including WebSocket and Comet. Basic features of Chicago Boss listed below:<br/>
  - 100% asynchronous I/O<br/>
  - Support for Erlang and Elixir code<br/>
  - BossDB: Database connection layer with an advanced ORM which with built-in support for Mnesia, MongoDB, MySQL, PostgreSQL, Riak and Tokyo Tyrant.<br/>
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
  We'll cover almost all of features of Chicago Boss during developing an applicatioin. So far so good let enjoy in developing a simple application with Chicago Boss framwork. See ya!!! :)
</p>
