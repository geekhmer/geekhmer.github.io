---
layout: post
title: "Gems for Helping Speed Up Your Ruby on Rails Application"
date: 2014-06-30 10:55
comments: true
categories: [Ruby, Ruby on Rails]

keywords: gem, gems for helping speed up your Ruby on Rails application, speed up Ruby on Rails application
description: Gems for Helping Speed Up Your Ruby on Rails Application
---

<p>
  <strong>1. rails-footnotes</strong><br/>
  Description: This gem allows you to display footnotes in your application on pertinent information such as database queries, request parameters, etc. You can also create your own custom footnotes for objects in your application.<br/>
  Helping: It’s helpful for debugging your application and showing you how long your database query took.<br/>
  Source: <a href="https://github.com/josevalim/rails-footnotes" target="_blank">https://github.com/josevalim/rails-footnotes</a>
</p>

<p>
  <strong>2. bullet</strong><br/>
  Description: This gem watches your database queries and alerts you when it thinks you should use eager loading, when you’re using eager loading unnecessarily, and when to consider using counter cache.<br/>
  Helping: Unnecessary database queries slow down your application’s performance. Using eager loading and counter cache are two easy things you can implement for a performance boost. For high traffic websites, database queries can be the bottleneck for performance.<br/>
  Source: <a href="https://github.com/flyerhzm/bullet" target="_blank">https://github.com/flyerhzm/bullet</a>
</p>

<p>
  <strong>3. request-log-analyzer</strong><br/>
  Description: This gem outputs a performance report based on your application’s database request log file(s). It includes metrics such as average server time (the average time a server needs to respond to a user request) and cumulative server time (the sum of all the server time needed to handle all the requests for a given action on the server, i.e., the “load” on a server).<br/>
  Helping: It uses your log files to tell you how your server is responding to database requests and points you in the direction of code to optimize in your application.<br/>
  Source: <a href="https://github.com/wvanbergen/request-log-analyzer" target="_blank">https://github.com/wvanbergen/request-log-analyzer</a>
</p>

<p>
  <strong>4. ruby-prof</strong><br/>
  Description: This is a code profiling tool for MRI ruby implementations. It can generate graphical reports and gives information on call times, memory usage, and object allocation.<br/>
  Helping: It can help you figure out where your “slow code” is in your rails application.<br/>
  Source: <a href="https://github.com/ruby-prof/ruby-prof" target="_blank">https://github.com/ruby-prof/ruby-prof</a>
</p>

<p>
  <strong>5. rack-mini-profiler</strong><br/>
  Description: This is originally a .NET tool ported over to Ruby that displays a speed profile badge on each html page you navigate to.<br/>
  Helping: If a page feels “slow”, MiniProfiler can give you a good idea of where the bottleneck is. It also lets you know which “sessions” you have not seen and displays them to you the next time you access your user interface. It allows you to easily see how much time you’re spending on database queries versus other non-SQL related bottlenecks.<br/>
  Source: <a href="https://github.com/MiniProfiler/rack-mini-profiler" target="_blank">https://github.com/MiniProfiler/rack-mini-profiler</a>
</p>