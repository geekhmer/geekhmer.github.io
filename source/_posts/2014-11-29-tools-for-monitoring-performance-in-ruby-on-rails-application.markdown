---
layout: post
title: "Tools for Monitoring Performance in Ruby on Rails Application"
date: 2014-11-29 22:31
comments: true
categories: [Other]
keywords: Tools for Monitoring Performance in Ruby on Rails Application, Ruby, Ruby on Rails, Rails 4, Ruby on Rails 4
---

<p>
  <img src="/images/move_to_rails.png" alt="Tools for Monitoring Performance in Ruby on Rails Application" />
</p>

<p>
  Here are some tips for you to monitor performance in a Ruby on Rails application. Some tools and tips are equally applicable for other web applications:
</p>

<p>
  <strong>1. Rails Logger</strong><br/>
  The simplest way to get information about performance is to analyze rails log. It will provide you information of the time spent processing each request, broken down into rendering and SQL time. You can also find whether a particular part are cached or the occurrence of cache expiry.
</p>

<p>
  <strong>2. Rails Performance Test - Profiling</strong><br/>
  Profiling helps you to see the details of a performance test and provide an in-depth picture of the slow and memory hungry parts. Each test case is run 1 time in profiling mode.
</p>

<p>
  <strong>3. Rails Performance Test - Benchmarking</strong><br/>
  Through Rails performance test, source of application’s memory or speed bottleneck and can be found. Benchmarking helps find out how fast each performance test runs. Each test case is run 4 times in benchmarking mode.
</p>

<p>
  <strong>4. <a href="https://github.com/wvanbergen/request-log-analyzer" target="_blank">Rails Analyzer</a></strong><br/>
  The Rails Analyzer project contains a collection of tools (The Production Log Analyzer, The Action Profiler, Rails Analyzer Tools, The SQL Dependency Grapher) for Rails that let you discover biggest slow spots in your applications allowing you to best focus optimization efforts.
</p>

<p>
  <strong>5. <a href="https://github.com/josevalim/rails-footnotes" target="_blank">Rails Footnote</a></strong><br/>
  It is a rails plugin which displays footnotes in your application for easy debugging, such as sessions, request parameters, cookies, filter chain, routes, queries, etc. Even more, it contains links to open files directly in your editor including your backtrace lines.
</p>

<p>
  <strong>6. <a href="https://github.com/nesquena/query_reviewer" target="_blank">Query Reviewer</a></strong><br/>
  Query Reviewer is an advanced SQL query analyzer. It generates a page with explanation output of all SELECT queries, rate a page’s SQL usage, display interactive summary on page.
</p>

<p>
  <strong>7. <a href="https://github.com/sdsykes/slim_scrooge" target="_blank">Slim Scrooge</a></strong></strong><br/>
  SlimScrooge is an optimization layer to ensure your application only fetches the database content needed to minimize wire traffic, excessive SQL queries and reduce conversion overheads to native Ruby types.<br/>
  SlimScrooge implements inline query optimization, automatically restricting the columns fetched based on what was used during previous passes through the same part of your code.
</p>

<p>
  <strong>8. <a href="http://newrelic.com" target="_blank">New Relic</a></strong><br/>
  New Relic is the all-in-one web application performance tool that lets you see performance from the end user experience down to the line of application code. It will also list the errors if such occurrence occur.
</p>

<p>
  <strong>9. <a href="https://github.com/brynary/rack-bug" target="_blank">Rack-Bug</a></strong><br/>
  It is a debugging toolbar for Rack applications implemented as middleware.
</p>

<p>
  <strong>10. <a href="https://github.com/mperham/sidekiq" target="_blank">Sidekiq</a></strong><br/>
  Sidekiq uses threads to handle many jobs at the same time in the same process. It does not require Rails but will integrate tightly with Rails 3/4 to make background processing dead simple.
</p>

<p>
  <strong>11. <a href="http://getfirebug.com" target="_blank">Firebug</a></strong><br/>
  Firebug is a firefox plugin and a very powerful tool. Apart from many outstanding features, it helps to monitor network performance. You can see the load time of each files (when they started to load and when they are completed), filter it by type (e.g. javascript or CSS) and examining http headers.
</p>

<p>
  So far so good, you can suggest more. These are a mere introduction. You can try those out and examine the health of your application. I have a plan to write another one with tips regarding enhancing performance based on the generated health report of a web application. Stay tuned for that!
</p>