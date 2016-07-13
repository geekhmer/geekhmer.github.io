---
layout: post
title: "5 Tips to Scale Your Ruby on Rails Application"
date: 2015-05-19 11:39
comments: true
categories: [Ruby, Ruby on Rails]
keywords: 5 Tips to Scale Your Ruby on Rails Application
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="5 Tips to Scale Your Ruby on Rails Application" />
</p>

<p>
  There are lots of resources on the web that explain how to scale web applications and how to scale Rails. Here's a quick summary of just five of the basic strategies for a scalable Ruby on Rails web application. Some of them are specific to Ruby on Rails; others generalize to any shared-nothing application server architecture.
</p>

<p>
  <strong>1. Cache, cache, cache and More Cache</strong><br/>
  Cache at the client and use Ajax libraries like JQuery to stream in data to the browser on demand. Use gateway /reverse proxy caches to cache HTTP responses at your website, and learn how to use expiration and etags. Take full advantage of Rails' built-in action, page and fragment caching. Use memcache to cache results that you'd otherwise pull from your database.
</p>

<p>
  <strong>2. Segregate Data and Data Serving</strong><br/>
  Don't munge all your data storage into a single database "for convenience." Datasets that are independent should go into separate databases. Serve static assets from a separate tier, or use Amazon S3 or a CDN like Akamai to serve those assets. It's more expensive, but it simplifies scaling. Relational databases scale up, not out, so sit down and have a heart to heart talk with your DBA over whether you really need a relational data model for all your data stores. Maybe you can get away with a simpler key-value data store for some of your simpler data. There are ruby clients, so use Hadoop for scaling the storage and analysis of large amounts of unstructured data. Also know the scalability limitations of whatever file system you're using. If you have heavy data reporting needs, do your reporting from a copy of your main database, not from your production database.
</p>

<p>
  <strong>3. Minimize & Handle External Dependencies</strong><br/>
  Watch for dependencies on external services like ad serving networks or RSS feeds. If a service isn't responding or can't handle your growing request load, make sure that you have a fallback strategy.
</p>

<p>
  <strong>4. Tend Your Database and Your Job Handlers</strong><br/>
  Any ORM, including Rails' ActiveRecord can generate SQL queries that cause database performance issues. Make sure you're looking at your slow query log after each major integration to make sure you don't have "missing" database indices, and haven't written inappropriate find-all's in your Rails code. Scrub your database periodically for indices that are no longer being used. Similarly, watch the resource consumption of your background and scheduled jobs. As your user base grows jobs can start to overlap, and daily log processing can start to take more than 24 hrs! This kind of thing can sneak up on you easily. Ideally, segregate your jobs in a separate tier. And, as you grow, look at moving to a message based job handler.
</p>

<p>
  <strong>5. Shard Your Unavoidably Relational Data</strong><br/>
  At high scaling levels, your MySQL database will have to be sharded. Sharding involves breaking up your datasets into independent pieces based on a key. For many consumer-oriented Rails sites, that can mean sharding based on userid's, but other sharding schemes use data-age, or access-frequency (if that's predictable).
</p>
