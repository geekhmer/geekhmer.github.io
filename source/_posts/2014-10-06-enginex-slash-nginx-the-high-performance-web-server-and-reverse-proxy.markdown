---
layout: post
title: "EngineX/Nginx the High-Performance Web Server and Reverse Proxy"
date: 2014-10-06 22:47
comments: true
categories: [Other]
keywords: EngineX/Nginx the High-Performance Web Server and Reverse Proxy, Nginx, EngineX, Engine X
---

<p>
  <img src="/images/nginx-plus.png" width="500" alt="Nginx plus" />
</p>

<p>
  Apache is the most popular Web server and one of the most successful open-source projects of all time. Apache has served more Web sites than any other Web server. Many of the world's largest Web sites, including YouTube, Facebook, Wikipedia , use Apache to serve billions of page views per month. Over the years, Apache has proven itself to be a very stable, secure and configurable Web server.
</p>

<p>
  Although Apache is an excellent Web server, what if there were an alternative with the same functionality, a simpler configuration and better performance?<br/>
  A Web server exists with better performance, and it's called Engine X or Nginx.
</p>

<p>
  Nginx is a high-performance Web server and reverse proxy. Nginx is used by some of the largest Web sites in the US, including WordPress etc, and it's currently serving about 500 million requests per day. Nginx is the fourth-most-popular Web server, and it is currently serving more than two million Web sites. 
</p>

<p>
  <strong>Why Nginx?</strong><br/>
  Like Apache, Nginx has all the features you would expect from a leading Web server:<br/>
  1. Static file serving<br/>
  2. SSL/TLS support<br/>
  3. Virtual hosts<br/>
  4. Reverse proxying<br/>
  5. Load balancing<br/>
  6. Compression<br/>
  7. Access controls<br/>
  8. URL rewriting<br/>
  9. Custom logging<br/>
  10. Server-side includes<br/>
  11. Caching
</p>

<p>
   The main advantages of Nginx over Apache are performance and efficiency. Nginx is able to serve more requests per second with less resources because of its architecture. It consists of a master process, which delegates work to one or more worker processes. Each worker handles multiple requests in an event-driven or asynchronous manner using special functionality from the Linux kernel. This allows Nginx to handle a large number of concurrent requests quickly with very little overhead. Apache can be configured to use either a process per request (pre-fork) or a thread for each request (worker). Although Apache's threaded mode performs much better than its pre-fork mode, it still uses more memory and CPU than Nginx's event-driven architecture.
</p>

<p>
  So far so good, next article I will show you how to install & use it. See you!
</p>