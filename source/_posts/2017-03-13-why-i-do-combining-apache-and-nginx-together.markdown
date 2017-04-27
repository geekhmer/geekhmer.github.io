---
layout: post
title: "Why I do combining apache and nginx together?"
date: 2017-03-13 10:08
comments: true
categories: [Other]
keywords: Why I do combining apache and nginx together?
---

<p>
  <img src="/images/nginx_apache.jpg" width="600" alt="Why I do combining apache and nginx together?" />
</p>

<p>
  Nginx and apache are powerful and effective servers by nowadays. Apache currently reigns as the number 1 server for websites and since its public release in 2006. And nginx has taken the world by storm and is now the number 2 server for active sites. 
</p>

<h3>The Reason Why I Use Nginx and Apache Together is</h3>

<p>
  <strong>Nginx</strong><br/>
  - nginx serves static files (images, css, html etc.) really fast and efficient and passes php and .htaccess requests to apache for processing.<br/>
  - nginx needs the help of php-fpm or similar modules for dynamic content.
</p>

<p>
  <strong>Apache</strong><br/>
  - apache is hard on server memory.<br/>
  - apache serves php and .htaccess (most cms site like wordpress needs it for rewrite) and if you throw in a php opcode cache like zend opcache or xcache it should serve php even faster.
</p>

<p>
  <strong>nginx + apache with php opcache = performance and server resource efficiency</strong>
</p>

<p>
  So far so good, in the next article I will show you the configurations. That's it!!! See ya!!! :)
</p>
