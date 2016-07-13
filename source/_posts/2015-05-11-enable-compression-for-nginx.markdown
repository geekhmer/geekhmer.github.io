---
layout: post
title: "Enable Compression for Nginx"
date: 2015-05-11 15:08
categories: [Other]
keywords: Enable Compression for Nginx
---

<p>
  <img src="/images/nginx-plus.png" width="500" alt="Enable Compression for Nginx" />
</p>

<p>
  Using Nginx for your Rails app? Use this trick to speed up your site. If you are using Nginx, you can quickly and easily speed up your site by enabling gzip compression for your application. Gzip compression compresses files and assets before they are sent to the client, resulting in a nice little speed boost. A sample of how to do this is listed below:
</p>

{% codeblock nginx.conf lang:ruby %}
server {
  listen 80;
  server_name mysite.com www.mysite.com;
  root /opt/mysite.com/current/public;
  passenger_enabled on;
  gzip on;
  gzip_http_version 1.1;
  gzip_vary on;
  gzip_comp_level 6;
  gzip_proxied any;
  gzip_types text/plain text/html text/css application/json application/javascript application/x-javascript text/javascript text/xml application/xml application/rss+xml application/atom+xml application/rdf+xml;
{% endcodeblock %}

<p>
  The gzip types specifies the mime types that will be compressed using the gzip protocol. You can exclude and include other mime types using this particular list. It's best not to include things such as images and/or media files.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>

