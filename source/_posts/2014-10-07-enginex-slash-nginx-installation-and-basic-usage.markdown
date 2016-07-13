---
layout: post
title: "EngineX/Nginx Installation and Basic Usage"
date: 2014-10-07 21:52
comments: true
categories: [Other]
keywords: EngineX/Nginx Installation and Basic Usage, Nginx, EngineX, Engine X
---

<p>
  <img src="/images/nginx_installation.png" width="500" alt="Nginx plus" />
</p>

<p>
  Nginx is available in most Linux distributions. In this article, I use Ubuntu 14.10.
</p>

<p>
  <strong>Installation</strong><br/>
  Open your terminal and run the following command as root user:
</p>

{% codeblock lang:ruby %}
apt-get install nginx
{% endcodeblock %}

<p>
  Now Nginx is installed, you can use the startup script to start, stop or restart the Web server:
</p>

{% codeblock lang:ruby %}
/etc/init.d/nginx start
/etc/init.d/nginx stop
/etc/init.d/nginx restart
{% endcodeblock %}

<p>
  Most configuration changes do not require to restart, in which case you can use the reload command. It is generally a good idea to test the Nginx configuration file for errors before reloading:
</p>

{% codeblock lang:ruby %}
nginx -t
/etc/init.d/nginx reload
{% endcodeblock %}

<p>
  Let's go ahead and start the server:
</p>

{% codeblock lang:ruby %}
/etc/init.d/nginx start
{% endcodeblock %}

<p>
  Nginx now should be running on your machine. If you open http://127.0.0.1/ or http://localhost in your browser, you should see a page with “Welcome to nginx!”.
</p>

<p>
  <strong>Main Configuration File (/etc/nginx/nginx.conf)</strong><br/>
  Now Nginx is installed, let's take a look at its config file that located at /etc/nginx/nginx.conf. This file contains the server-wide settings for Nginx, and it should look similar to this:
</p>

{% codeblock /etc/nginx/nginx.conf lang:ruby %}
user www-data;
worker_processes  1;
error_log  /var/log/nginx/error.log;
pid  /var/run/nginx.pid;

events {
  worker_connections  1024;
}

http {
  include /etc/nginx/mime.types;
  default_type application/octet-stream;
  access_log /var/log/nginx/access.log;
  sendfile on;
  keepalive_timeout 65;
  tcp_nodelay on;
  gzip on;
  include /etc/nginx/sites-enabled/*;
}
{% endcodeblock %}

<p>
  We are not going to change any of these settings, but let's talk about some of them to help us understand how Nginx works:
</p>

<p>
  worker_processes setting tells Nginx how many child processes to start. If your server has more than one processor or is performing large amounts of disk IO, you might want to try increasing this number to see if you get better performance.
</p>

<p>
  worker_connections setting limits the number of concurrent connections per worker process. To determine the maximum number of concurrent requests, you simply multiply worker_processes by worker_connections.
</p>

<p>
  error_log and access_log settings indicate the default logging locations. You also can configure these settings on a per-site basis, as you will see later in the next article. Like Apache, Nginx is configured to run as the www-data user, but you easily can change this with the user setting. The startup script for Nginx needs to know the process ID for the master process, which is stored in /var/run/nginx.pid, as indicated by the pid setting.
</p>

<p>
  sendfile setting allows Nginx to use a special Linux system call to send a file over the network in a very efficient manner. The gzip option instructs Nginx to compress each response, which uses more CPU but saves bandwidth and decreases response time. Additionally, Nginx provides another compression module called gzip precompression (available as of version 0.6.24). This module looks for a compressed copy of the file with a .gz extension in the same location and serves it to gzip-enabled clients. This prevents having to compress the file each time it's requested.
</p>

<p>
  The last setting we are concerned with is the include directive for the sites-enabled directory. Inside /etc/nginx, you'll see two other directories, /etc/nginx/sites-available and /etc/nginx/sites-enabled. For each Web site you want to host with Nginx, you should create a config file in /etc/nginx/sites-available, then create a symlink in /etc/nginx/sites-enabled that points to the config file you created. The main Nginx config file includes all the files in /etc/nginx/sites-enabled. This helps organize your configuration files and makes it very easy to enable and disable specific Web sites.
</p>

<p>
  So far so good, next article I will show you how to work with Nginx. :)
</p>