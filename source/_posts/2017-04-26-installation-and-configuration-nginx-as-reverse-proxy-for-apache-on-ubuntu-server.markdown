---
layout: post
title: "Installation and Configuration Nginx as Reverse Proxy for Apache on Ubuntu Server"
date: 2017-04-26 16:11
categories: [Other]
keywords: Installation and Configuration Nginx as Reverse Proxy for Apache on Ubuntu Server
---

<p>
  <img src="/images/nginx_apache.jpg" width="600" alt="Installation and Configuration Nginx as Reverse Proxy for Apache on Ubuntu Server" />
</p>

<p>
  In this article, I will show you how to install and configure Nginx as a caching reverse proxy for an Apache web server on Ubuntu, Nginx is used as the front-end and Apache as the back-end. 
</p>

<p>
  Nginx will run on port 80 to respond to requests from user/browser, the request will be forwarded to the Apache server that is running on port 7070.
</p>

<p>
  <h3>Apache</h3>
</p>

<p>
  <strong>Install Apache & PHP</strong><br/>
  Log into your ubuntu server with SSH and switch to root user by running:
</p>

{% codeblock lang:ruby %}
sudo su
{% endcodeblock %}

<p>
  Then install apache with the apt-get command:
</p>

{% codeblock lang:ruby %}
apt-get install apache2
{% endcodeblock %}

<p>
  Once apache is installed, we must install PHP as apache module for this tutorial:
</p>

{% codeblock lang:ruby %}
apt-get install php5 php5-mysql libapache2-mod-php5
{% endcodeblock %}

<p>
  <strong>Configure Apache and PHP</strong><br/>
  By default, apache listens on port 80. We have to configure apache to run on port 7070 for our proxy setup as port 80 will be used by nginx later.
</p>

<p>
  If you want to change the port for apache web server, you must edit the apache configuration file <code>/etc/apache2/ports.conf</code>, and then proceed with the virtual host configuration in the <code>/etc/apache2/sites-available/</code> directory.
</p>

<p>
  First change the Apache port to 7070 by editing the file <code>ports.conf</code> with the vim editor:
</p>

{% codeblock lang:ruby %}
vim /etc/apache2/ports.conf
{% endcodeblock %}

<p>
  And then change port 80 to 7070:
</p>

{% codeblock ports.conf lang:ruby %}
Listen 7070
{% endcodeblock %}

<p>
  And then save and exit.
</p>

<p>
  And now go to the virtualhost directory and edit the file <code>000-default.conf</code>:
</p>

{% codeblock lang:ruby %}
vim /etc/apache2/sites-available/000-default.conf
{% endcodeblock %}

<p>
  And then make sure your configuration is same as below:
</p>

{% codeblock 000-default.conf lang:ruby %}
<VirtualHost *:7070>
  ServerName www.reverse.com
  ServerAlias reverse.com

  ServerAdmin webmaster@localhost
  DocumentRoot /var/www/html/geekhmer-dev
   
  <Directory "/var/www/html/geekhmer-dev">
    RewriteEngine On
    RewriteBase /
    RewriteRule ^index\.php$ - [L]
    RewriteCond %{REQUEST_FILENAME} !-f
    RewriteCond %{REQUEST_FILENAME} !-d
    RewriteRule . /index.php [L]
  </Directory>

  ErrorLog ${APACHE_LOG_DIR}/geekhmer-dev_error.log
  CustomLog ${APACHE_LOG_DIR}/geekhmer-dev_access.log combined
</VirtualHost>
{% endcodeblock %}

<p>
  And then save and exit.
</p>

<p>
  Next test the configuration and restart apache:
</p>

{% codeblock lang:ruby %}
apachectl configtest

systemctl restart apache2
{% endcodeblock %}

<p>
  Then we verify that the apache and php is working by creating a new file with the name <code>info.php</code> in the directory <code>/var/www/html/</code>.

</p>

{% codeblock lang:ruby %}
echo "<?php phpinfo(); ?>" > /var/www/html/info.php
{% endcodeblock %}

<p>
  Visit your site www.reverse.com:7070/info.php.
</p>

<p>
  <h3>Nginx</h3>
</p>

<p>
  <strong>Install Nginx</strong><br/>
  Let install Nginx with the following apt-get command:
</p>

{% codeblock lang:ruby %}
apt-get install nginx
{% endcodeblock %}

<p>
  <strong>Configure Nginx</strong><br/>
  Once Nginx is installed, configure Nginx to act as reverse proxy for the apache web server that running on port 7070.
</p>

<p>
  Go to the nginx configuration directory and edit the file <code>nginx.conf</code>:
</p>

{% codeblock lang:ruby %}
vim /etc/nginx/nginx.conf
{% endcodeblock %}

<p>
  And then enable Gzip compression for Nginx by uncomment the gzip lines below:
</p>

{% codeblock nginx.conf lang:ruby %}
##
# Gzip Settings
##
gzip on;
gzip_disable "msie6";

gzip_vary on;
gzip_proxied any;
gzip_comp_level 6;
gzip_buffers 16 8k;
gzip_http_version 1.1;
gzip_types text/plain text/css application/json application/javascript text/xml application/xml application/xml+rss text/    javascript;
{% endcodeblock %}

<p>
  The most important is:<br/>
  - gzip on : to turn gzip compression.<br/>
  - gzip_types : is list of MIME-types which you want to turn the compression.<br/>
  - gzip_proxied any : is enable compression for proxied request.
</p>

<p>
  Under gzip settings, add these proxy cache settings:
</p>

{% codeblock nginx.conf lang:ruby %}
##
# Proxy Cache Settings
##
proxy_cache_path /var/cache levels=1:2 keys_zone=reverse_cache:60m inactive=90m max_size=1000m;
{% endcodeblock %}

<p>
  The important is:<br/>
  - The directory for the proxy cache is /var/cache.<br/>
  - levels : is a directive that tells Nginx how the cache is saved in file system.
<br/>
  - key_zone : is just a name of the cache zone, you can choose it freely, but don't add special chars or whitespace in the name. I will use the name "reverse_cache" here.
</p>

<p>
  And then save and exit.
</p>

<p>
  And then we will configure proxy params in <code>/etc/nginx/proxy_params</code> file for using in virtualhost later.
</p>

{% codeblock lang:ruby %}
vim /etc/nginx/proxy_params
{% endcodeblock %}

<p>
  Paste the configuration below:
</p>

{% codeblock proxy_params lang:ruby %}
proxy_set_header X-Real-IP $remote_addr;
proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
proxy_set_header Host $http_host;
proxy_set_header X-Forwarded-Proto $scheme;
proxy_set_header X-Forwarded-Host $host;

##
# Cache configuration
##
proxy_cache reverse_cache;
proxy_cache_valid 3s;
proxy_no_cache $cookie_PHPSESSID;
proxy_cache_bypass $cookie_PHPSESSID;
proxy_cache_key "$scheme$host$request_uri";
add_header X-Cache $upstream_cache_status;
{% endcodeblock %}

<p>
  And then save and exit.
</p>

<p>
  Now we will configure a virtualhost in the directory <code>/etc/nginx/sites-available</code>.  And I will create a new virtualhost configuration file named <code>reverse.conf</code>. Just got to the directory and create new file with vim:
</p>

{% codeblock lang:ruby %}
vim /etc/nginx/sites-available/reverse.conf
{% endcodeblock %}

<p>
  Paste the configuration below:
</p>

{% codeblock reverse.conf lang:ruby %}
server {
  listen 80;

  # Site Directory same in the apache virtualhost configuration
  root /var/www/html/geekhmer_dev;
  index index.php index.html index.htm;
  
  # Domain
  server_name www.reverse.com reverse.com;

  location / {
    try_files $uri @proxy;
  }

  location @proxy {
    proxy_pass http://127.0.0.1:7070;
    include /etc/nginx/proxy_params;
  }

  location ~* \.php$ {
    proxy_pass http://127.0.0.1:7070;
    include /etc/nginx/proxy_params;
  }

  # Enable Cache the file 30 days
  location ~* .(jpg|png|gif|jpeg|css|mp3|wav|swf|mov|doc|pdf|xls|ppt|docx|pptx|xlsx|css|js)$ {
    proxy_cache_valid 200 120m;
    # expires max;
    expires 30d;
    proxy_cache reverse_cache;
    access_log off;
  }

  # Disable Cache for the file type html, json
  location ~* .(?:manifest|appcache|html?|xml|json)$ {
    expires -1;
  }

  location ~ /\.ht {
    deny all;
  }
}
{% endcodeblock %}

<p>
  And then save and exit.
</p>

<p>
  And then activate the new virtualhost configuration:
</p>

{% codeblock lang:ruby %}
ln -s /etc/nginx/sites-available/reverse.conf /etc/nginx/sites-enabled/
{% endcodeblock %}

<p>
  And then we will test the nginx configuration and restart nginx:
</p>

{% codeblock lang:ruby %}
nginx -t

systemctl restart nginx
{% endcodeblock %}

<p>
  Nginx is configured as reverse proxy now. You can test it with curl:
</p>

{% codeblock lang:ruby %}
curl -I www.reverse.com

curl -I www.reverse.com/info.php
{% endcodeblock %}

<p>
  <h3>Configure Logging</h3>
</p>

<p>
  I will configure apache to log the real ip of the visitor instead of the local IP.
</p>

<p>
  Let go to install the apache module <code>libapache2-mod-rpaf</code> and edit the module configuration file:
</p>

{% codeblock lang:ruby %}
apt-get install libapache2-mod-rpaf

vim /etc/apache2/mods-available/rpaf.conf
{% endcodeblock %}

<p>
  Add the server IP to the line 10. My server IP is: 192.168.1.117.
</p>

{% codeblock rpaf.conf lang:ruby %}
RPAFproxy_ips 127.0.0.1 192.168.1.117 ::1
{% endcodeblock %}

<p>
  And then save and exit.
</p>

<p>
  And then restart apache:
</p>

{% codeblock lang:ruby %}
systemctl restart apache2
{% endcodeblock %}

<p>
  Test rpaf by viewing the apache access log with the tail command:
</p>

{% codeblock lang:ruby %}
tail -f /var/log/apache2/geekhmer-dev_access.log
{% endcodeblock %}

<p>
  Nginx is installed as reverse proxy in front of the Apache web server. If a visitor requests a php file, the request will be passed to apache on port 7070, and you can see the real IP visitor on the apache log file.
</p>

<p>
  <h3>Conclusion</h3>
</p>

<p>
  Nginx is fast and popular web server with low memory usage that can act as web server and reverse proxy for HTTP and HTTPS protocol. Nginx reverse proxy for apache is a setup that uses Nginx as front-end, and apache as back-end. Nginx handles the incoming request from the browser and passes it to the apache back-end. In this article, we have setup a configuration for nginx as http cache that caches PHP file requests and images.
</p>
