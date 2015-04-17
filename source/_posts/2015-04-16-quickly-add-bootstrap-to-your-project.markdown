---
layout: post
title: "Quickly Add Bootstrap to Your Project"
date: 2015-04-16 08:52
comments: true
categories: [Other]
keywords: Quickly Add Bootstrap to Your Project
---

<p>
  <img src="/images/bootstrap.png" alt="Quickly Add Bootstrap to Your Project" />
</p>

<p>
  Bootstrap is an amazing layout library that is awesome for building both prototypes as well as full blown websites. This very website makes extensive use of Bootstrap 3 to make things look awesome on both mobiles sites as well as the desktop.
</p>

<p>
  What if we want to quickly prototype out a site using bootstrap? Fortunately you can easily add bootstrap to your site without downloading anything, simply reference the associated files on the Yandex CDN. 
</p>

<p>
  Yandex is a worldwide search engine that is extremely popular in russia. Their CDN is extremely fast and has many different libraries available. To add bootstrap via yandex, simply include them like so (this also include jQuery, since Bootstrap JS requires it).
</p>

{% codeblock lang:javascript %}
<script src="http://yandex.st/jquery/1.11.1/jquery.min.js"></script>
<link href="http://yandex.st/bootstrap/3.1.1/css/bootstrap.min.css" media="screen" rel="stylesheet" />
<script src="http://yandex.st/bootstrap/3.1.1/js/bootstrap.min.js"></script>
{% endcodeblock %}

<p>
  A full list of the libraries available can be found on the <a href="https://tech.yandex.ru/jslibs/" target="_blank">yandex</a> website. Note that it's in russian, so you might need to translate it.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
