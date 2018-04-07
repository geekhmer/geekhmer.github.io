---
layout: post
title: "Serve static files sitemap.xml, robots.txt and favicon.ico with Next.js"
date: 2018-04-07 15:22
comments: true
categories: [Next.js]
keywords: Serve static files sitemap.xml, robots.txt and favicon.ico with Next.js, React, Reactjs, Next, Nextjs, Next.js, Serve static files with Next.js, Serve static files with Next, Serve sitemap.xml with Next.js, Serve sitemap.xml with Next, Serve sitemap.xml in Next.js, Serve sitemap.xml in Next
---

<p>
  <img src="/images/nextjs.png" width="500" alt="Serve static files sitemap.xml, robots.txt and favicon.ico with Next.js" />
</p>

Well, to serve static files such as sitemap.xml, robots.txt and favicon.ico with Next.js you just put those static files in <code>static</code> folder and add the below code to your server (server.js) config:

{% codeblock lang:ruby %}
const robotsOptions = {
  root: __dirname + '/static/',
  headers: {
    'Content-Type': 'text/plain;charset=UTF-8',
  }
};
server.get('/robots.txt', (req, res) => (
  res.status(200).sendFile('robots.txt', robotsOptions)
));

const sitemapOptions = {
  root: __dirname + '/static/',
  headers: {
    'Content-Type': 'text/xml;charset=UTF-8',
  }
};
server.get('/sitemap.xml', (req, res) => (
  res.status(200).sendFile('sitemap.xml', sitemapOptions)
));

const faviconOptions = {
  root: __dirname + '/static/'
};
server.get('/favicon.ico', (req, res) => (
  res.status(200).sendFile('favicon.ico', faviconOptions)
));
{% endcodeblock %}

<p>
  So far so good, That’s it!!! See ya!!! :)
</p>