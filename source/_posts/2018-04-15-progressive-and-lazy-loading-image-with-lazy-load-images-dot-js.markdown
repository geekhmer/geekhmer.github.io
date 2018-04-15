---
layout: post
title: "Progressive &amp; Lazy Loading Image with lazy-load-images.js"
date: 2018-04-15 18:08
comments: true
comments: true
categories: [Javascript, Other]
keywords: progressive image, progressive images, progressive image loading, progressive images loading, lazy image, lazy images, lazy image loading, lazy images loading, lazy image loading with lazy-load-images.js, lazy images loading with lazy-load-images.js, progressive and lazy loading image with lazy-load-images.js, progressive and lazy loading images with lazy-load-images.js
---

<p>
  <img src="/images/lazy-load-images.png" width="700" alt="Lazy load images" />
</p>

<p>
  With images making up a whopping 65% of all web content, page load time on websites can easily become an issue.
</p>

<p>
  Images can weight quite a bit. This can have a negative impact on the time visitors have to wait before they can access content on your website. they will get navigate somewhere else, unless you come up with a solution to image loading.
</p>

<p>
  <strong>What is lazy loading?</strong><br/>
  Lazy loading images means loading images on websites asynchronously that is, after the content is fully loaded, or even conditionally, only when they appear in the browser’s viewport. This means that if users don’t scroll all the way down, images placed at the bottom of the page won't be loaded.
</p>

<p>
  <strong>What reason you should care of lazy loading images?</strong><br/>
  There are many reasons you should consider of lazy loading images for your website:
</p>

<p>
  If your website uses JavaScript to display content or provide some functionality to users, loading the DOM quickly becomes critical. It’s common for scripts to wait until the DOM has completely loaded before they start running. On a site with a number of images, lazy loading or loading images asynchronously could make the difference between users staying or leaving your website.
</p>

<p>
  Since most lazy loading solutions work by loading images only if the user has scrolled to the location where images would be visible inside the viewport, those images will never be loaded if users never get to that point. This means considerable savings in bandwidth, for which most users, especially those accessing the web on mobile devices and slow-connections.
</p>

<p>
  Lazy loading images helps with website performance, but what’s the best way to go about it?
</p>

<p>
  Well, <a href="https://github.com/codefacebook/lazy-load-images.js" target="_blank">lazy-load-images.js</a> is a javascript library which could help you with the website performance.
</p>

<p>
  <strong>lazy-load-images.js is loading with blurred image effect</strong><br/>
  If you are a Medium reader, you have certainly noticed how the site loads the main image inside a post.
</p>

<p>
  The first thing you see is a blurred, low-resolution copy of the image, while its high-resversion is being lazy loaded:
</p>

<p>
  Blurred placeholder image on <a href="https://lazyloadimages.github.io/" target="_blank">lazy-load-images.js</a> website:
</p>

<p>
  <img src="/images/lazy-load-images-tiny.png" width="700" alt="Lazy load images" />
</p>

<p>
  High-res, lazy loaded image on <a href="https://lazyloadimages.github.io/" target="_blank">lazy-load-images.js</a> website:
</p>

<p>
  <img src="/images/lazy-load-images-full.png" width="700" alt="Lazy load images" />
</p>

<p>
  You can lazy load images with this interesting blurring effect in a number of ways.
</p>

<p>
  My favorite technique/library is using <a href="https://github.com/codefacebook/lazy-load-images.js" target="_blank">lazy-load-images.js</a>. Here’s all the features/goodness:<br/>
  - Fast loading<br/>
  - High performance<br/>
  - Supports all images type<br/>
  - Responsive images<br/>
  - Supports all modern browsers Chrome, Firefox, Safari, (IE10+), ... etc.
</p>

<p>
  You can read all details and download the lazy-load-images.js library on the project's <a href="https://github.com/codefacebook/lazy-load-images.js" target="_blank">GitHub repo</a>.
</p>
