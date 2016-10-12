---
layout: post
title: "Gulp – Hello World"
date: 2016-08-28 17:54
comments: true
categories: [Gulp]
keywords: Gulp, Gulp – Hello World
---

<p>
  <img src="/images/gulp.jpg" width="600" alt="Gulp – Hello World" />
</p>

<p>
  Let’s start a new node project in our folder and add a package.json by using command below:
</p>

{% codeblock lang:ruby %}
npm init
{% endcodeblock %}

<p>
  Time to install gulp using npm. First globally to access the gulp command and then locally for our package.json .
</p>

{% codeblock lang:ruby %}
npm install gulp -g
npm install gulp --save-dev
{% endcodeblock %}

<p>
  By default gulp looks for a <code>gulpfile.js</code> to run. Let’s create a simple gulpfile.js.
</p>

{% codeblock gulpfile.js lang:ruby %}
var gulp = require('gulp');

gulp.task('default', [], function() {
  console.log("Hellow World");
});
{% endcodeblock %}

<p>
  In your terminator run the gulp command below:
</p>

{% codeblock lang:ruby %}
gulp
{% endcodeblock %}

<p>
  You should see:
</p>

{% codeblock lang:ruby %}
Hello Gulp

Using gulpfile ~/YOUR_DIRECTORY/gulpfile.js
Starting 'default'...
Hello Gulp! You are mighty fine.
Finished 'default' after ...
{% endcodeblock %}

<p>
  Congratulations creating your first gulp build script. So far so good, That’s it!!! See ya!!! :)
</p>
