---
layout: post
title: "Gulp – Minify Our CSS"
date: 2016-10-03 16:22
comments: true
categories: [Gulp]
keywords: Gulp, Gulp – Minify Our CSS
---

<p>
  <img src="/images/gulpjs.jpg" width="600" alt="Gulp, Gulp – Minify Our CSS" />
</p>

<p>
  Now since we have our css in a single file, we can continue to increase the performance of our site by minifying our css. 
</p>

<p>
  Minifying is the process of eliminating all the unnecessary formatting in a css file. Human’s need spaces and tabs (also known as white space) to more easily read the styles. A browser doesn’t care about white space so we can make the file smaller by removing them.
</p>

<p>
  You should start seeing a pattern when using gulp... because for this we need to use a plugin:
</p>

{% codeblock lang:ruby %}
$ npm install gulp-minify-css --save-dev
{% endcodeblock %}

<p>
  For more information on gulp-minify-css, check out <a href="https://www.npmjs.org/package/gulp-minify-css" target="_blank">https://www.npmjs.org/package/gulp-minify-css</a>.
</p>

{% codeblock gulpfile.js lang:ruby %}
var gulp = require('gulp');
var concat = require('gulp-concat');
var clean = require('gulp-rimraf');
var cssmin = require("gulp-minify-css");

gulp.task('clean', [], function() {
  console.log("Clean all files in build folder");

  return gulp.src("build/*", { read: false }).pipe(clean());
});

gulp.task('default', ['clean'], function() {
  console.log("Concat, move, and minify all the css files in styles folder");
  return gulp.src("contents/styles/**.css")
    .pipe(concat('main.min.css'))
    .pipe(cssmin())
    .pipe(gulp.dest('build/styles'));
});
{% endcodeblock %}

<p>
  Open up your terminator to run gulp:
</p>

{% codeblock lang:ruby %}
$ gulp

Using gulpfile ~/YOUR_DIRECTORY/gulpFile.js
Starting 'clean'...
Clean all files in build folder
Finished 'clean' after 18 ms
Starting 'default'...
Concat, move, and minify all the css files in styles folder
Finished 'default' after 40 ms
{% endcodeblock %}

<p>
  Our <code>build/styles/main.min.css</code> should now look like:
</p>

{% codeblock lang:ruby %}
p{font-size:30px}h1{color:red}
{% endcodeblock %}

<p>
  So far so good, That’s it!!! See ya!!! :)
</p>
