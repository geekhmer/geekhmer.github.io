---
layout: post
title: "Gulp – Web Server"
date: 2016-10-12 16:12
comments: true
categories: [Gulp]
keywords: Gulp, Gulp – Web Server
---

<p>
  <img src="/images/bunlong_gulp.jpg" width="600" alt="Gulp, Gulp – Web Server" />
</p>

<p>
  We can actually serve our webpages by using a gulp plugin.
</p>

{% codeblock lang:ruby %}
$ npm install gulp-webserver --save-dev
{% endcodeblock %}

<p>
  For more information about <code>gulp-webserver</code> check out <a href="https://www.npmjs.com/package/gulp-webserver" target="_blank">https://www.npmjs.com/package/gulp-webserver</a>
</p>

{% codeblock gulpfile.js lang:ruby %}
...
var webserver = require('gulp-webserver');
...
gulp.task('webserver', function() {
  return gulp.src('build')
    .pipe(webserver());
});
{% endcodeblock %}

<p>
  Now when we run our gulp task webserver we will have a local webserver to view our website.
</p>

{% codeblock lang:ruby %}
$ gulp webserver

Using gulpfile ~/YOUR_DIRECTORY/gulpfile.js
Starting 'webserver'...
Webserver started at http://localhost:8000
Finished 'webserver' after 20 ms
{% endcodeblock %}

<p>
  If you go to <code>http://localhost:8000</code> in your web browser you should see our index.html page saying Hello Gulp!.
</p>
