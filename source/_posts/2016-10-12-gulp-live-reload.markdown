---
layout: post
title: "Gulp – Live Reload"
date: 2016-10-12 16:24
comments: true
categories: [Gulp]
keywords: Gulp, Gulp – Live Reload
---

<p>
  <img src="/images/bunlong_gulp.jpg" width="600" alt="Gulp, Gulp – Live Reload" />
</p>

<p>
  So far so good, lets link our css file in <code>index.html</code>.
</p>

{% codeblock /contents/index.html lang:ruby %}
<!DOCTYPE html>
<html>
  <head>
    <title>Learning Gulp</title>
    <link rel="stylesheet" href="/styles/main.min.css" />
  </head>
  <body>
    <h1>Hello Gulp!</h1>
  </body>
</html>
{% endcodeblock %}

<p>
  Now let's turn on live reload with our <code>gulp-webserver</code>.
</p>

{% codeblock gulpefile.js lang:ruby %}
gulp.task('webserver', function() {
  return gulp.src('build')
    .pipe(webserver({ livereload: true }));
});
{% endcodeblock %}

<p>
  If we run <code>gulp webserver</code> in one terminator and <code>gulp watch</code> in another, we will have our webserver running and live refreshing on each build.
</p>

<p>
  terminator1:
</p>

{% codeblock lang:ruby %}
$ gulp webserver
{% endcodeblock %}

<p>
  terminator2:
</p>

{% codeblock lang:ruby %}
$ gulp watch
{% endcodeblock %}

<p>
  Update the css file to:
</p>

{% codeblock /contents/styles/some_styles.css lang:ruby %}
h1 {
  color: blue;
}
{% endcodeblock %}

<p>
  Go to <code>http://localhost:8000</code> to watch our webpage.
</p>
