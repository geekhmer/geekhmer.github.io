---
layout: post
title: "Gulp – Creating a Webpage"
date: 2016-10-12 09:16
comments: true
categories: [Gulp]
keywords: Gulp, Gulp – Creating a Webpage
---

<p>
  <img src="/images/bunlong_gulp.jpg" width="600" alt="Gulp, Gulp – Creating a Webpage" />
</p>

<p>
  Moving CSS and JavaScript files is all well and good, but we do actually want webpages right?
</p>

<p>
  Let’s start our webpage generation by first moving the <code>index.html</code> file we created while learning more about streams.
</p>

<p>
  It should look like:
</p>

{% codeblock /contents/index.html lang:ruby %}
<!DOCTYPE html>
<html>
  <head>
    <title>Learning Gulp</title>
  </head>
  <body>
    <h1>Hello Gulp!</h1>
  </body>
</html>
{% endcodeblock %}

<p>
  We will then create a simple homepage task to move the index.html file to our build directory.
</p>

{% codeblock gulpfile.js lang:ruby %}
...
gulp.task("homepage", function() {
  return gulp.src("contents/index.html")
    .pipe(gulp.dest("build"));
});
{% endcodeblock %}

<p>
  Now test the task.
</p>

{% codeblock gulpfile.js lang:ruby %}
$ gulp homepage

Using gulpfile ~/YOUR_DIRECTORY/gulpfile.js
Starting 'homepage'...
Finished 'homepage' after 15 ms
{% endcodeblock %}

<p>
  It would be nice to be able to preview our website as we generate the content. Let’s do that next.
</p>
