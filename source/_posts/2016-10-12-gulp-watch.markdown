---
layout: post
title: "Gulp – Watch"
date: 2016-10-12 16:02
comments: true
categories: [Gulp]
keywords: Gulp, Gulp – Watch
---

<p>
  <img src="/images/bunlong_gulp.jpg" width="600" alt="Gulp, Gulp – Watch" />
</p>

<p>
  Now for something super amazing. Instead of running the gulp task explicitly, lets have gulp run our tasks when the files change.
</p>

<p>
  First reorganize some of our tasks:<br/>
  - Rename default task to css.<br/>
  - Create a new default task to run css, 'javascript', and 'homepage' tasks.
</p>

{% codeblock gulpfile.js lang:ruby %}
...
gulp.task('css', ['clean'], function() {
  console.log("Concat, move, and minify all the css files in styles folder");
  return gulp.src("contents/styles/**.css")
    .pipe(concat('main.min.css'))
    .pipe(cssmin())
    .pipe(gulp.dest('build/styles'));
});
...
gulp.task('default', ['css', 'homepage', 'javascript']);
...
{% endcodeblock %}

<p>
  Next create our file watching task. Could you guess what?... there isn’t a plugin for this. It is just part of gulp.
</p>

<p>
  We will create a gulp watch task to watch our contents folder and run our default task on file change.
</p>

{% codeblock gulpfile.js lang:ruby %}
...
gulp.task('watch', [], function() {
  return gulp.watch(['contents/**'], ['default']);
});
...
{% endcodeblock %}

<p>
  In the terminal type:
</p>

{% codeblock lang:ruby %}
$ gulp watch

Using gulpfile ~/YOUR_DIRECTORY/gulpfile.js
Starting 'watch'...
Finished 'watch' after 11 ms
{% endcodeblock %}

<p>
  If you update any of the css files in the styles folder, you should see gulp run the default task.
</p>

{% codeblock lang:ruby %}
Starting 'clean'...
Clean all files in build folder
Finished 'clean' after 21 ms
Starting 'css'...
Concat, move, and minify all the css files in styles folder
Starting 'homepage'...
Starting 'javascript'...
Validate, Concat, Uglify, and Move all the javascript files
Finished 'homepage' after 77 ms
Finished 'javascript' after 75 ms
Finished 'css' after 84 ms
Starting 'default'...
Finished 'default' after 14 μs
{% endcodeblock %}
