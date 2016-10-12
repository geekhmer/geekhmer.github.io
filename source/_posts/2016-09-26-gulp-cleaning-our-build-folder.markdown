---
layout: post
title: "Gulp – Cleaning Our Build Folder"
date: 2016-09-26 16:56
categories: [Gulp]
keywords: Gulp, Gulp – Cleaning Our Build Folder
---

<p>
  <img src="/images/gulpjs.jpg" width="600" alt="Gulp, Gulp – Cleaning Our Build Folder" />
</p>

<p>
  A normal part of a build process is a cleaning task to remove all the old files in the build folder.
</p>

<p>
For us, this means getting rid of the leftover <code>more_styles.css</code> and <code>some_styles.css</code> files in our <code>build/styles</code> folder.
</p>

<p>
  To clean files, we will need another gulp plugin:
</p>

{% codeblock lang:ruby %}
$ npm install gulp-rimraf --save-dev
{% endcodeblock %}

<p>
For more information on gulp-rimraf check out <a href="https://www.npmjs.com/package/gulp-rimraf" target="_blank"> https://www.npmjs.org/package/gulp-rimraf</a>.
</p>

<p>
  This task used to be handled by <code>gulp-clean</code> but has been replaced by <code>gulp-rimraf</code>.
</p>

<p>
Instead of adjusting our <code>default</code> task, lets create a new task to clean out the directory.
</p>

{% codeblock gulpfile.js lang:ruby %}
var gulp = require('gulp');
var concat = require('gulp-concat');
var clean = require('gulp-rimraf');

gulp.task('clean', [], function() {
  console.log("Clean all files in build folder");
  return gulp.src("build/*", { read: false }).pipe(clean());
});

gulp.task('default', ['clean'], function() {
  console.log("Concating and moving all the css files in styles folder");
  return gulp.src("contents/styles/**.css")
    .pipe(concat('main.css'))
    .pipe(gulp.dest('build/styles'));
});
{% endcodeblock %}

<p>
  So like before, we need to require <code>gulp-rimraf</code>.
</p>

<p>
  This time though, we created a new task called <code>clean</code>. We tell this task to look at all the files in the <code>build</code> folder and then pipe them to our clean operation. This will delete the files.
</p>

<p>
  You might notice that in our options we pass in <code>{ read: false }</code>. This tells the task to not read the contents of the files it is deleting. It is an easy performance gain.
</p>

<p>
  To run our clean task from the command line, we just tell gulp which task to run:
</p>

{% codeblock lang:ruby %}
$ gulp clean

Using gulpfile ~/YOUR_DIRECTORY/gulpfile.js
Starting 'clean'...
Clean all files in build folder
Finished 'clean' after 8.95 ms
{% endcodeblock %}

<p>
  What we would like is to run our <code>clean</code> task before we run our <code>default</code> task. That way our build folder will be nice and empty before we starting moving files there.
</p>

<p>
You might have been wondering what the empty array (<code>[]</code>) was before our <code>function</code>. This is where we specify dependency tasks.
</p>

<p>
  A <strong>dependency task</strong> is a task that needs to be completed before gulp can run the current task.
</p>

<p>
  So for our scenario, our <code>clean</code> task is a dependency for <code>default</code>.
</p>

{% codeblock gulpfile.js lang:ruby %}
...
gulp.task('default', ['clean'], function() {
  ...
});
{% endcodeblock %}

<p>
  Now when we run our <code>default</code> gulp task, we should see that it runs the <code>clean</code> task before <code>default</code>.
</p>

{% codeblock lang:ruby %}
$ gulp

Using gulpfile ~/YOUR_DIRECTORY/gulpfile.js
Starting 'clean'...
Clean all files in build folder
Finished 'clean' after 9.03 ms
Starting 'default'...
Concating and moving all files from styles folder
Finished 'default' after 8.42 ms
{% endcodeblock %}

<p>
  So far so good, That’s it!!! See ya!!! :)
</p>
