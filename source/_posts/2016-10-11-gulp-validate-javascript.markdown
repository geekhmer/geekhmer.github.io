---
layout: post
title: "Gulp – Validate JavaScript"
date: 2016-10-11 11:42
comments: true
categories: [Gulp]
keywords: Gulp, Gulp – Validate JavaScript
---

<p>
  <img src="/images/bunlong_gulp.jpg" width="600" alt="Gulp, Gulp – Validate JavaScript" />
</p>

<p>
  The web runs on more than HTML and CSS, It runs on JavaScript. Let’s perform some common JavaScript build operations with gulp.
</p>

<p>
  We will first look into validating our JavaScript using gulp.
</p>

<p>
  And yes... there is a plugin for that.
</p>

{% codeblock lang:ruby %}
$ npm install gulp-jsvalidate --save-dev
{% endcodeblock %}

<p>
  For more information on <code>gulp-jsvalidate</code> check out <a href="https://github.com/sindresorhus/gulp-jsvalidate" target="_blank">https://github.com/sindresorhus/gulp-jsvalidate</a>.
</p>

<p>
  We will now create a new Javascript task in our <code>gulpfile.js</code>. At first, all we will do is validate our Javascript in a new <code>/contents/javascripts</code> folder.
</p>

{% codeblock /gulpfile.js lang:ruby %}
...
var jsValidate = require('gulp-jsvalidate');
...

...
gulp.task('javascript', function () {
  console.log("Validate JavaScript");
  return gulp.src("contents/javascripts/**.js")
    .pipe(jsValidate());
});
...
{% endcodeblock %}

<p>
  Time to test out our new task and plugin. Create a javascript file and make a syntax error.
</p>

{% codeblock /contents/javascript/somejs.js lang:ruby %}
function OMG() {
  var x * 2; // this is not valid!
  return x + 10;
}
{% endcodeblock %}

<p>
  Now when we run our javascript task, we will get an error message in the terminator:
</p>

{% codeblock lang:ruby %}
$ gulp javascript

Using gulpfile ~/YOUR_DIRECTORY/gulpfile.js
Starting 'javascript'...
'javascript' errored after 14 ms Line 3: Unexpected token *
{% endcodeblock %}

<p>
  When we fix the error and then run our gulp task, we won’t get that error message:
</p>

{% codeblock /contents/javascript/somejs.js lang:ruby %}
function OMG() {
  var x = 2;
  return x + 10;
}
{% endcodeblock %}

{% codeblock lang:ruby %}
$ gulp javascript

Using gulpfile ~/YOUR_DIRECTORY/gulpfile.js
Starting 'javascript'...
Validate JavaScript
Finished 'javascript' after 14 ms
{% endcodeblock %}

<p>
  Sweet codes! Gulp can now check if our JavaScript is valid. But the error message in the console is rather bland, lets find a better way to tell us that we messed up.
</p>
