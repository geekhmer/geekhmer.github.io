---
layout: post
title: "Gulp – Uglify"
date: 2016-10-11 22:02
comments: true
categories: [Gulp]
keywords: Gulp, Gulp – Uglify
---

<p>
  <img src="/images/bunlong_gulp.jpg" width="600" alt="Gulp, Gulp – Uglify" />
</p>

<p>
  For JavaScript files we also want to uglify them. Uglifying JavaScript involves changing variable and function names to reduce their size. So a variable named customer might be renamed to x. JavaScript engines don’t care about descriptive names, only developers. So how do we uglify JavaScript files with gulp?
</p>

<p>
  I know what you are going to say: "Blah, blah, blah... there is a plugin." and you are correct.
</p>

{% codeblock lang:ruby %}
$ npm install gulp-uglify --save-dev
{% endcodeblock %}

<p>
  For more information on <code>gulp-uglify</code> check out <a href="https://www.npmjs.org/package/gulp-uglify" target="_blank">https://www.npmjs.org/package/gulp-uglify</a>.
</p>

<p>
  While we are uglifying the file, we will also concat all our JavaScript files together and move them to <code>build/javascripts</code>.
</p>

{% codeblock gulpfile.js lang:ruby %}
...
var uglify = require('gulp-uglify');
...

gulp.task('javascript', function () {
  console.log("Validate, Concat, Uglify, and Move all the javascript files");

  return gulp.src("contents/javascripts/**.js")
    .pipe(jsValidate())
    .on("error", notify.onError(function(error) {
      return error.message;
    }))
    .pipe(uglify())
    .pipe(concat('main.js'))
    .pipe(gulp.dest('build/javascripts'));
});
{% endcodeblock %}

<p>
  When you run our gulp javascript task now, we should see that our javascript files were uglified, concated, and moved to the build folder.
</p>

{% codeblock lang:ruby %}
$ gulp javascript

Using gulpfile ~/YOUR_DIRECTORY/gulpFile.js
Starting 'javascript'...
Validate, Concat, Uglify, and Move all the javascript files
Finished 'javascript' after 55 ms
{% endcodeblock %}

<p>
  If you have an error here, be sure to check that your JavaScript is valid. Remember we were testing that last section.
</p>

<p>
  The build script should create our <code>/build/javascripts/main.js</code> file.
</p>

{% codeblock /build/javascripts/main.js lang:ruby %}
function OMG(){var n=2;return n+10}
{% endcodeblock %}
