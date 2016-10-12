---
layout: post
title: "Gulp – Notify Pop Up"
date: 2016-10-11 21:11
comments: true
categories: [Gulp]
keywords: Gulp, Gulp – Notify Pop Up
---

<p>
  <img src="/images/bunlong_gulp.jpg" width="600" alt="Gulp, Gulp – Notify Pop Up" />
</p>

<p>
  In the previous article, we used gulp to validate our JavaScript. The error message would appear in the console. While this is awesome, there is a chance we could miss it.
</p>

<p>
  Let’s use notifications to display a pop up window when we have a JavaScript error.
</p>

<p>
  There is a gulp plugin to send notifications.
</p>

{% codeblock lang:ruby %}
$ npm install gulp-notify --save-dev
{% endcodeblock %}

<p>
  For more information on <code>gulp-notify</code> check out <a href="https://www.npmjs.org/package/gulp-notify" target="_blank">https://www.npmjs.org/package/gulp-notify</a>
</p>

<p>
  Remember that gulp uses node’s streaming. It shouldn’t be a surprise that when <code>gulp-jsvalidate</code> finds an error, it emits an error event.
</p>

<p>
  All we need to do is handle the event and use <code>gulp-notify</code> to send a notification with the error message.
</p>

{% codeblock gulpfile.js lang:ruby %}
...
var notify = require('gulp-notify');
...

...
gulp.task('javascript', function () {
  console.log("Validate JavaScript");
  return gulp.src("contents/javascripts/**.js")
    .pipe(jsValidate())
    .on("error", notify.onError(function(error) {
      return error.message;
    }));
});
...
{% endcodeblock %}

<p>
  Since our JavaScript is now valid, we need to make it invalid so we can see the error message.
</p>

{% codeblock /contests/javascript/somejs.js lang:ruby %}
function OMG() {
  var x * 2;
  return x + 10;
}
{% endcodeblock %}

<p>
  Now when we run gulp javascript we will get a notification window that an error was found.
</p>

{% codeblock lang:ruby %}
$ gulp javascript

Using gulpfile ~/YOUR_DIRECTORY/gulpFile.js
Starting 'javascript'...
Validate JavaScript
gulp-notify: [Error running Gulp] Line 3: Unexpected token *
'javascript' errored after 41 ms Line 3: Unexpected token *
{% endcodeblock %}

