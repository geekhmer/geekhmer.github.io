---
layout: post
title: "Gulp – Testing with Jasmine"
date: 2016-10-12 09:07
comments: true
categories: [Gulp]
keywords: Gulp, Gulp – Testing with Jasmine
---

<p>
  <img src="/images/bunlong_gulp.jpg" width="600" alt="Gulp, Gulp – Testing with Jasmine" />
</p>

<p>
  You do test your JavaScript right? Well... you should and with gulp + Karma + Jasmine it is super easy.
</p>

<p>
  First if you have not installed Karma and Jasmine, then do so now.
</p>

{% codeblock lang:ruby %}
$ npm install karma-jasmine --save-dev
{% endcodeblock %}

<p>
  Next we will install the <code>gulp-jasmine</code> plugin for gulp.
</p>

{% codeblock lang:ruby %}
$ npm install gulp-jasmine --save-dev
{% endcodeblock %}

<p>
  We can then create a test task to run all the specs found in a specs folder we will create.
</p>

{% codeblock gulpfile.js lang:ruby %}
var jasmine = require('gulp-jasmine');
...
gulp.task('specs', function () {
  return gulp.src('specs/**.js')
    .pipe(jasmine());
});
{% endcodeblock %}

<p>
  Let’s create a basic (failing) test to see that it is working.
</p>

{% codeblock /specs/our_test.js lang:ruby %}
describe('OMG a JavaScript Test', function() {
  it('should pass', function() {
    expect(true).toBe(false);
  });
});
{% endcodeblock %}

{% codeblock lang:ruby %}
$ gulp specs

Using gulpfile ~/YOUR_DIRECTORY/gulpfile.js
Starting 'specs'...
F

Failures:

  1) OMG a JavaScript Test should pass
  1.1) Expected true to be false.

1 spec, 1 failure
Finished in 0.004 seconds
'specs' errored after 39 ms Tests failed

{% endcodeblock %}

<p>
  Now it is time to refactor. We will make the extremely difficult change from false to true to make our test pass.
</p>

{% codeblock /specs/our_test.js lang:ruby %}
describe('OMG a JavaScript Test', function() {
  it('should pass', function() {
    expect(true).toBe(true);
  });
});
{% endcodeblock %}

<p>
  And run our test suite again.
</p>

{% codeblock lang:ruby %}
$ gulp specs

Using gulpfile ~/YOUR_DIRECTORY/gulpfile.js
Starting 'specs'...
.

1 spec, 0 failures
Finished in 0 seconds
Finished 'specs' after 39 ms
{% endcodeblock %}

<p>
  Next we will do is improve our testing task by adding a test-watch task to run as we edit our JavaScript files.
</p>

{% codeblock gulpfile.js lang:ruby %}
...
gulp.task('spec-watch', function() {
  gulp.watch(['specs/**.js', 'contents/javascripts/**.js'], ['test'])
});
{% endcodeblock %}

