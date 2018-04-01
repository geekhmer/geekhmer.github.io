---
layout: post
title: "Gulp – Moving Files"
date: 2016-08-28 18:35
comments: true
categories: [Gulp]
keywords: Gulp, Gulp – Moving Files
---

<p>
  <img src="/images/gulp.jpg" width="600" alt="Gulp – Moving Files" />
</p>

<p>
  The first thing we will learn to do with gulp is to move files.
</p>

<p>
  Let’s create simple styles files some_styles.css and more_styles.css.
</p>

{% codeblock /contents/styles/some_styles.css lang:ruby %}
h1 {
  color: red;
}
{% endcodeblock %}

{% codeblock /contents/styles/more_styles.css lang:ruby %}
p {
  font-size: 30px;
}
{% endcodeblock %}

<p>
  Our project structure should now look like:
</p>

{% codeblock lang:ruby %}
/node_modules
/contents
  /styles
    more_styles.css
    some_styles.css
gulpfile.js
package.json
{% endcodeblock %}

<p>
  Update our <code>gulpfile.js</code> from the previous section and instruct gulp to move all the files found in the styles folder to our <code>build/styles</code> folder.
</p>

{% codeblock gulpfile.js lang:ruby %}
var gulp = require('gulp');

gulp.task('default', [], function() {
  console.log("Moving all files in styles folder");
  gulp.src("contents/styles/**.*")
    .pipe(gulp.dest('build/styles'));
});
{% endcodeblock %}

<p>
  Well, What do we expect will happen when we run gulp? If you guessed the files will be copied and moved to the <code>build/styles</code> folder, then give yourself a cookie.
</p>

<p>
  When we run gulp , we should see:
</p>

{% codeblock lang:ruby %}
$ gulp

Using gulpfile ~/YOUR_DIRECTORY/gulpfile.js
Starting 'default'...
Moving all files in styles folder
Finished 'default' after 7.27 ms
{% endcodeblock %}

<p>
  Our project should now look like:
</p>

{% codeblock lang:ruby %}
/build
  /styles
    some_styles.css
    more_styles.css
/node_modules
/contents
  /styles
    some_styles.css
    more_styles.css
gulpfile.js
package.json
{% endcodeblock %}

<p>
  So far so good, That’s it!!! See ya!!! :)
</p>
