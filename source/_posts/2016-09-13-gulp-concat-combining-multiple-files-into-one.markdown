---
layout: post
title: "Gulp – Concat: Combining multiple files into one"
date: 2016-09-13 09:35
comments: true
categories: [Gulp]
keywords: Gulp, Gulp – Concat Combining multiple files into one
---

<p>
  <img src="/images/gulp.jpg" width="600" alt="Gulp, Gulp – Concat: Combining multiple files into one" />
</p>

<p>
  Printing Hello and moving files is rather boring. Let’s do something productive.
</p>

<p>
  When we create websites, we are always trying to deliver the best experience possible.
  This includes having our webpages displaying fast. Back in the day, this meant having
  all our styles in one css file.
</p>

<p>
  While this made our webpages load faster, it made maintaining the css file a night-mare!
</p>

<p>
  These days we can use multiple css files for better organization and then concat (meaning merge or combine) the files together into one large file.
</p>

<p>
  We left our project looking like:
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
  Right now, we have two separate css files in our build/styles folder. We are going to use a gulp plugin to concat all our css files in the styles folder.
</p>

<p>
  Gulp contains some basic tasks, but the power of gulp is the customization you can bring into your build process by using plugins.
</p>

<p>
  For a list of all the gulp plugins available, go to <a href="http://gulpjs.com/plugins/" target="_blank">http://gulpjs.com/plugins/</a>
</p>

<p>
  To concat the files together, we will need to install one of these plugins.
</p>

{% codeblock lang:ruby %}
npm install gulp-concat --save-dev
{% endcodeblock %}

<p>
  We can then update our default gulp task to concat the files.
</p>


{% codeblock gulpfile.js lang:ruby %}
var gulp = require('gulp');
var concat = require('gulp-concat');

gulp.task('default', [], function() {
  console.log("Concating and moving all the css files in styles folder");
  gulp.src("contents/styles/**.css")
    .pipe(concat('main.css'))
    .pipe(gulp.dest('build/styles'));
});
{% endcodeblock %}

<p>
  Couple of things have changed, can you spot them? First, we had to reference the gulp plugin with:
</p>

{% codeblock lang:ruby %}
var concat = require('gulp-concat');
{% endcodeblock %}

<p>
  We chose to label this <code>concat</code>. Obviously we could call it anything we want, but concat communicates what the plugin does to those reading our build script.
</p>

<p>
  Second, we added another step to our task. In between the <code>src</code> and the <code>pipe(gulp.dest...)</code> steps, we added <code>pipe(concat(...))</code>.
</p>

<p>
  Gulp works by streaming files from one process to another. This allows us to create complex build tasks out of small, simple steps. Composition == winning.
</p>

<p>
  Now run our gulp task:
</p>

{% codeblock lang:ruby %}
$ gulp
Using gulpfile ~/YOUR_DIRECTORY/gulpfile.js
Starting 'default'...
Moving all the css files in styles folder
Finished 'default' after 6.09 ms
{% endcodeblock %}

<p>
  Our task will read all the css files in the <code>styles</code> folder, combine them into one <code>main.css</code> file, and then place that file in the <code>build/styles</code> folder.
</p>

<p>
  Our project should now look like:
</p>

{% codeblock lang:ruby %}
/build
  /styles
    main.css
    more_styles.css
    some_styles.css
/node_modules
/styles
  more_styles.css
  some_styles.css
gulpfile.js
package.json
{% endcodeblock %}

<p>
  Notice the more_styles.css and some_styles.css files are still in our build folder. :(
</p>

<p>
  We don’t want those chumps there anymore. In the next chapter we will learn how to
  get rid of those files.
</p>

<p>
  So far so good, That’s it!!! See ya!!! :)
</p>