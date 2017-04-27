---
layout: post
title: "Gulp – Markdown"
date: 2016-10-30 11:22
comments: true
categories: [Gulp]
keywords: Gulp, Gulp – Markdown
---

<p>
  <img src="/images/bunlong_gulp.jpg" width="600" alt="Gulp, Gulp – Markdown" />
</p>

<p>
  Now for someting new. We are going to use Gulp with Handlebars to create our own CMS system.
</p>

<p>
  First of all, We want to be able to process markdown files and create html files with Gulp with plugin.
</p>

{% codeblock lang:ruby %}
$ npm install gulp-markdown --save-dev
{% endcodeblock %}

<p>
  For more information about <code>gulp-markdown</code> check out <a href="https://www.npmjs.org/package/gulp-markdown" target="_blank">https://www.npmjs.org/package/gulp-markdown</a>
</p>

<p>
  We will read all the markdown files in the <code>contents/pages</code> folder and generate html files.
</p>

{% codeblock gulpfile.js lang:ruby %}
...
var markdown = require('gulp-markdown');
...
gulp.task('generate_pages', function() {
  return gulp.src('content/pages/**.md')
    .pipe(markdown())
    .pipe(gulp.dest("build/pages"));
});
{% endcodeblock %}

<p>
  Lets create our first page.
</p>

{% codeblock ontents/pages/first_page.md lang:ruby %}
Yes, it makes a **bold** statement.
{% endcodeblock %}

<p>
  When We run our <code>gulp generate_pages</code> task, We will take the markdown and convert it into html and place the files in the <code>build/pages</code> directory.
</p>

{% codeblock lang:ruby %}
$ gulp generate_pages
Using gulpfile ~/js/gulpwalkthru/gulpfile.js
Starting 'generate_pages'...
Finished 'generate_pages' after 22 ms
{% endcodeblock %}

<p>
  If We look in our <code>build/pages</code> directory, We should see our new html file.
</p>

{% codeblock build/pages/first_page.html lang:ruby %}
p>Yes, it makes a <strong>bold</strong> statement.</p>
{% endcodeblock %}

<p>
  If We visit http://localhost:8000/pages/first_page.html we should see our generated webpage.
</p>
