---
layout: post
title: "Gulp – Streams"
date: 2016-10-04 09:18
comments: true
categories: [Gulp]
keywords: Gulp, Gulp – Streams
---

<p>
  <img src="/images/gulpjs.jpg" width="600" alt="Gulp, Gulp – Minify Our CSS" />
</p>

<p>
  Before we continue, I think a brief detour to cover some basics of Node streams would be helpful.
</p>

<p>
  Lets create a simple Node script to read a <code>index.html</code> file we will create:
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
  First we require the file system library <code>fs</code> and create a read stream.
</p>

{% codeblock /streams.js lang:ruby %}
var fs = require("fs");
var stream = fs.createReadStream(__dirname + "/contents/index.html");
{% endcodeblock %}

<p>
  <code>_dirname</code> is a helper that returns the absolute path of the code file being run.
</p>

<p>
  Node reads files asynchronously. This is normally where we could dive into what "non-blocking I/O" means vs threads, etc. This is a guide about gulp though so I will keep this detour basic.
</p>

<p>
  For our purposes, this means that we have to listen to <strong>events</strong> from the stream to be able to read the file.
</p>

<p>
  The events we are going to listen to are <code>data</code> and <code>end</code>.
</p>

<p>
  <code>data</code> fires when a chunk of the file has been read and returned. <strong>This chunk is not always the entire file</strong>. In fact, you should assume it is not the entire file.
</p>

{% codeblock /streams.js lang:ruby %}
...
stream.on("data", function(chunk) {
  // just output chunk to terminal
  console.log(chunk.toString());
});
{% endcodeblock %}

<p>
  <code>end</code> fires when the file has been completly read.
</p>

{% codeblock /streams.js lang:ruby %}
...
stream.on("data", function(chunk) {
  // just output chunk to terminal
  console.log(chunk.toString());
});
{% endcodeblock %}

<p>
  Now altogether, <code>streams.js</code> looks like:
</p>

{% codeblock /streams.js lang:ruby %}
var fs = require("fs");
var stream = fs.createReadStream(__dirname + "/contents/index.html");

stream.on("data", function(chunk) {
  console.log(chunk.toString());
});

stream.on("end", function() {
  console.log("END");
});
{% endcodeblock %}

<p>
  Now if you run the node script in the terminal, you should see:
</p>

{% codeblock lang:ruby %}
$ node streams.js

<!DOCTYPE html>
<html>
<head>
  <title>Learning Gulp</title>
</head>
<body>
  <h1>Hello Gulp!</h1>
</body>
</html>
END
{% endcodeblock %}

<p>
  So far so good, That’s it!!! See ya!!! :)
</p>
