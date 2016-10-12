---
layout: post
title: "Gulp – Handling Errors with Streams"
date: 2016-10-10 22:08
comments: true
categories: [Gulp]
keywords: Gulp, Gulp – Handling Errors with Streams
---

<p>
  <img src="/images/gulpjs.jpg" width="600" alt="Gulp, Gulp – Handling Errors with Streams" />
</p>

<p>
  What if the file was named incorrectly? What happens?
</p>

<p>
  Change the string <code>index.html</code> to <code>OMG_WRONG_FILE.html</code> and rerun the script.
</p>

{% codeblock /streams.js lang:ruby %}
var fs = require("fs");
var stream = fs.createReadStream(__dirname + "/contents/OMG_WRONG_FILE.html");

stream.on("data", function(chunk) {
  // just output chunk to terminal
  console.log(chunk.toString());
});

stream.on("end", function() {
  console.log("END");
});
{% endcodeblock %}

<p>
  Running the script this time results in:
</p>

{% codeblock lang:ruby %}
$ node streams.js

events.js:72
        throw er; // Unhandled 'error' event

Error: ENOENT, open '~/YOUR_DIRECTORY/OMG_WRONG_FILE.html'
{% endcodeblock %}

<p>
  If we read the error message carefully, then we can see that there is an error event we can listen to. So lets listen for that event.
</p>

{% codeblock /streams.js lang:ruby %}
...
stream.on("error", function(er) {
  console.log("error", er);
});
{% endcodeblock %}

<p>
  Now we rerun the script and see:
</p>

{% codeblock lang:ruby %}
$ node streams.js

error { [Error: ENOENT, open '/Users/bunlong/js/gulp/contents/OMG_WRONG_FILE.html']
  errno: -2,
  code: 'ENOENT',
  path: '~/YOUR_DIRECTORY/contents/OMG_WRONG_FILE.html' }
{% endcodeblock %}

<p>
  And that is it for now. We will come back and use some of what we learned later.
</p>
