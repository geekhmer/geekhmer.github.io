---
layout: post
title: "Tokeninput Jquery Multiple Select Item"
date: 2014-05-22 16:46
comments: true
categories: Javascript
keywords: javascript, jquery, tokeninput jquery multiple select item
---

<p>
  <strong>What is Tokeninput?</strong><br/>
  Tokeninput is a jQuery plugin which allows your users to select multiple items from a predefined list, using autocompletion as they type to find each item. You may have seen a similar type of text entry when filling in the recipients field sending messages on facebook.
</p>

<p>
  <a class="fancybox" href="/images/tokeninput_jquery.png"><img src="/images/tokeninput_jquery.png" alt="" width="680" /></a>
</p>

<p>
  <strong>Practice</strong><br/>
</p>

{% codeblock demo.html lang:javascript %}
<html>
<head>
  <script type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js"></script>
  <script type="text/javascript" src="src/jquery.tokeninput.js"></script>
  <link rel="stylesheet" href="styles/token-input.css" type="text/css" />
  <link rel="stylesheet" href="styles/token-input-facebook.css" type="text/css" />
</head>
<body>
  <h2 id="theme">Json local</h2>
  <div>
    <input type="text" id="input-local" />
    <script type="text/javascript">
      $(document).ready(function() {
        $("#input-local").tokenInput([
            {"id":"856","name":"House"},
            {"id":"1035","name":"Desperate Housewives"}
          ], {
          theme: "facebook"
        });
      });
    </script>
  </div>

  <h2 id="theme">Json local - no duplicates</h2>
  <div>
    <input type="text" id="input-local-prevent-duplicates" />
    <script type="text/javascript">
      $(document).ready(function() {
        $("#input-local-prevent-duplicates").tokenInput([
            {"id":"856","name":"House"},
            {"id":"1035","name":"Desperate Housewives"}
          ], {
          theme: "facebook",
          preventDuplicates: true
        });
      });
    </script>
  </div>

  <h2 id="theme">Json server</h2>
  <div>
    <input type="text" id="input-server" />
    <script type="text/javascript">
      $(document).ready(function() {
        $("#input-server").tokenInput("http://shell.loopj.com/tokeninput/tvshows.php", {
          theme: "facebook"
        });
      });
    </script>
  </div>

  <h2 id="prevent-duplicates">Json server - no duplicates</h2>
  <div>
    <input type="text" id="input-server-prevent-duplicates" />
    <script type="text/javascript">
      $(document).ready(function() {
        $("#input-server-prevent-duplicates").tokenInput("http://shell.loopj.com/tokeninput/tvshows.php", {
          theme: "facebook",
          preventDuplicates: true
        });
      });
    </script>
  </div>
</body>
</html>
{% endcodeblock %}

<p>
  You can download the <a href="https://github.com/Bunlong/tokeninput_jquery" target="_blank">source code</a> and try it out.
</p>