---
layout: post
title: "ruby websocket and real-time app tutorial"
date: 2014-02-04 21:12
comments: true
categories: [Websocket, Ruby]
keywords: websocket, ruby websocket, ruby websocket tutorial, ruby websocket real time app
---

<p>
  In this post I would like to present a small tutorial, I hope it will serve as a good introduction to web-socket api.
</p>

<p>
  <strong>Websocket Server</strong><br/>
  In the part we focus on server part. In ruby we use eventmachine, em-websocket gem to install websocket server.<br/>
  To install eventmachine run <code>gem install eventmachine</code><br/>
  To install em-websocket run <code>gem install em-websocket</code><br/>
  Make a file server.rb and implement codes below:<br/>
</p>

{% codeblock server.rb lang:ruby %}
require 'eventmachine'
require 'em-websocket'
 
EventMachine.run {
  EventMachine::WebSocket.start(:host => "0.0.0.0", :port => 8080) do |ws|
    ws.onopen {
      puts "WebSocket connection open"
    }

    ws.onmessage { |msg|
      puts msg
      ws.send(msg)
    }

    ws.onclose {
      puts "WebSocket connection closed"
    }
  end
}
{% endcodeblock %}

<p>
  What the code does is creates a websocket-server which listens at localhost:8080. Callbacks have been provided for open and close events, so when a client creates a connection or a connection gets closed the associated callbacks print an appropriate message to the terminal. And callbacks have been provided for message event when a client send the message.<br/><br/>
  To run websocket server run <code>ruby sever.rb</code>
</p>

<p>
  <strong>Websocket Client</strong><br/>
  So far, so good. But the main purpose of a websocket server is to get the message from client and relay data to the client. How do we do that? Turns out that is pretty simple too.<br/>
  Make a file index.html and implement codes below:<br/>
</p>

{% codeblock index.html lang:html %}
<html>
<head>
  <title>Websocket GeeKhmer</title>
  <script type="text/javascript">
    var ws = null;
    function init() {
      ws = new WebSocket("ws://localhost:8080");
      ws.onopen = function() {
        console.log("Connection is opened");
      }

      ws.onclose = function() {
        console.log("Connection is closed");
      }

      ws.onmessage = function(msg) {
        document.getElementById("display").innerHTML = msg.data;
      }
    }

    function send() {
      ws.send(document.getElementById("txt").value);
    }
  </script>
</head>
<body onload="init();">
  <h2>WebSocket GeeKhmer</h2>
  <input type="text" id="txt">
  <input type="button" onclick="send();" value="Send">
  <p id="display"></p>
</body>
</html>
{% endcodeblock %}
<p>
  Feel free to provide your suggestions and to point out errors.
</p>