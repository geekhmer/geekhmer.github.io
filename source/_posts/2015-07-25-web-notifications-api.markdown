---
layout: post
title: "Web Notifications API"
date: 2015-07-25 22:12
comments: true
categories: [Javascript]
keywords: Web Notifications API, HTML5 Web Notifications API, Web Notifications, HTML Web Notifications
---

<p>
  <img src="/images/web_notifications api.png" width="400" alt="HTML5 Web Notifications API" />
</p>

<p>
  The Web Notifications API is defined as an API for end-user notifications. A notification allows alerting the user outside the context of a web page of an occurrence, such as the delivery of email.
</p>

<p>
  Thinking of some use cases for such an API isn’t very hard. For example, you may want to receive a notification as soon as you receive an email. You may also want to be notified if someone mentions you in a Tweet, or posts a photo of you on Facebook or Google+.
</p>

<p>
  Now that we know what this API is, and what it’s good for, let’s delve into the description of its methods, properties, and events.
</p>

<p>
  <strong>Methods</strong><br/>
  The Web Notifications API is exposed through the notification property of the window object. This is a constructor that allows us to create a notification instance. It accepts two parameters, a string containing the title of the notification, and an optional object of settings. 
</p>

<p>
  Syntax:
</p>

{% codeblock lang:javascript %}
var notification = new Notification(
  'Email received', 
  {
    body: 'You have a total of 3 unread emails'
  }
);
{% endcodeblock %}

<p>
  <strong>Properties</strong><br/>
  - <code>body</code>: A string used to further specify the purpose of the notification.<br/>
  - <code>lang</code>: Specifies the language of the notification. Its value must be compliant with the <a href="http://tools.ietf.org/html/bcp47" target="_blank">BCP 47 standard</a>. Examples of valid strings are en-US and it-IT.<br/>
  - <code>dir</code>: Defines the direction of the message’s text. Its value can be auto meaning that the direction is based on the browser’s settings, ltr to specify a left-to-right direction (for European languages), or rtl to specify a right-to-left direction (for some Asian languages).<br/>
  - <code>tag</code>: A string that is used as an ID that can be employed to retrieve, replace, or remove the notification.<br/>
  - <code>icon</code>: Specifies the URL of an image that will be used as the notification’s icon.
</p>

<p>
  <strong>Events</strong><br/>
  Sometimes we may need to perform an action as soon as the status of the notification changes. For example, we may want to know if the user has clicked the notification, or when it is closed. To do that, we can attach a handler to one of the four events exposed:<br/>
  - <code>onclick</code>: Fired when the user clicks on the notification.<br/>
  - <code>onclose</code>: Fired as soon as the user or the brower closes the notification.<br/>
  - <code>onerror</code>: Fired if an error occurs with the notification.<br/>
  - <code>onshow</code>: Fired when the notification is shown.
</p>

<p>
  Example:
</p>

{% codeblock lang:javascript %}
var notification = new Notification('Email received', {
  body: 'You have a total of 3 unread emails'
});
 
notification.onshow = function() {
  console.log('Notification shown');
};
{% endcodeblock %}

<p>
  <strong>Requesting Permission</strong><br/>
  Before you display a notification you need to ask the user for permission.
</p>

<p>
  Syntax:
</p>

{% codeblock lang:javascript %}
Notification.requestPermission(function(permission){
  //display notification here
});
{% endcodeblock %}

<p>
  Once the user grants you permission you are good to go. At any point you can check the granted permission using Notifcation.permission property.  It can have one of the following values :<br/>
  - <code>granted</code>: The user has granted the permission.<br/>
  - <code>denied</code>: The user has denied the permission.<br/>
  - <code>default</code>: The status is unknown. The browser will treat this as if the user denied the permission.
</p>

<p>
  <strong>Browser Compatibility</strong><br/>
  Support for the Web Notifications API isn’t very good on both desktop and mobile. On desktop, Chrome and Firefox implemented the first version of this API a while ago. However, considering only the new version of the API, implementation starts from Chrome 22 and Firefox 22 (both without a vendor prefix). Safari 6+ also supports the Web Notifications API. On the mobile side of things, only Firefox and Blackberry have full support. Testing if a browser supports this API is a matter of writing a check like the one shown below:
</p>

{% codeblock lang:javascript %}
if ('Notification' in window) {
  // API supported
} else {
  // API not supported
}
{% endcodeblock %}

<p>
  <strong>Demo</strong><br/>
</p>

{% codeblock lang:javascript %}
<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
    <title>Web Notifications API</title>
  </head>
  <body>
    <button onclick="notifyMe()">Notify me!</button>
    <script type="text/javascript">
      function notifyMe() {
        // Check if the browser supports notifications
        if (!("Notification" in window)) {
          alert("This browser does not support desktop notification");
        }

        // Check whether notification permissions have alredy been granted
        else if (Notification.permission === "granted") {
          // If it's okay let's create a notification
          var notification = new Notification("Hi there!");
        }

        // Otherwise, we need to ask the user for permission
        else if (Notification.permission !== 'denied') {
          Notification.requestPermission(function (permission) {
            // If the user accepts, let's create a notification
            if (permission === "granted") {
              var notification = new Notification("Hi there!");
            }
          });
        }
      }
    </script>
  </body>
</html>
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
