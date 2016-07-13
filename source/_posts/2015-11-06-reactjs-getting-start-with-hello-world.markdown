---
layout: post
title: "ReactJS Getting Start with Hello World"
date: 2015-11-06 23:03
comments: true
categories: [Javascript, ReactJS]
keywords: ReactJS Getting Start with Hello World, ReactJS Getting Start
---

<p>
  <img src="/images/reactjs.png" width="600" alt="ReactJS Getting Start with Hello World" />
</p>

<p>
  <a href="https://facebook.github.io/react/" target="_blank">React</a> is built around the concept of components. This is in contrast to frameworks like Angular and Ember, which use two-way data bindings to update the HTML of the page. In my opinion React is easier to learn than Angular and Ember – it is much smaller and plays nicely with jQuery and other frameworks. It is also extremely fast, because it uses a virtual DOM, and syncs only the changed parts with the underlying page.
</p>

<p>
  <strong>How to use?</strong><br/>
  To start using the ReactJS library, you need to download a single javascript file from <a href="https://facebook.github.io/react/docs/getting-started.html" target="_blank">Facebook Starter Kit</a> and include in header tag:
</p>

{% codeblock lang:javascript %}
<script src="react.js"></script>
{% endcodeblock %}

<p>
  The ReactJS library can also be used directly from the <a href="https://cdnjs.com/libraries/react/" target="_blank">Facebook CDN</a> to increase the performance of the page load by including the link below in header tag:
</p>

{% codeblock lang:javascript %}
<script src="https://cdnjs.cloudflare.com/ajax/libs/react/0.14.2/react.js"></script>
{% endcodeblock %}

<p>
  <strong>Hello World</strong><br/>
  Create a helloworld.html file with the following contents:
</p>

{% codeblock helloworld.html lang:javascript %}
<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8" />
    <title>Hello World</title>
    <script src="react.js"></script>
  </head>
  <body>
    <script type="text/javascript">
      var App = React.createClass({
        render: function() {
          return React.createElement("h1", null, "Hello World")
        }
      });

      React.render(React.createElement(App), document.body);
    </script>
  </body>
</html>
{% endcodeblock %}

<p>
  The primary type in React is the ReactElement. It has four properties: type, props, key   and ref. It has no methods and nothing on the prototype. We can create a element through <code>React.createElement</code>.
</p>

<p>
  To render a new tree into the DOM, we create <code>ReactElements</code> and pass them to <code>React.render</code> along with a regular DOM Element.
</p>

<p>
  In codes above we create a <code>h1 element</code> in <code>App component</code> and render it in body DOM.
</p>

<p>
  <strong>Hello World - Separate File</strong><br/>
  Create a helloworld.js file with the following contents:
</p>

{% codeblock helloworld.js lang:javascript %}
var App = React.createClass({
  render: function() {
    return React.createElement("h1", null, "Hello World")
  }
});

React.render(React.createElement(App), document.body);
{% endcodeblock %}

<p>
  And update HTML file as below:
</p>

{% codeblock helloworld.html lang:javascript %}
<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8" />
    <title>Hello World</title>
    <script src="react.js"></script>
  </head>
  <body>
    <script src="helloworld.js"></script>
  </body>
</html>
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
