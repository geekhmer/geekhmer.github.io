---
layout: post
title: "ReactJS JSX"
date: 2015-11-07 10:21
comments: true
categories: [Javascript, ReactJS]
keywords: ReactJS JSX
---

<p>
  <img src="/images/reactjs.png" width="600" alt="ReactJS JSX" />
</p>

<p>
  <strong>What is JSX?</strong><br/>
  <a href="https://facebook.github.io/jsx/" target="_blank">JSX</a> is a JavaScript syntax extension that looks similar to XML. You can use a simple JSX transform with ReactJS.
</p>

<p>
  It's more familiar for casual developers such as designers.
</p>

<p>
  XML has the benefit of balanced opening and closing tags. This helps make large trees easier to read than function calls or object literals.
</p>

<p>
  <strong>Why JSX?</strong><br/>
  You don't have to use JSX with React. You can just use plain JS. However, we recommend using JSX because it is a concise and familiar syntax for defining tree structures with attributes.
</p>

<p>
  <strong>How to use?</strong><br/>
  To start using the JSX library, you need to download a single javascript file from <a href="https://facebook.github.io/react/docs/getting-started.html" target="_blank">Facebook Starter Kit</a> and include in header tag:
</p>

{% codeblock lang:javascript %}
<script src="JSXTransformer.js"></script>
{% endcodeblock %}

<p>
  The JSX library can also be used directly from the <a href="https://cdnjs.com/libraries/react/" target="_blank">Facebook CDN</a> to increase the performance of the page load by including the link below in header tag:
</p>

{% codeblock lang:javascript %}
<script type="text/javascript" src="http://fb.me/JSXTransformer-0.12.2.js"></script>
{% endcodeblock %}

<p>
  Example:
</p>

{% codeblock helloworld.html lang:javascript %}
<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8" />
    <title>Hello World</title>
    <script src="react.js"></script>
    <script src="JSXTransformer.js"></script>
  </head>
  <body>
    <script type="text/javascript">
      var App = React.createClass({
        render: function() {
          return (<h1>Hello World</h1>);
        }
      });

      React.render(<App />, document.body);
    </script>
  </body>
</html>
{% endcodeblock %}

<p>
  <strong>Transform</strong><br/>
  React JSX transforms from an XML-like syntax into native JavaScript. XML elements, attributes and children are transformed into arguments that are passed to <code>React.createElement</code>.
</p>

{% codeblock lang:javascript %}
// Input (JSX):
<App />;

// Output (JS):
React.createElement(App);
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
