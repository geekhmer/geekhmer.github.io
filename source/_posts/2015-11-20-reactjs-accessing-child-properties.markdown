---
layout: post
title: "ReactJS Accessing Child Properties"
date: 2015-11-20 13:18
comments: true
categories: [Javascript, ReactJS]
keywords: ReactJS Accessing Child Properties
---

<p>
  <img src="/images/reactjs.png" width="600" alt="ReactJS Accessing Child Properties" />
</p>

<p>
  When you are building your React components, you'll probably want to access child properties of the markup.
</p>

<p>
  Parent can read its children by accessing the special <code>this.props.children</code>
</p>

<p>
  Example:
</p>

{% codeblock lang:javascript %}
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title></title>
  <script type="text/javascript" src="http://fb.me/react-0.12.2.js"></script>
  <script type="text/javascript" src="http://fb.me/JSXTransformer-0.12.2.js"></script>
</head>
<body>
  <script type="text/jsx">
    var App = React.createClass({
      render: function() {
        return (
          <Button>React <Heart /></Button>
        );
      }
    });

    var Button = React.createClass({
      render: function() {
        return(
          <button>{this.props.children}</button>
        );
      }
    });

    var Heart = React.createClass({
      render: function() {
        return (
          <span>Heart</span>
        );
      }
    });

    ReactDOM.render(<App />, document.body);
  </script>
</body>
</html>
{% endcodeblock %}

<p>
  App has two children Button and Heart, all thoes children come thought from <code>{this.props.children}</code>.
</p>

<p>
  So far go good, That's it!!! See ya!!! :)
</p>
