---
layout: post
title: "ReactJS Ownership"
date: 2015-11-18 17:36
comments: true
categories: [Javascript, ReactJS]
keywords: ReactJS Ownership
---

<p>
  <img src="/images/reactjs.png" width="600" alt="ReactJS Ownership" />
</p>

<p>
  In React, an owner is the component that sets the props of other components. More formally, if a component X is created in component Y's render() method, it is said that X is owned by Y.
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
      getInitialState: function() {
        return {
          txt: ''
        }
      },
      update: function(e) {
        this.setState({txt: e.target.value});
      },
      render: function() {
        return (
          <div>
            <Widget txt={this.state.txt} update={this.update} />
            <Widget txt={this.state.txt} update={this.update} />
          </div>
        );
      }
    });

    var Widget = React.createClass({
      render: function() {
        return (
          <div>
            <input type="text" onChange={this.props.update} />
            <br />
            <b>{this.props.txt}</b>
          </div>
        );
      }
    });

    React.render(<App txt="this is the txt prop" />, document.body);
  </script>
</body>
</html>
{% endcodeblock %}

<p>
  The example above Widget component is owned by App component.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
