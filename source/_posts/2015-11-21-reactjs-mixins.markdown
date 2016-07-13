---
layout: post
title: "ReactJS Mixins"
date: 2015-11-21 18:10
comments: true
categories: [Javascript, ReactJS]
keywords: ReactJS Mixins
---

<p>
  <img src="/images/reactjs.png" width="600" alt="ReactJS Mixins" />
</p>

<p>
  Components are the best way to reuse code in React, but sometimes very different components may share some common functionality. These are sometimes called cross-cutting concerns. React provides mixins to solve this problem.
</p>

<p>
  Example:
</p>

<p>
  Let's create a simple mixin that uses lifecycle methods for using in two components.
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
    var ReactMixin = {
      getInitialState: function() {
        return {count: 0}
      },
      componentWillMount: function() {
        console.log('will mount');
      },
      incrementCount: function() {
        this.setState({count: this.state.count+1});
      }
    }

    var App = React.createClass({
      render: function() {
        return(
          <div>
            <Button txt="this is the button" />
            <br/>
            <Label txt="this is the label" />
          </div>
        );
      }
    });

    var Button = React.createClass({
      mixins: [ReactMixin],
      render: function() {
        return <button onClick={this.incrementCount}>{this.props.txt} - {this.state.count}</button>
      }
    });

    var Label = React.createClass({
      mixins: [ReactMixin],
      componentWillMount: function() {
        setInterval(this.incrementCount, 1000);
      },
      render: function() {
        return <label>{this.props.txt} - {this.state.count}</label>
      }
    });

    ReactDOM.render(<App />, document.body);
  </script>
</body>
</html>
{% endcodeblock %}

<p>
  So far go good, That's it!!! See ya!!! :)
</p>
