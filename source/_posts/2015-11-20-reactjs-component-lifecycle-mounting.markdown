---
layout: post
title: "ReactJS Component Lifecycle Mounting"
date: 2015-11-20 16:59
comments: true
categories: [Javascript, ReactJS]
keywords: ReactJS Component Lifecycle
---

<p>
  <img src="/images/reactjs.png" width="600" alt="ReactJS Component Lifecycle Mounting" />
</p>

<p>
  ReactJS mounting have 3 methods:<br/>
  - <code>getInitialState()</code>: object is invoked before a component is mounted. Stateful components should implement this and return the initial state data.<br/>
  - <code>componentWillMount()</code>: is invoked immediately before mounting occurs.<br/>
  - <code>componentDidMount()</code>: is invoked immediately after mounting occurs. Initialization that requires DOM nodes should go here.
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
    var Button = React.createClass({
      getInitialState: function() {
        return {
          val: 0
        }
      },
      handleUpdate: function() {
        this.setState({val: this.state.val + 1}
        );
      },
      componentWillMount: function() {
        console.log('mounting');
      },
      render: function() {
        console.log('rendering');
        return (
          <div>
            <button onClick={this.handleUpdate}>{this.state.val}</button>
          </div>
        );
      },
      componentDidMount: function() {
        console.log('mounted');
      }
    });

    var App = React.createClass({
      handleMount: function() {
        React.render(<Button />, document.getElementById('app'));
      },
      render: function() {
        return(
          <div>
            <button onClick={this.handleMount}>Mount</button>
            <button onClick={this.handleUnmount}>Unmount</button>
            <div id="app"></div>
          </div>
        );
      }
    });

    ReactDOM.render(<App />, document.body);
  </script>
</body>
</html>
{% endcodeblock %}

<p>
  In Button component <code>getInitialState()</code> method initial <code>val: 0</code> by default.
</p>

<p>
  When you click on Mount button, <code>componentWillMount()</code> method is invoked immediately before mounting occurs, and <code>render()</code> method render button into body DOM, and then <code>componentDidMount()</code> method is invoked immediately after mounting occurs.
</p>

<p>
  So far go good, That's it!!! See ya!!! :)
</p>
