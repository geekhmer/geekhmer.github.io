---
layout: post
title: "ReactJS State"
date: 2015-11-14 11:41
comments: true
categories: [Javascript, ReactJS]
keywords: ReactJS State
---

<p>
  <img src="/images/reactjs.png" width="600" alt="ReactJS State" />
</p>

<h3>What is State?</h3>

<p>
  The state object is internal to a component. It holds data which can change over time. So far, we've used ReactJS as a static rendering engine. Now, we're going to add state to make React components more dynamic.
</p>

<p>
  The key difference between props and state is that state is internal and controlled by the component itself while props are external and controlled by whatever renders the component. Let's see it in practice:
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
    var Profile = React.createClass({
      getInitialState: function() {
        return {
          name: 'Bunlong'
        };
      },
      onNameInput: function(e) {
        this.setState({name: e.target.value});
      },
      render: function() {
        return (
          <div>
            <input type="text" onChange={this.onNameInput} />
            <h1>{this.state.name}</h1>
          </div>
        );
      }
    });

    React.render(<Profile txt="Bunlong VAN" />, document.body);
  </script>
</body>
</html>
{% endcodeblock %}

<h3>State API</h3>

<p>
  <strong>Getinitial State</strong><br/>
  Implement the function <code>getInitialState</code> which returns the initial state of the component. This is an object map of keys to values.
</p>

{% codeblock lang:javascript %}
getInitialState: function() {
  return {
    name: "Bunlong"
  };
}
{% endcodeblock %}

<p>
  <strong>this.state</strong><br/>
  To access a component's state use <code>this.state</code> just like how we use <code>this.props.</code>
</p>

<p>
  <strong>this.setState</strong><br/>
  To update a component's state, call <code>this.setState</code> with an object map of keys to updated values. Keys that are not provided are not affected.
</p>

{% codeblock lang:javascript %}
this.setState({
  name: "Sokleap"
})
{% endcodeblock %}

<p>
  When a component's state changes, <code>render</code> is called with the new state and the UI is updated to the new output. This is the heart of React.
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
    var Counter = React.createClass({
      getInitialState: function() {
        return {count: 0};
      },
      render: function() {
        return (
          <div>
            <h1>{this.state.count}</h1>
            <button type="button" onClick={this.increase}>Increase</button>
          </div>
        );
      },
      increase: function() {
        this.setState({count: this.state.count + 1});
      }
    });
    React.render(<Counter />, document.body);
  </script>
</body>
</html>
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
