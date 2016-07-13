---
layout: post
title: "ReactJS Refs"
date: 2015-11-19 16:47
comments: true
categories: [Javascript, ReactJS]
keywords: ReactJS Refs
---

<p>
  <img src="/images/reactjs.png" width="600" alt="ReactJS Refs" />
</p>

<p>
  ReactJS provides a mechanism for referencing the actual component instance by using the <code>refs</code> property:
</p>

{% codeblock lang:javascript %}
var App = React.createClass({
  handleClick: function () {
    console.log(this.refs.input.getDOMNode().value);
  },
  render: function () {
    return (
      <div>
        <input ref="input" />
        <button onClick={this.handleClick}>Submit</button>
      </div>
    );
  }
});
{% endcodeblock %}

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
          red: 0,
          green: 0,
          blue: 0
        }
      },
      update: function(e) {
        this.setState({
          red: this.refs.red.refs.inp.getDOMNode().value,
          green: this.refs.green.refs.inp.getDOMNode().value,
          blue: this.refs.blue.refs.inp.getDOMNode().value
        });
      },
      render: function() {
        return (
          <div>
            <Slider ref="red" txt={this.state.txt} update={this.update} />
            <label>{this.state.red}</label>
            <Slider ref="green" txt={this.state.txt} update={this.update} />
            <label>{this.state.green}</label>
            <Slider ref="blue" txt={this.state.txt} update={this.update} />
            <label>{this.state.blue}</label>
          </div>
        );
      }
    });

    var Slider = React.createClass({
      render: function() {
        return (
            <div>
              <input ref="inp" type="range" min="0" max="255" onChange={this.props.update} />
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
  So far so good, That's it!!! See ya!!! :)
</p>
