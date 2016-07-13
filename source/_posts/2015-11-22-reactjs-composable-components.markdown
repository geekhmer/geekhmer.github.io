---
layout: post
title: "ReactJS Composable Components"
date: 2015-11-22 13:17
comments: true
categories: [Javascript, ReactJS]
keywords: ReactJS Composable Components
---

<p>
  <img src="/images/reactjs.png" width="600" alt="ReactJS Composable Components" />
</p>

<p>
  ReactJS is all about building reusable components. In fact, with React the only thing you do is build components. Since they're so encapsulated, components make code reuse, testing, and separation of concerns easy. Let see the example below:
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
          red: 0
        }
      },
      update: function(e) {
        this.setState({
          red: this.refs.red.refs.inp.getDOMNode().value
        });
      },
      render: function() {
        return (
          <div>
            <NumInput 
              ref="red"
              min={0}
              max={255}
              step={0.01}
              val={this.state.red}
              type="number"
              label="Red"
              update={this.update} />
          </div>
        );
      }
    });

    var NumInput = React.createClass({
      propTypes: {
        min: React.PropTypes.number,
        max: React.PropTypes.number,
        step: React.PropTypes.number,
        val: React.PropTypes.number,
        label: React.PropTypes.string,
        update:React.PropTypes.func.isRequired,
        type: React.PropTypes.oneOf(['number', 'range'])
      },
      getDefaultProps: function() {
        return {
          min: 0,
          max: 0,
          step: 1,
          val: 0,
          label: '',
          type: 'range'
        }
      },
      render: function() {
        var label = this.props.label !== '' ? <label>{this.props.label} {this.props.val}</label> : ''
        return (
            <div>
              <input 
                ref="inp" 
                type={this.props.type} 
                min={this.props.min} 
                max={this.props.max} 
                step={this.props.step} 
                defaultValue={this.props.val} 
                onChange={this.props.update} />
                {label}
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
  So far go good, That's it!!! See ya!!! :)
</p>
