---
layout: post
title: "ReactJS Component Lifecycle Updating"
date: 2015-11-21 11:36
comments: true
categories: [Javascript, ReactJS]
keywords: ReactJS Component Lifecycle Updating
---

<p>
  <img src="/images/reactjs.png" width="600" alt="ReactJS Component Lifecycle Updating" />
</p>

<p>
  ReactJS mounting have 4 methods:<br/>
  - <code>componentWillReceiveProps(object nextProps)</code>: is invoked when a mounted component receives new props. This method should be used to compare <code>this.props</code> and nextProps to perform state transitions using <code>this.setState()</code>.<br/>
  - <code>shouldComponentUpdate(object nextProps, object nextState): boolean</code>: is invoked when a component decides whether any changes warrant an update to the DOM. Implement this as an optimization to compare <code>this.props</code> with <code>nextProps</code> and <code>this.state</code> with <code>nextState</code> and return <code>false</code> if React should skip updating.<br/>
  - <code>componentWillUpdate(object nextProps, object nextState)</code>: is invoked immediately before updating occurs. You cannot call <code>this.setState()</code> here.<br/>
  - <code>componentDidUpdate(object prevProps, object prevState)</code>: is invoked immediately after updating occurs.
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
        return {increasing: false}
      },
      handleUpdate: function() {
        this.setProps({val: this.props.val+1});
      },
      componentWillReceiveProps: function(nextProps) {
        this.setState({increasing: nextProps.val > this.props.val});
      },
      shouldComponentUpdate: function(nextProps, nextState) {
        return nextProps.val % 5 === 0;
      },
      render: function() {
        console.log(this.state.increasing);
        return(
          <div>
            <button onClick={this.handleUpdate}>{this.props.val}</button>
          </div>
        );
      },
      componentDidUpdate: function(prevProps, prevState) {
        console.log('prevProps', prevProps);
      }
    });

    ReactDOM.render(<Button val={0} />, document.body);
  </script>
</body>
</html>
{% endcodeblock %}
