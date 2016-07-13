---
layout: post
title: "ReactJS PropTypes"
date: 2015-11-14 10:14
comments: true
categories: [Javascript, ReactJS]
keywords: ReactJS PropTypes
---

<p>
  <img src="/images/reactjs.png" width="600" alt="ReactJS PropTypes" />
</p>

<p>
  ReactJS is built around the concept of components. The components get many specific attributes just like a HTML tag does.
</p>

<p>
  The attributes are called "props" in ReactJS and can be of any type. It can be a string, function or an Array, as long as its valid javascript you can use it as a prop.
</p>

{% codeblock lang:javascript %}
< MyComponent size={24} position="fixed" /> 
// size & position is props made up by MyComponent component.
// Using anything other than a string you need to wrap your props in {}
{% endcodeblock %}

<p>
  <strong>What is PropTypes?</strong><br/>
  PropTypes defines type and which props are required. This benefits the future you using your component in two ways:<br/>
  1. You can easily open up a component and check which props are required and what type they should be.<br/>
  2. When things get messed up ReactJS will give you an awesome error message in the console, saying which props is wrong/missing plus the render method that caused the problem.
</p>

<p>
  So how do we write the propTypes?
</p>

{% codeblock lang:javascript %}
propTypes: {
  size: React.PropTypes.number,
  position: React.PropTypes.string.isRequired
}
{% endcodeblock %}

<p>
  And more awesome, if we can’t find a PropType that suits our needs we can write own with regex or shapes:
</p>

{% codeblock lang:javascript %}
propTypes: {
  email: function(props, propName, componentName) {
    if (!/emailRegex/.test(props[email])) {
      return new Error('Give me a real email!');
    }
  },
  user: React.PropTypes.shape({
    name: React.PropTypes.string.isRequired,
    age: React.PropTypes.number
  }).isRequired
}
{% endcodeblock %}

<p>
  Example:
</p>

{% codeblock lang:javascript %}
<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8" />
    <title>ReactJS PropTypes</title>
    <script src="react.js"></script>
    <script src="JSXTransformer.js"></script>
  </head>
  <body>
    <script type="text/javascript">
      var Profile = React.createClass({
        getDefaultProps: function() {
          return {
            name: 'This is a default prop',
            age: 0,
            activate: true
          }
        },
        propTypes: {
          name: React.PropTypes.string,
          age: React.PropTypes.number.isRequired,
          activate: React.PropTypes.bool.isRequired
        },
        render: function() {
          return(
            <div>
              <h1>Name: {this.props.name}</h1>
              <h1>Age: {this.props.age}</h1>
              <h1>Activate: {this.props.activate}</h1>
            </div>
          );
        }
      });

      React.render(<Profile name="Bunlong" age={0} activate={true} />, document.body);
    </script>
  </body>
</html>
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
