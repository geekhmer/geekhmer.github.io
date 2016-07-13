---
layout: post
title: "ReactJS Concepts and Getting Start"
date: 2015-10-30 21:17
comments: true
categories: [Javascript, ReactJS]
keywords: ReactJS Concepts and Getting Start, Learning ReactJS, ReactJS Concepts
---

<p>
  <img src="/images/reactjs.png" width="600" alt="ReactJS Concepts and Getting Start" />
</p>

<p>
  Well, before we start building anything meaningful, its important that we cover some base concepts first, so lets getting start.
</p>

<p>
  <h3>What is React?</h3>
  <a href="https://facebook.github.io/react/" target="_blank">React</a> is a UI library developed at Facebook to facilitate the creation of interactive, stateful and reusable UI components. It is used at Facebook in production, and Instagram.com is written in React.
</p>

<p>
  One of it’s unique points is that not only does it perform on the client side, but it can also be rendered server side, and they can work together inter-operably.
</p>

<p>
  <h3>Concepts</h3>
</p>

<p>
  React has quite a small API. This makes it fun to use, easy to learn, and simple to understand. However, being simple does not mean it's familiar. There are a few concepts to cover before getting started. Let's look at each in turn:
</p>

<p>
  <strong>React Elements</strong><br/>
  React elements are JavasScript objects which represent HTML elements. They don't exist in the browser. They represent browser elements such as an h1, div or section.
</p>

<p>
  <strong>JSX</strong><br/>
  JSX is a technique for creating React elements and components.
</p>

<p>
  <strong>Virtual DOM</strong><br/>
  The Virtual DOM is a JavaScript tree of React elements and components. React render the Virtual DOM to the browser to make the user interface visible. React observes the Virtual DOM for changes and automatically mutates browser DOM to mach the Virtual DOM.
</p>

<p>
  With a small understanding of these concepts we can move to using React. We will build a series of user interfaces, each adding a layer of functionality on the previous. We will build a photo stream similar to instagram.
</p>

<p>
  <strong>Rendering</strong><br/>
  The first order of business is rendering a virtual element (a React element or component). Remember, since a virtual element exists only in JavaScript memory, we must explicitly tell React to render it to the browser DOM.
</p>

{% codeblock lang:javascript %}
React.render(<img src='http://geekhmer.github.io/logo.png' />, document.body);
{% endcodeblock %}

<p>
  The render function accepts two arguments, a virtual element and a real DOM node. React takes the virtual element and inserts it into the given DOM node.
</p>

<p>
  <strong>Components</strong><br/>
  Components are the heart and soul of React. They are custom React elements. They are usually extended with unique functionality and structure.
</p>

{% codeblock lang:javascript %}
var Photo = React.createClass({
  render: function() {
    return <img src='http://geekhmer.github.io/logo.png' />;
  }
});

React.render(<Photo />, document.body);
{% endcodeblock %}

<p>
  The createClass function accepts an object which implements a render function.
</p>

<p>
  The Photo component is constructed and rendered to the document body.
</p>

<p>
  This component does nothing more than the previous React image element but it's ready to be extended with custom functionality and structure.
</p>

<p>
  <strong>Props</strong><br/>
  Props can be thought of as a component's options. They are given as arguments to a component and look exactly like HTML attributes.
</p>

{% codeblock lang:javascript %}
var Photo = React.createClass({
  render: function() {
    return (
      <div className='photo'>
        <img src={this.props.imageURL} />
        <span>{this.props.caption}</span>
      </div>
    );
  }
});

React.render(<Photo imageURL='http://geekhmer.github.io/logo.png' caption='Phnom Penh, Cambodia' />, document.body);
{% endcodeblock %}

<p>
  <strong>Specs, Lifecycle & State</strong><br/>
  The render method is the only required spec for creating a component, but there are serveral lifecycle methods & specs we can use that are mighty helpful when you actually want your component to do anything.
</p>

<p>
  Lifecycle Methods:><br/>
  - <code>componentWillMount</code> - Invoked once, on both client & server before rendering occurs.<br/>
  - <code>componentDidMount</code> - Invoked once, only on the client, after rendering occurs.<br/>
  - <code>shouldComponentUpdate</code> - Return value determines whether component should update.<br/>
  - <code>componentWillUnmount</code> - invoked prior to unmounting component.
</p>

<p>
  Specs:<br/>
  - <code>getInitialState</code> - Return value is the initial value for state.<br/>
  - <code>getDefaultProps</code> - Sets fallback props values if props are not supplied.<br/>
  - <code>mixins</code> - An array of objects, used to extend the current component's functionality.
</p>

<p>
  State<br/>
  The state object is internal to a component. It holds data which can change over time.
</p>

{% codeblock lang:javascript %}
var Photo = React.createClass({
  getInitialState: function() {
    return {liked: false};
  },
  toggleLiked: function() {
    this.setState({liked: !this.state.liked});
  },
  render: function() {
    var buttonClass= this.state.liked ? 'active' : '';
    return (
      <div className='photo'>
        <img src={this.props.src} />
        <div className='bar'>
          <button onClick={this.toggleLiked} className={buttonClass}>Heart</button>
          <span>{this.props.caption}</span>
        </div>
      </div>
    );
  }
});

React.render(Photo src='http://geekhmer.github.io/logo.png' caption='Phnom Penh, Cambodia', document.body);
{% endcodeblock %}

<p>
  Having state in a component introduces a bit more complexity.
</p>

<p>
  The component has a new function getInitialState. React calls this function when the component is initialised. The returned object is set as the component's initial state (as the function name implies).
</p>

<p>
  The component has another new function toggleLiked. This function calls setState on the component which toggles the liked value.
</p>

<p>
  Within the component's render function a variable buttonClass is assigned either 'active' or nothing -- depending on the liked state.
</p>

<p>
  buttonClass is used as a class name on the React button element. The button also has an onclick event handler set to the toggleLiked function.
</p>

<p>
Here's what happens when the component is rendered to the browser DOM:<br/>
- When the component's button is clicked, toggleLiked is called.<br/>
- The liked state is changed.<br/>
- React re-renders the component to the virtual DOM.<br/>
- The new virtual DOm is compared with previous virtual DOM.<br/>
- React isolates what has changed and updates the browser DOM.
</p>

<p>
  <strong>Composition</strong><br/>
  Composition means combining smaller components to form a larger whole. For example the Photo component could be used inside a PhotoGallery component.
</p>

{% codeblock lang:javascript %}
var Photo = React.createClass({
  toggleLiked: function() {
    this.setState({liked: !this.state.liked});
  },
  getInitialState: function() {
    return {liked: false}
  },
  render: function() {
    var buttonClass=this.state.liked ? 'active' : '';
    return (
      <div className='photo'>
        <img src={this.props.src} />
        <div className='bar'>
          <button onClick={this.toggleLiked} className={buttonClass}>Heart</button>
          <span>{this.props.caption}</span>
        </div>
      </div>
    );
  }
});

var PhotoGallery = React.createClass({
  getDataFromServer: function() {
    return [
      {
        url: 'http://geekhmer.github.io/phnom_penh.png',
        caption: 'Phnom Penh'
      },
      {
        url: 'http://geekhmer.github.io/sr.png',
        caption: 'SR'
      }
    ];
  },
  render: function() {
    var data=this.getDataFromServer();

    var photos = data.map(function(photo) {
      return <Photo src={photo.url} caption={photo.caption} />
    });

    return (
      <div className='photo-gallery'>
        {photos}
      </div>
    );
  }
});

React.render(<PhotoGallery />, document.body);
{% endcodeblock %}

<p>
  The Photo component is exactly the same as before.
</p>

<p>
  There's a new PhotoGallery component which generates Photo components. In this case there's some fake server data which return an array of 2 objects, each with a url and caption.
</p>

<p>
  The data is looped over and will generates 2 Photo components which are inserted into the return value of the component's render function.
</p>

<p>
  <strong>Events</strong><br/>
  React also has a built in cross browser events system. The events are attached as properties of components and can trigger methods. Lets make our count increment below using events:
</p>

{% codeblock lang:javascript %}
var Counter = React.createClass({
  incrementCount: function() {
    this.setSate({count: this.state.count + 1});
  },
  render: function() {
    return (
      <div class="my-component">
        <h1>Count: {this.state.count}</h1>
        <button type="button" onClick={this.incrementCount}>Increment</button>
      </div>
    );
  }
});

React.render(<Counter />, document.body);
{% endcodeblock %}

<p>
React events:<br/>
- Clipboard Events - <code>onCopy</code> <code>onCut</code> <code>onPaste</code><br/>
- Keyboard Events - <code>onKeyDown</code> <code>onKeyPress</code> <code>onKeyUp</code><br/>
- Focus Events - <code>onFocus</code> <code>onBlur</code><br/>
- Mouse Events - <code>onClick</code> <code>onDoubleClick</code> <code>onDrag</code> <code>onDragEnd</code> <code>onDragEnter</code> <code>onDragExit</code> <code>onDragLeave</code> <code>onDragOver</code> <code>onDragStart</code> <code>onDrop</code> <code>onMouseDown</code> <code>onMouseEnter</code> <code>onMouseLeave</code> <code>onMouseMove</code> <code>onMouseOut</code> <code>onMouseOver</code> <code>onMouseUp</code><br/>
- Touch events - <code>onTouchCancel</code> <code>onTouchEnd</code> <code>onTouchMove</code> <code>onTouchStart</code><br/>
- UI Events - <code>onScroll</code> <code>onWheel</code><br/>
- Form Events - <code>onChange</code> <code>onInput</code> <code>onSubmit</code>
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
