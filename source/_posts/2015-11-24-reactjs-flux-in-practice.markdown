---
layout: post
title: "ReactJS Flux in Practice"
date: 2015-11-24 09:40
comments: true
categories: [Javascript, ReactJS]
keywords: What is Flux?
---

<p>
  <img src="/images/react_flux.jpg" width="600" alt="ReactJS Flux in Practice" />
</p>

<p>
  To build a ReactJS application with Flux I have deviced a little library called flux-react.
</p>

<p>
  <strong>Actions</strong><br/>
  Actions are what links your components (and anything else that wants to change state) with your stores. You define them just by naming them. When calling an action the arguments will be deepCloned to avoid later mutation of complex objects passed to the store.
</p>

{% codeblock lang:javascript %}
var Actions = flux.createActions([
  'addItem']
);
{% endcodeblock %}

<p>
  <strong>Stores</strong><br/>
  Stores are where you define your application state, update it and notify components about changes.
</p>

{% codeblock lang:javascript %}
var Store = flux.createStore({
  // We put the state directly on the store object
  lists: [],
  // Then we point to the actions we want to react to in this store
  actions: [
    Actions.addItem
  ],
  // The action maps directly to a method. So action addItem maps to the
  // method addItem()
  addItem: function (item) {
    this.lists.push(item);
    this.emit('list.add');
  },
  // The methods that components can use to get state information
  // from the store. The context of the methods is the store itself. 
  // The returned values are deepcloned, which means
  // that the state of the store is immutable
  exports: {
    getLists: function () {
      return this.lists;
    }
  }
});
{% endcodeblock %}

<p>
  Lets see how this is used in a component before going over the concept:
</p>

{% codeblock lang:javascript %}
var ComponentList = React.createClass({
  getInitialState: function () {
    return {
      lists: Store.getLists()
    };
  },
  triggerAction: function () {
    Actions.addItem("Item added");
  },
  onChange: function () {
    this.setState({
      lists: Store.getLists()
    });
  },
  componentWillMount: function () {
    Store.on('lists.add', this.onChange);
  },
  componentWillUnmount: function () {
    Store.off('lists.add', this.onChange);
  },
  render: function () {
    return (
      <div>
        <button onClick={this.triggerAction}>Click</button>
        <br/><br/>
        {JSON.stringify(this.state.lists)}
      </div>
    );
  }
});
{% endcodeblock %}

<p>
  Okay. So what I noticed was that my actions always mapped directly to a method. If the action was called "addItem", the method handling that action in my store was also called "addItem". That is why actions in "flux-react" map directly to a method.
</p>

<p>
  An other concept is the "exports". You only want to expose a set of getter methods that your components can use. Three things happens with exports:<br/>
  - The exports object is the object that is returned when creating a store.<br/>
  - All methods in exports are bound to the store, letting you use "this" to point to the state in the store.<br/>
  - Exported values are automatically deep cloned.
</p>

<p>
  Now that last part needs a bit more explenation. The state, in your store should only be changed inside the store. ex. returning a list of todos to a component should not allow that component to do changes there that is reflected in the store. This is because of debugging. If lots of different components starts to change state directly in your store you will get into trouble. So instead the "flux-react" store makes sure that any value returned from exports is deep cloned, right out of the box. The same goes for complex objects passed as arguments to an action. We do not want them to be changed later outside of the store and by doing so change the state of the store.
</p>

<p>
  How about performance? Well, the thing is that values you return from a store are not big, neither are values you pass as arguments. Yes, maybe you have a phonebook of 10.000 people in your store, but your interface will never show all 10.000 of them. You may grab the 50 first of them and the rest requires searching or pagination. In that case it is only the search result, or next page, that is deep cloned. Never all the 10.000 people.
</p>

<p>
  Full codes:
</p>

{% codeblock lang:javascript %}
<!DOCTYPE html>
<html>
  <head>
    <script type="text/javascript" src="http://fb.me/react-0.12.2.js"></script>
    <script type="text/javascript" src="http://fb.me/JSXTransformer-0.12.2.js"></script>
    <script src="flux-react.js"></script>
  </head>
<body>
  <script type="text/jsx">
    var Actions = flux.createActions([
      'addItem']
    );

    var Store = flux.createStore({
      lists: [],
      actions: [
        Actions.addItem
      ],
      addItem: function (item) {
        this.lists.push(item);
        this.emit('lists.add');
      },
      exports: {
        getLists: function () {
          return this.lists;
        }
      }
    });

    var ComponentList = React.createClass({
      getInitialState: function () {
        return {
          lists: Store.getLists()
        };
      },
      triggerAction: function () {
        Actions.addItem("Item added");
      },
      onChange: function () {
        this.setState({
          lists: Store.getLists()
        });
      },
      componentWillMount: function () {
        Store.on('lists.add', this.onChange);
      },
      componentWillUnmount: function () {
        Store.off('lists.add', this.onChange);
      },
      render: function () {
        return(
          <div>
            <button onClick={this.triggerAction}>Click</button>
            <br/><br/>
            {JSON.stringify(this.state.lists)}
          </div>
        );
      }
    });

    ReactDOM.renderComponent(<ComponentList />, document.body);
  </script>
</body>
</html>
{% endcodeblock %}

<p>
  You can download the source code <a href="https://github.com/Bunlong/reactjs_flux_in_practice" target="_blank">here</a>.
</p>

<p>
  So far go good, That's it!!! See ya!!! :)
</p>
