---
layout: post
title: "What is Flux?"
date: 2015-11-22 16:27
comments: true
categories: [Javascript, ReactJS]
keywords: What is Flux?
---

<p>
  <img src="/images/react_flux.jpg" width="600" alt="What is Flux?" />
</p>

<p>
  Flux is an architecture that Facebook uses internally when working with React. It is not a framework or a library. It is a new kind of architecture that complements React and the concept of Unidirectional Data Flow (Central to the Flux Pattern).
</p>

<p>
  Facebook provide a Dispatcher javascript library. The dispatcher is a kind of global pub/sub handler that broadcasts payloads to registered callbacks.
</p>

<p>
  Flux architecture will use this Dispatcher javascript library, along with NodeJS’s EventEmitter module in order to set up an event system that helps manage an applications state.
</p>

<p>
  <strong>Flux have 4 layers:</strong><br/>
  - <code>Actions</code> - Helper methods that facilitate passing data to the Dispatcher.<br/>
  - <code>Dispatcher</code> - Receives actions and broadcasts payloads to registered callbacks.<br/>
  - <code>Stores</code> - Containers for application state & logic that have callbacks registered to the dispatcher.<br/>
  - <code>Controller Views</code> - React Components that grab the state from Stores and pass it down via props to child components.
</p>

<p>
  <strong>Structure and Data Flow </strong><br/>
  Data in a Flux application flows in a single direction:<br/>
</p>

<p>
  <img src="/images/flux_diagram_1.png" width="600" alt="What is Flux?" />
</p>

<p>
  The dispatcher, stores and views are independent nodes with distinct inputs and outputs. The actions are simple objects containing the new data and an identifying type property.
</p>

<p>
  The views may cause a new action to be propagated through the system in response to user interactions:
</p>

<p>
  <img src="/images/flux_diagram_2.png" width="600" alt="What is Flux?" />
</p>

<p>
  All data flows through the dispatcher as a central hub. Actions are provided to the dispatcher in an action creator method, and most often originate from user interactions with the views. The dispatcher then invokes the callbacks that the stores have registered with it, dispatching actions to all stores. Within their registered callbacks, stores respond to whichever actions are relevant to the state they maintain. The stores then emit a change event to alert the controller-views that a change to the data layer has occurred. Controller-views listen for these events and retrieve data from the stores in an event handler. The controller-views call their own <code>setState()</code> method, causing a re-rendering of themselves and all of their descendants in the component tree.
</p>

<p>
  <img src="/images/flux_diagram_3.png" width="600" alt="What is Flux?" />
</p>

<p>
  <strong>Summary</strong><br/>
  Flux is a pattern for unidirectional data flows Actions encapsulate events Dispatcher is a central hub that holds callbacks Stores hold app state Many implementations
</p>

<p>
  So far after reading this article, I hope that if you didn’t get Facebook’s Flux Architecture before, that now you can say you do. 
</p>

<p>
  If you want to use the Flux architecture. but you don't know how to use it and which library you should use, please stay turned for the next article. See you!!! :)
</p>