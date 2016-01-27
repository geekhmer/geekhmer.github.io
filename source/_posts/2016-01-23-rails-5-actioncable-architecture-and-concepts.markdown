---
layout: post
title: "Rails 5 ActionCable Architecture and Concepts"
date: 2016-01-23 00:23
comments: true
categories: [Ruby, Ruby on Rails]
keywords: ActionCable, Rails 5 ActionCable, Rails ActionCable, Rails 5 ActionCable Architecture and Concepts, Rails 5 ActionCable Architecture, Rails 5 ActionCable Concepts
---

<p>
  <img src="/images/rail_5_beta_1.jpg" width="600" alt="Rails 5 ActionCable Architecture and Concepts" />
</p>

<p>
  ActionCable is an component of Ruby on Rails 5.0 - it is the first "official" solution for integrating websocket communication with Rails. 
</p>

<p>
  <strong>Websocket Concepts</strong><br/>
  The basic protocol that is pervasive in browser server interaction is HTTP. This is the protocol that gets used when a browser asks for an HTML page, JavaScript or CSS assets.
</p>

<p>
  An HTTP connection is typically short-lived, initiated by the browser and ends when the application server has returned a response. This poses a problem given an event that browsers need to know about, but have not asked for - the server has no way to send data to the browser unasked.
</p>

<p>
  Before websockets, developers have opted for polling loops or long-running http requests (ActionController::Live) to solve such cases, both of which have their own technical difficulties.
</p>

<p>
  Websockets enable browsers and application servers to keep an open connection, enabling both parties to initiate sending data to each other. Given a websocket connection, a server will send an event message to the browser through an open websocket connection, enabling direct interaction between events on a server and the browser.
</p>

<p>
  <strong>ActionCable Architecture and Concepts</strong><br/>
  Traditionally, the websocket way of handling connections did not integrate well into a Rails application - the process of handling a request in Rails is fundamentally aligned with the request-response-end way HTTP connections are handled.
</p>

<p>
  As such, ActionCable does not hook into the typical Rails MVC handling of requests, but adds another entry point to a Rails application - the ActionCable server: This server is, as of now, started as a different process dedicated to handling multiple open websocket connections while loading all components of the Rails application and providing developers the comfort of using its components. The Rails server will propagate messages to the ActionCable server by pushing them to a queue that the ActionCable server is listening to. In the current implementation, this role is given to a Redis instance.
</p>

<p>
  We can explore the architecture and the way communication works between the components using the example of a chat application: It allows multiple users to connect to it and send messages to each other. Users receive messages from other users immediately i.e. users will not need to refresh the browser to see a new message but see it pop up at the end of the message list. You know, a chat like you would expect.
</p>

<p>
  Let's trace one possible way of propagation of a user’s message:<br/>
  - A user opens the page in his browser which in turn opens a websocket connection to the ActionCable server (Websocket).<br/>
  - A user sends the message by remote form submission to the Rails server (HTTP).<br/>
  - The Rails server persists the message and publishes a message including the user and the message body to the queue. It sends an acknowledgement to the user. This ends the HTTP request.<br/>
  - The ActionCable server receives the published message from the queue. It publishes the user and message body to all relevant open websocket connections.<br/>
  - All relevant browsers connected to the ActionCable server receive the message and show it in the DOM.
</p>


<p>
  <img src="/images/rails_5_actioncable_concepts.png" width="600" alt="Rails 5 ActionCable Architecture and Concepts" />
</p>

<p>
  To allow for differentiation between different groups of users, ActionCable uses the concept of channels: A user may subscribe to a select few of all available channels and thus only receive messages meant for the given channels. If the chat application were to have multiple "rooms", a user could subscribe to each room via a designated channel.
</p>

<p>
  On the technical side, ActionCable uses EventMachine to handle connections and spawns a new Ruby thread for handling messages. This means that an ActionCable server will need to be multi-threaded.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
