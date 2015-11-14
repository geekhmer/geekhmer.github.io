---
layout: post
title: "Why you should use ReactJS for your next project?"
date: 2015-10-29 21:09
comments: true
categories: [Javascript, ReactJS]
keywords: Why you should use ReactJS for your next project?
---

<p>
  <img src="/images/reactjs.png" width="600" alt="Why you should use ReactJS for your next project?" />
</p>

<p>
  <a href="https://facebook.github.io/react/" target="_blank">React</a>, developed by Facebook, is an open source library for building User Interfaces. As the front end world moves too fast. I am a big fan of AngularJS and a few months back I started trying React. The biggest reason for me to try React was to create apps that render on both client and server (with a little help from other libraries as well). In this article I am going to share the best reasons to use React in your next project:
</p>

<p>
  <strong>1. Can Render on Both Client and Server</strong><br/>
  Well, React uses a concept called Virtual DOM (discussed subsequently) that gives your UI a great performance boost. You can build the UI using React and render it on both server and client. This has a strong advantage over traditional SPAs (Single Page Apps). While SPAs are great for building rich AJAX based web apps. SEO is the biggest challenge. With React what you can do is take a hybrid approach and create apps. The initial request to your app should be a server side rendered version. The subsequent user interactions should use client side rendering. With this approach your app takes advantage of all the features of SPAs while still being search engine friendly. You don't need the techniques to pre-render your pages anymore.
</p>

<p>
  <strong>2. Component Oriented Development is Good</strong><br/>
  React takes a component oriented approach. You can break your whole UI in terms of reusable components. As the components are well encapsulated, reusing and testing.
</p>

<p>
  <strong>3. Separation of Concerns</strong><br/>
  By expressing your UI in terms of well encapsulated and reusable components you achieve separation of concerns. A component shouldn't try to do more than one thing. Rather it should do only one job and anytime you need to separate the concerns you build new components.
</p>

<p>
  <strong>4. Simple and Declarative</strong><br/>
  Your components represent how your app looks at any given time. Simply feed some data to your components and React takes care of refreshing the components when the underlying data changes.
</p>

<p>
  <strong>5. Virtual DOM is Fast</strong><br/>
  React creates a fake DOM for you. Most of the times you will be working with this virtual DOM as it's way faster than manipulating the actual DOM. React keeps an in memory copy of the DOM and your components describe how the DOM looks at any given time. So, React is fast in computing the Diff and knows which parts to update. In short React decides what's the fastest way to update the UI and this technique is definitely performant.
</p>

<p>
  <strong>6. JSX is Cool and Simple</strong><br/>
  JSX is known as JavaScript XML transform. You create your components with JSX which are then compiled into JavaScript. Don't worry! React provides tools which continuously watch your source directory and re build whenever there is a code change. JSX, on the other hand, has 2 simple benefits such as:<br/>
  - It's easier to visualize the DOM with JSX syntax.<br/>
  - Designers can be very comfortable with JSX.
</p>

<p>
  <strong>7. Reactive Updates</strong><br/>
  React's philosophy is based on Reactive Updates. Traditionally what we do is watch the data and update our UI whenever the data changes. On the other hand React components have a render method which describes how the DOM should look like at any point. React caches the value obtained from the last call to render and compares it each time render is re-run. So, it knows exactly which part of your component needs to be updated and refreshes the UI accordingly.
</p>

<p>
  <strong>8. Event Handling Done Right</strong><br/>
  React takes care to bubble and capture events according to W3C spec. It uses a synthetic event system to ensure all the events work consistently across the browsers IE 8+. So, you should not be afraid of the inconsistent DOM APIs anymore.
</p>

<p>
  <strong>9. React Addons</strong><br/>
  React Addons offer additional utilities that can help you write better UIs. For example, you can use the Animation and Two way data binding Addons. You can also use the testing addon to test React components using test framework of your choice.
</p>

<p>
  <strong>10. Facebook has Created Something Awesome</strong><br/>
  React is being used in large websites such as Facebook, Instagram, The New York Times and many others. Clearly, React is doing a great job.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
