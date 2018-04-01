---
layout: post
title: "Create-React-App Vs NextJs"
date: 2017-11-04 22:13
comments: true
categories: [Other]
keywords: React, ReactJs, Create-React-App, CRP, NextJs, Create React App
---

<p>
  <img src="/images/cra_vs_next.png" width="600" alt="Create-React-App Vs NextJs" />
</p>

<p>
  <a href="https://github.com/zeit/next.js/" target="_blank">NextJs</a> is a new project with a lot to offer.
</p>

<p>
  The comparison between NextJs and Create-React-App is an apt one. What NextJs brings is great defaults. Like Create-React-App, NextJs is opinionated. It makes choices for you about what an ideal React setup should look like.
</p>

<p>
  One of the biggest pain points in starting a new javascript App is the tooling. Webpack, babel, and the like can be a pain to setup, especially with the aggressive release cycle of open source javascript projects. As of this writing you're probably already using Webpack syntax that's been deprecated.

</p>

<p>
  Here are the biggest differences between Create-React-App and NextJs.
</p>

<p>
  <strong>Create-React-App Is Ejectable, NextJs Is Extensible</strong><br/>
</p>

<p>
  Create-React-App uses babel, webpack, and eslint but "hides" this tooling and bundles it together in react-scripts. But Create-React-App doesn't lock you in; when you're ready to depart from training wheels you can unmask these dependencies and then configure them.
</p>

<p>
  NextJs, on the other hand, provides great defaults with the option to configure tooling if you want to. For example, you can override (or extend) NextJs's webpack configuration by adding a webpack.config.js file. Or you can add an express server if you don't want to use NextJs' server.
</p>

<p>
  <strong>NextJs is Out Of The Box</strong>
</p>

<p>
  The biggest point of NextJs is server-side rendering.
</p>

<p>
  People will tell you that Google crawls javascript and that it's sufficient to serve up an almost-empty html document with <code>root</code> class along with a massive bundle.js.
</p>

<p>
  It's true that Google crawls javascript. But this just isn't a good approach for apps that are content-focused and need to expose their content to search. 
</p>

<p>
  <strong>Styling is A Pain With NextJs</strong>
</p>

<p>
  NextJs can be a pain with styling. Out of the box, NextJs uses styled-jsx, which is OK. But what if you want to use SASS or styled-components? You're in for a few hours of frustration.
</p>

<p>
  <strong>You Can't make API Calls In Components With NextJs</strong>
</p>

<p>
  Initializing a new NextJs project creates two directores ./pages and ./components.
</p>

<p>
  Pages are like container React components. But they have more significance than simply wrapping other components. Page components are literally rendered into pages with a little help from react-router. That is, http://localhost:3000/about points to ./pages/about.js. This approach has strengthes and limitations. One of the limitations is that you can only make a client-side fetch request in top-level page components.
</p>

<p>
  <strong>Create-React-App Vs NextJs: Comparison Table</strong>
</p>

<table>
  <thead>
    <tr>
      <th></th>
      <th>Create React App</th>
      <th>NextJs</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Dependencies</td>
      <td>One (react-scripts)</td>
      <td>One (next)</td>
    </tr>
    <tr>
      <td>Ejectable</td>
      <td>Yes</td>
      <td>No</td>
    </tr>
    <tr>
      <td>Extensible</td>
      <td>No</td>
      <td>Yes</td>
    </tr>
    <tr>
      <td>Isomorphic/Universal</td>
      <td>No</td>
      <td>Yes</td>
    </tr>
    <tr>
      <td>Zero-configuration</td>
      <td>Yes</td>
      <td>Yes</td>
    </tr>
    <tr>
      <td>Service workers</td>
      <td>Yes</td>
      <td>No</td>
    </tr>
    <tr>
      <td>Hot-reloading</td>
      <td>Yes</td>
      <td>Yes</td>
    </tr>
    <tr>
      <td>Code-splitting</td>
      <td>Can be configured</td>
      <td>Out of the box</td>
    </tr>
  </tbody>
</table>

<br/>

<p>
  <strong>Conclusion</strong>
</p>

<p>
  NextJs is a good start if you need SSR first, SEO friendly with lots of public content. But if you build a highly dynamic statically deployed Single Page Application client, CRA (Create React App) is better suited for that.
</p>

<p>
  So for blog, news, with lots of public content and shareability, I'll go with NextJs. For dashboard, admin, apps, I'll go with CRA (Create React App)
</p>
