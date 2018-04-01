---
layout: post
title: "New Way to Develop React App with Create React App (No Build Configuration)"
date: 2017-05-22 00:10
categories: [Javascript, ReactJS]
keywords: New Way to Develop React App with Create React App (No Build Configuration), ReactJS
---

<p>
  <img src="/images/create_react_app.png" width="600" alt="New Way to Develop React App with Create React App (No Build Configuration)" />
</p>

<p>
  Setting up Gulp, Webpack, Browserify, Babel, JSX, ES6, ES6 modules, hot reloading, ... etc. manually - forget about it and no more fuss with it. 
</p>

<p>
  Inspired by the cohesive developer experience provided by Ember.js Facebook wanted to provide an easy way to develop React apps, they created <a href="https://github.com/facebookincubator/create-react-app" target="_blank">create-react-app</a> with the targets zero configuration.
</p>

<p>
  <strong>Installation</strong><br/>
  You may need <a href="https://nodejs.org/en/download/" target="_blank">NPM</a> installed and you can use <a href="https://github.com/creationix/nvm#usage" target="_blank">NVM</a> to easily switch Node versions between different projects.
</p>

<p>
  The Node installation is only required for Create React App itself.
</p>

<p>
  To install create-react-app module, run:
</p>

{% codeblock lang:ruby %}
npm install -g create-react-app
{% endcodeblock %}

<p>
  <strong>Creating an App</strong><br/>
  To create a new app, run:
</p>

{% codeblock lang:ruby %}
create-react-app geekhmer
{% endcodeblock %}

<p>
  It will create a directory called geekhmer inside the current folder. And inside that directory, it will generate the initial project structure and install the transitive dependencies:
</p>

{% codeblock lang:ruby %}
geekhmer/
  README.md
  node_modules/
  package.json
  .gitignore
  public/
    favicon.ico
    index.html
  src/
    App.css
    App.js
    App.test.js
    index.css
    index.js
    logo.svg
{% endcodeblock %}

<p>
  No configuration or complicated folder structures, just the files you need to build your app.
</p>

<p>
  <strong>Run the App</strong><br/>
  Runs the app in development mode:
</p>

{% codeblock lang:ruby %}
npm start
{% endcodeblock %}

<p>
  Open http://localhost:3000 to view it in the browser.
</p>

<p>
  <strong>Run the Test</strong><br/>
  Runs the test watcher in an interactive mode:
</p>

{% codeblock lang:ruby %}
npm test
{% endcodeblock %}

<p>
  <a href="https://github.com/facebookincubator/create-react-app/blob/master/packages/react-scripts/template/README.md#running-tests" target="_blank">Read more about testing.</a>
</p>

<p>
  <strong>Builds the App for Production</strong><br/>
  Builds the app for production to the build folder. It correctly bundles React in production mode and optimizes the build for the best performance.
</p>

<p>
  The build is minified and the filenames include the hashes. By default, it also <a href="https://github.com/facebookincubator/create-react-app/blob/master/packages/react-scripts/template/README.md#making-a-progressive-web-app" target="_blank">includes a service worker</a> so that your app loads from local cache on future visits.
</p>

{% codeblock lang:ruby %}
npm run build
{% endcodeblock %}

<p>
  Your app is ready to be deployed. So far so good, That's it!!! See ya!!! :)
</p>
