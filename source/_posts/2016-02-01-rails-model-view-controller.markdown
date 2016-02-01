---
layout: post
title: "Rails Model View Controller"
date: 2016-02-01 15:33
comments: true
categories: [Ruby on Rails]
keywords: Rails Model View Controller, Model View Controller Rails, Model View Controller
---

<p>
  <img src="/images/rails_mvc.png" width="500" alt="Rails Model View Controller" />
</p>

<p>
  The <strong>browser</strong> makes a request, such as http://geekhmer.com/video/show/15
</p>

<p>
  The <strong>web server</strong> (mongrel, WEBrick, etc.) receives the request. It uses routes to find out which controller to use: the default route pattern is <code>/controller/action/id</code> as defined in <code>config/routes.rb</code>. In our case, it’s the "video" controller, method "show", id "15". The web server then uses the dispatcher to create a new controller, call the action and pass the parameters.
</p>

<p>
  <strong>Controllers</strong> do the work of parsing user requests, data submissions, cookies, sessions and the "browser stuff". They’re the pointy-haired manager that orders employees around. The best controller is Dilbert-esque: It gives orders without knowing (or caring) how it gets done. In our case, the show method in the video controller knows it needs to lookup a video. It asks the model to get video 15, and will eventually display it to the user.
</p> 

<p>
  <strong>Models</strong> are Ruby classes. They talk to the database, store and validate data, perform the business logic and otherwise do the heavy lifting. They’re the chubby guy in the back room crunching the numbers. In this case, the model retrieves video 15 from the database.
</p>

<p>
  <strong>Views</strong> are what the user sees: HTML, CSS, XML, Javascript, JSON. They’re the sales rep putting up flyers and collecting surveys, at the manager’s direction. Views are merely puppets reading what the controller gives them. They don’t know what happens in the back room. In our example, the controller gives video 15 to the "show" view. The show view generates the HTML: divs, tables, text, descriptions, footers, etc.
</p>

<p>
  The controller returns the response body (HTML, XML, etc.) & metadata (caching headers, redirects) to the server. The server combines the raw data into a proper HTTP response and sends it to the user.
</p>

<p>
  It’s more fun to imagine a story with "fat model, skinny controller" instead of a sterile "3-tiered architecture". Models do the grunt work, views are the happy face, and controllers are the masterminds behind it all.
</p>

<p>
  Many MVC discussions ignore the role of the web server. However, it’s important to mention how the controller magically gets created and passed user information. The web server is the invisible gateway, shuttling data back and forth: users never interact with the controller directly.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
