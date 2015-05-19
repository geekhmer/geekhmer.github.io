---
layout: post
title: "Ruby on Rails with Bootstrap-Sass"
date: 2015-05-11 16:41
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Ruby on Rails with Bootstrap-Sass
---

<p>
  <img src="/images/bootstrap.png" alt="Ruby on Rails with Bootstrap-Sass" />
</p>

<p>
  Twitter Bootstrap is an insanely popular CSS framework that is used to quickly and easily throw together great looking websites. Many websites (including this one) are built on this framework, and with good reason; the power and ease of use rival that of most other frameworks.
</p>

<p>
  In this tutorial we will show you how to add the framework using the bootstrap-sass gem. We will also show you how to override the bootstrap styles using the various scss variables provided by the bootstrap framework. Finally we will show you how to use the will_paginate-bootstrap gem to style your pagination using bootstrap's pagination styles.
</p>

<p>
  <strong>Setup Project</strong><br/>
  In an existing or new project, add the following line to Gemfile:
</p>

{% codeblock Gemfile lang:ruby %}
gem 'bootstrap-sass', '~> 2.3'
{% endcodeblock %}

<p>
  Run a <code>bundle install</code> and start your <code>rails server</code>. Now that the gem is installed you are ready to to start using bootstrap.
</p>

<p>
  The first thing we will need to do is create a new SCSS file to store our bootstrap configuration. Create a file called <code>bootstrap_config.scss</code> in the <code>app/assets/stylesheets</code> folder.  This file will be used to store bootstrap specific configuration. Next, we will need to tell Rails to actually include bootstrap in this file. Open <code>app/assets/stylesheets/bootstrap_config.scss</code> and add the following line:
</p>

{% codeblock app/assets/stylesheets/bootstrap_config.scss lang:ruby %}
@import "bootstrap";
{% endcodeblock %}

<p>
  If you want a responsive layout, also add the following line:
</p>

{% codeblock app/assets/stylesheets/bootstrap_config.scss lang:ruby %}
@import "bootstrap-responsive";
{% endcodeblock %}

<p>
  This will include bootstrap in your application. Now, any views you create will automatically include bootstrap as long as they use the main application layout. We aren't quite done yet though, if you are on Rails 4 (Rails 3.2 users don't need to do this) you will need to perform one more step. Open up the <code>config/application.rb</code> file and add the following line to your application's configuration:
</p>

{% codeblock config/application.rb lang:ruby %}
config.assets.precompile += %w(*.png *.jpg *.jpeg *.gif)
{% endcodeblock %}

<p>
  This is due to rails no longer compiling images in vendor assets by default.
</p>

<p>
  Now go ahead and create a controller and try it out. If you aren't familiar with the framework itself, the absolute best place to learn about it is at <a href="http://getbootstrap.com/" target="_blank">The Official Twitter Boostrap Site</a>. That's all there is to setting it up!
</p>

<p>
  <strong>Bootstrap Variables</strong><br/>
  The bootstrap-sass gem, much like it's official LESS-based cousin, has a number of variables that you can override to change the look and feel of various styles. Lets try it out. Suppose we want to change the background color of our website. To do this we just need to open up <code>app/assets/stylesheets/bootstrap_config.scss</code> and add the following line to the very top of the file:
</p>

{% codeblock app/assets/stylesheets/bootstrap_config.scss lang:ruby %}
$bodyBackground: #e0e0e0;
{% endcodeblock %}

<p>
  Now, if you refresh the page you will notice that the body background has changed to a light gray color. 
</p>

<p>
  <strong>Will Paginate</strong><br/>
  One pitfall that rails users often run into is utilizing will_paginate in a twitter bootstrap project. Luckily the will_paginate-bootstrap gem makes this very easy. Add the following line to your gemfile and then run a <code>bundle install</code> and restart your rails server:
</p>

{% codeblock Gemfile lang:ruby %}
gem 'will_paginate-bootstrap'
{% endcodeblock %}

<p>
  Now, open up the view where you are using will_paginate, find any will_paginate lines and add <code>:renderer => BootstrapPagination::Rails</code> to the end of them so they look like this:
</p>

{% codeblock lang:ruby %}
<%= will_paginate @products, :renderer => BootstrapPagination::Rails %>
{% endcodeblock %}

<p>
  Now if you refresh the page, you'll see that that will_paginate is now styled using bootstrap.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
