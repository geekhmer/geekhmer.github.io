---
layout: post
title: "Tips to Improve the Performance of Ruby on Rails Application"
date: 2014-12-01 22:06
comments: true
categories: [Other]
keywords: Tips to Improve the Performance of Ruby on Rails Application, Ruby, Ruby on Rails, Rails 4, Ruby on Rails 4
---

<p>
  <img src="/images/ruby_on_rails.png" alt="Tips to Improve the Performance of Ruby on Rails Application" />
</p>

<p>
   In this article, I will show you tips to improve performance of Ruby on Rails application. let’s dive into those.
</p>

<p>
  <strong>1. Limit Amount of Data in a Controller Method</strong><br/>
  Thin controllers are easy to test and has a good performance profile because there’s some overhead involved in passing the controller instance variable around. In short, you need to follow "Thin controller and fat model".
</p>

<p>
  <strong>2. Split View in Separate Partials</strong><br/>
  n this way, views will be easier to read and easier to cache.
</p>

<p>
  <strong>3. Choose Right Session Storage</strong><br/>
  Based on your level of need, choose your session storage carefully. Here are what rails provide:<br/>
  - CookieStore – Stores everything on the client.<br/>
  - DRbStore – Stores the data on a DRb server.<br/>
  - MemCacheStore – Stores the data in a memcache.<br/>
  - ActiveRecordStore – Stores the data in a database using Active Record.
</p>

<p>
  <strong>4. DRY (Don't Repeat Yourself)</strong><br/>
  This is the most common things programmers tend to listen and don’t follow. Here is very basic example:
</p>

{% codeblock lang:ruby %}
if Listing.find_by_id(1).name == "Bambo"
  return Listing.find_by_id(1)
else
  return nil
end
{% endcodeblock %}

<p>
  It should be written by:
</p>

{% codeblock lang:ruby %}
listing = Listing.find_by_id(1)
if listing.name == "Bambp" then listing else nil end
{% endcodeblock %}

<p>
  <strong>4. Eager Loading</strong><br/>
  Eager loading is a way to solve the classic N + 1 query performance problem caused by inefficient use of child objects.
</p>

<p>
  Let’s look at the following code. It will fetch zip of 10 users.
</p>

{% codeblock lang:ruby %}
users = User.all(:limit => 10)

users.each do |user|
  puts user.address.zip
end
{% endcodeblock %}

<p>
  11 queries will be executed, 1 for the top and 10. The solution is to rewrite it to eager load address:
</p>

{% codeblock lang:ruby %}
users = User.includes(:address).limit(10)

users.each do |user|
  puts user.address.zip
end
{% endcodeblock %}

<p>
  <strong>5. Indexing</strong><br/>
  Database indexing is one of the simplest ways to improve database performance. The insert operation will become slower but will boost up fetching data which is more frequently used in web application.
</p>

<p>
  <strong>6. Avoid Dynamism</strong><br/>
  Although find_by and find_all_by dynamic methods are really cool, the are also kind of slow because each one needs to run through method_missing and parse the filename against the list of columns in database table.
</p>

<p>
  <strong>7. Caching</strong><br/>
  This is the purest way to speed up a rails application. Here are a short example of different types of caching:<br/>
  - Page Caching<br/>
  - Action Caching<br/>
  - Fragment Caching<br/>
  - SQL Caching<br/>
  - Asset Caching<br/>
</p>

<p>
  <strong>8. Image Spriting</strong><br/>
  In websites, a significant times are consumed for loading large number of images. One way of minimizing is to sprite your images. This will reduce number of images to be served significantly.
</p>

<p>
  <strong>9. Minify and GZIP Stylesheets and Javascripts</strong><br/>
  This is the last point, but an important one. You can reduce size of the stylesheets and javascripts significantly by Minifying it and serve as GZip format. It will improve the performance significantly by reducing request/response time.
</p>

<p>
  So far so good, these are pretty basic guidelines but surely help you to improve your application. Now, the bounce rate of your site should be less and you are expected to be a happier product owner. :)
</p>