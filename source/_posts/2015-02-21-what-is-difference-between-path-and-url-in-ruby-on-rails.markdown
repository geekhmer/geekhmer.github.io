---
layout: post
title: "What is Difference Between Path and URL in Ruby on Rails?"
date: 2015-02-21 11:23
comments: true
categories: [Ruby on Rails, Ruby]
keywords: What is Difference Between Path and URL in Ruby on Rails?
---

<p>
  <img src="/images/move_to_rails.png" alt="What is Difference Between Path and URL in Ruby on Rails?" />
</p>

<p>
  Have you ever wondered what is difference between <code>_path</code> and <code>_url</code> in Ruby on Rails?
</p>

<p>
  For example <code>root_path</code> and <code>root_url</code>. As it turns out, <code>_path</code> only returns the url relative to your domain. 
</p>

<p>
  For instance, <code>root_path</code> returns <code>/</code> while <code>root_url</code> returns <code>http://mydomain.com/</code>.
</p>

<p>
  You should always use <code>_url</code> for redirects and <code>_path</code> for hyperlinks unless you have a good reason not to do so.
</p>

<p>
  So far so good, That's it! See ya! :)
</p>
