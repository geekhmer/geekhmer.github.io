---
layout: post
title: "Facebook signed request lose session in Ruby on Rails"
date: 2013-09-28 22:03
comments: true
categories: [Ruby, Facebook]
keywords: facebook,facebook-signed-request, facebook signed request, ruby,ruby on rails,rails,facebook signed request lose session in ruby on rails
description: session in your Ruby on Rails application lose after a user goes to a facebook tab app then facebook tab app request to your server and your server receives a signed request
---

<p>
  <strong>Question: </strong> Does the session in your Ruby on Rails application lose after a user goes to a facebook tab app then facebook tab app request to your server and your server receives a signed request?<br/>
  <strong>Answer: </strong> This can be happening because of the facebook app doesn’t send a csrf token in it’s token.
</p>

<p>
  You can fix this by add <strong>skip_before_filter :verify_authenticity_token</strong> to the controller that facebook app’s request to or remove <strong>protect_from_forgery</strong> from application_controller.rb
</p>