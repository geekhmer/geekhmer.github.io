---
layout: post
title: "Rails Server Command Line Options"
date: 2015-07-11 22:19
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Rails Server Command Line Options
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="Rails Server Command Line Options" />
</p>

<p>
  Syntax:
</p>

{% codeblock lang:ruby %}
rails [options]
{% endcodeblock %}

<p>
  Options:<br/>
  -s, –config-script=path Uses the specified mongrel config script.<br/>
  -p, –port=port Runs Rails on the specified port. Default: 3000<br/>
  -b, –binding=ip Binds Rails to the specified ip. Default: 0.0.0.0<br/>
  -e, –environment=name Specifies the environment to run this server under (test/development/production). Default: development
</p>

<p>
  <strong>Listen on any Interface</strong><br/>
  By default versions of Rails is localhost, this prevents users on your local network from accessing your network. You may not want to do this however. You may wish to share your development site with coworker so that they can review the site. Otherwise, you may wish to test the site on other devices such as a mobile device. Fortunately you can easily open the rails server up to all interfaces using the -b argument. Simple run the rails s command below:
</p>

{% codeblock lang:ruby %}
rails s -b 0.0.0.0
{% endcodeblock %}

<p>
  <strong>Use a Different Port</strong><br/>
  Sometimes you want to use a port other than 3000 for your Rails server. For instance, maybe you need to run multiple Rails servers. You can easily do this with the -p argument:
</p>

{% codeblock lang:ruby %}
rails s -p 3001
{% endcodeblock %}

<p>
  This command line tells rails to start the server on port 3001 instead of 3000.
</p>

<p>
  <strong>Run in a Different Environment</strong><br/>
  You can start a rails server for another environment such as production or staging by using the -e argument along with the name of the environment you wish to start up in:
</p>

{% codeblock lang:ruby %}
rails s -e production
{% endcodeblock %}

<p>
  The code above starts the Rails server in the production environment. Very handy when you have custom environments or need to debug something.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
