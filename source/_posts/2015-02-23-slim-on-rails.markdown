---
layout: post
title: "Slim on Rails"
date: 2015-02-23 00:12
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Slim on Rails, Ruby on Rails Slim, Rails Slim
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Slim on Rails" />
</p>

<p>
  The first thing you need to do is add a couple gems to the Gemfile. The slim gem tells Rails how to parse the Slim syntax. The slim-rails gem overrides the Rails generators for ERB and replaces them with Slim generators. Add these gems to your Gemfile as below:
</p>

{% codeblock Gemfile lang:ruby %}
gem 'slim', '~> 2.0.3'
gem 'slim-rails', '~> 2.1.5'
{% endcodeblock %}

<p>
  Then run <code>bundle install</code> to install the gems:
</p>

{% codeblock lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  Then generate a Homes controller with a show action by running the command below:
</p>

{% codeblock lang:ruby %}
rails g controller Homes show
{% endcodeblock %}

<p>
  Notice that the generated view ends in .slim, Rails has generated a slim file for us, courtesy of the slim-rails gem. Now let's modify our routes file to set up a resource and add a root path. Open up your routes file (<code>config/routes.rb</code>) and modify it so that it looks somethings like:
</p>

{% codeblock routes.rb lang:ruby %}
root to: "homes#show"
resource :home, only: [:show]
{% endcodeblock %}

<p>
  Now you are ready to play with Slim. Open up the show view for homes (<code>app/views/homes/show.html.slim</code>) and modify it so that it looks somethings like:
</p>

{% codeblock show.html.slim lang:ruby %}
javascript: 
  alert("HELLO!!!!!")
h1 Welcome to my site
<b>inline html is supported</b>
p
  | The time is #{ Time.now }, 
    I Like to program in Ruby on Rails.
#hashtag I threw a hashtag in here!
#classy I'm classy!

p = Random.rand(10)

- if Random.rand(6) == 1
  |Lucky number 1!  You win!
br
br
small Goodbye!
{% endcodeblock %}

<p>
  Which translates to:
</p>

{% codeblock lang:ruby %}
<script type="text/javascript">
  alert("HELLO!!!!!")
</script>

<h1>Welcome to my site</h1>
<b>inline html is supported</b>
<p>The time is 2015-02-22 00:24:59 - 0400, I Like to program in Ruby on Rails.</p>

<div id="hashtag">
  I threw a hashtag in here!
</div>

<div id="classy">
  I'm classy!
</div>

<p>2</p>
<br>
<br>
<small>Goodbye!</small>
{% endcodeblock %}

<p>
  So far so good, That's it! See ya! :)
</p>
