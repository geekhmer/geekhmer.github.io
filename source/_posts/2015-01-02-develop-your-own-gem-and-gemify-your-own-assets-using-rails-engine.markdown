---
layout: post
title: "Develop Your Own Gem and Gemify Your Own Assets Using Rails Engine"
date: 2015-01-02 23:32
comments: true
categories: 
---

<p>
  <img src="/images/rubygems_logo.png" alt="Develop Your Own Gem and Gemify Your Own Assets Using Rails Engine" />
</p>

<p>
  The Rails asset pipeline, powered by sprockets, compiles (sass, coffeescript, others), aggregates (combines multiple source files into one file for performance purposes), and post-processes (minimization, gzip’ing) your assets. And which make it easy to include versioned external assets as application dependencies as well.
</p>

<p>
  External assets are made available in Rails via Rails engines. When the engine is loaded into your Rails application, the engine's asset paths are added to your application's load paths. This makes them available for require in your manifest files. An asset gem is just an absurdly simple engine.
</p>

<p>
  You will find almost any JS or CSS library you want, already Gemified, but, if it is not the case, you can Gemify those libraries by your own, and I can help you with it. So, let's do it!
</p>

<p>
  <strong>Create a bare-bones Gem:</strong><br/>
  Bundler makes it simple to create the files and directories necessary for creating a gem. Run the following command to create and initialize a Git repository along with several template files for the gem:
</p>

{% codeblock lang:ruby %}
bundle gem timeago-rails
{% endcodeblock %}

<p>
  This command will create basically the following tree:
</p>

{% codeblock lang:ruby %}
├── Gemfile
├── lib
│   ├── timeago
│   │   └── rails
│   │           └── version.rb
│   └── rails.rb
├── LICENSE.txt
├── Rakefile
├── README.md
└── timeago-rails.gemspec
{% endcodeblock %}

<p>
  <strong>Versioning</strong><br/>
  timeago-rails is a gem packaged version of the timeago.js library. Its version should track the version of JavaScript library. Open /lib/timeago/rails/version.rb and set the version:
</p>

{% codeblock version.rb lang:ruby %}
module Timeago
  module Rails
    VERSION = "1.4.1"
  end
end
{% endcodeblock %}

<p>
  <strong>Turn the Gem into an Engine</strong><br/>
  Bundler created the gem as a standard Ruby module, but we want it to be a Rails Engine.
</p>

{% codeblock rails.rb lang:ruby %}
require "timeago/rails/version"

module Timeago
  module Rails
    class Engine < ::Rails::Engine
    end
  end
end
{% endcodeblock %}

<p>
  Well, the module is empty. All we're doing here is declaring the gem as a Rails Engine. This will cause Rails to add its directories to the load path when the Gem is required.
</p>

<p>
  <strong>Add the Assets (Javascript library, CSS, Image) in the Gem</strong><br/>
  We're going to create the directory /vendor/images/, /vendor/javascripts/, vendor/stylesheets/ and place the source for the timeago.js plugin there:
</p>

{% codeblock lang:ruby %}
├── Gemfile
├── lib
│   ├── timeago
│   │   └── rails
│   │           └── version.rb
│   └── rails.rb
├── LICENSE.txt
├── Rakefile
├── README.md
├── timeago-rails.gemspec
└── vendor
         └── assets
                  ├── images
                  ├── javascripts                
                  │             └── timeago.js
                  └── stylesheets
{% endcodeblock %}

<p>
  <strong>Test</strong><br/>
  Moving to a sample Rails application, we can include the gem in our host application by adding it to the Gemfile using the path option:
</p>

{% codeblock Gemfile lang:ruby %}
gem "timeago-rails", path: "../timeago-rails"
{% endcodeblock %}

<p>
  Since we included an asset that needs to be included in the Rails assets, we have to take one more step and instruct the user to add the following to their app/assets/javascripts/application.js file:
</p>

{% codeblock application.js lang:ruby %}
//= require timeago-rails
{% endcodeblock %}

<p>
  This directive actually refers to the app/assets/javascripts/timeago.js file we included in our gem.
</p>

<p>
  Type command below to make sure timeago.js is included in sample Rails application:
</p>

{% codeblock lang:ruby %}
curl http://localhost:3000/assets/timeago.js
{% endcodeblock %}

<p>
  The curl command should return the contents of the timeago.js file if everything is correctly.
</p>

<p>
  <strong>README.md</strong><br/>
  Make a simple readme file with the Gem as documentation.
</p>

<p>
  <strong>Push to GitHub & RubyGems</strong><br/>
  Create a GitHub repository for the Gem, stage all of your commits, commit, and push the code to GitHub.<br/>
  If you've never published a gem on RubyGems before, you'll need to sign up for an account there. Your account settings will contain an API key that should be copied to ~/.gem/credentials.
</p>

<p>
  Publishing your gem is as simple as:
</p>

{% codeblock lang:ruby %}
rake release
{% endcodeblock %}

<p>
  So far so good, hope you enjoyed the post. see ya! :)
</p>