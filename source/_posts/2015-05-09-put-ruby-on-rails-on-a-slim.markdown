---
layout: post
title: "Put Ruby on Rails on a Slim"
date: 2015-05-09 08:39
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Put Ruby on Rails on a Slim
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="Put Ruby on Rails on a Slim" />
</p>

<p>
  Ruby on Rails is my usual framework of choice to build web applications. It is good and easy to use, but let’s face it: it’s massive compared to more lightweight frameworks, such as Sinatra. Why is it bad? There are a few reasons and slow start-up time and high resources usage are just two most common concerns. Slimming down Ruby on Rails applications can allow you to use different components (say Sequel instead of ActiveRecord) and improve system security. Remember the last year’s security dramas related to JSON parameters parsing? It would be largely irrelevant for most applications if everyone who does not use it, disabled it.
</p>

<p>
  <strong>Rails Modularity</strong><br/>
  To enable individual modules, replace the following in config/application.rb:
</p>

{% codeblock config/application.rb lang:ruby %}
require 'rails/all'
{% endcodeblock %}

<p>
  With:
</p>

{% codeblock config/application.rb lang:ruby %}
require 'active_record/railtie'
require 'action_controller/railtie'
require 'action_mailer/railtie'
require 'active_model/railtie'
require 'sprockets/railtie'
{% endcodeblock %}

<p>
  As you can see above, you can remove ActiveRecord altogether if you use different database, the same with ActionMailer or Sprockets. It probably does not make sense to remove ActionController, however.
</p>

<p>
  <strong>Internal Middleware Stack</strong><br/>
  Each Ruby on Rails applications comes with <a href="http://guides.rubyonrails.org/rails_on_rack.html" target="_blank">Internal Middleware Stack Enabled</a>. Check out <a href="http://guides.rubyonrails.org/rails_on_rack.html#internal-middleware-stack" target="_blank">This Section of Documentation</a> for a list and description of each component. On a fairly small Ruby on Rails application I work on, the stack looks like this:
</p>

{% codeblock lang:ruby %}
$ rake middleware
use Rack::Sendfile
use ActionDispatch::Static
use #<ActiveSupport::Cache::Strategy::LocalCache::Middleware:0x007f0bda2d73e0>
use Rack::Runtime
use Rack::MethodOverride
use ActionDispatch::RequestId
use Rails::Rack::Logger
use ActionDispatch::ShowExceptions
use ActionDispatch::DebugExceptions
use ActionDispatch::RemoteIp
use ActionDispatch::Callbacks
use ActionDispatch::Cookies
use ActionDispatch::Session::CookieStore
use ActionDispatch::Flash
use ActionDispatch::ParamsParser
use Rack::Head
use Rack::ConditionalGet
use Rack::ETag
run Blog::Application.routes
{% endcodeblock %}

<p>
  I can safely disable most of the above by modifying config/application.rb, for example:
</p>

{% codeblock config/application.rb lang:ruby %}
config.middleware.disable "Rack::ETag"
{% endcodeblock %}

<p>
  The same way, I was able to disable most of the default middleware, without doing any harm to my little app. You will have to be careful, however, what you disable. If you use login system, you will need sessions, if you display flash messages do not remove “ActionDispatch::Flash”... etc.
</p>

<p>
  When it comes to JSON parameters parsing, that was a headache for most Ruby on Rails developers last year, it can be easily disabled. To disable JSON params parsing, put this line into config/application.rb:
</p>

{% codeblock config/application.rb lang:ruby %}
config.middleware.swap ActionDispatch::ParamsParser, ActionDispatch::ParamsParser, Mime::JSON => nil
{% endcodeblock %}

<p>
  <strong>Rails API</strong><br/>
  If you are building only API for your application, you can use nice gem called <a href="https://github.com/rails-api/rails-api" target="_blank">rails-api</a>.
</p>

<p>
  <strong>Put Controllers on a Hunger Slim</strong><br/>
  If you check out Ruby on Rails source code for <a href="https://github.com/rails/rails/blob/master/actionpack/lib/action_controller/base.rb" target="_blank">ActionController::Base</a>, you will learn that it inherits from ActionController::Metal and includes <a href="https://github.com/rails/rails/blob/master/actionpack/lib/action_controller/base.rb#L203" target="_blank">Bunch of Modules</a> that you might or might not need in your controllers. A way to slim down your controllers stack, is to cherry-pick the ones you need. If all you need is basic rendering, there is nothing stopping you from creating lightweight controllers like this one:
</p>

{% codeblock lang:ruby %}
class SlimController < ActionController::Metal
  include ActionController::Rendering

  def index
    render text: "I'm Slim, yes I'm the real Bunlong"
  end
end
{% endcodeblock %}

<p>
  There is no ActionController:HipHop I’m afraid, but that will work just as good.
</p>

<p>
  <strong>Single File Rails Applications</strong><br/>
  Okay, they don’t have to be single-file. But you are not forced to use default Rails catalogues structure, and you can make them really slim and custom.
</p>

<p>
  You can, however, create 100% functional Rails application only in config.ru:
</p>

{% codeblock config.ru lang:ruby %}
# Port of https://gist.github.com/josevalim/1942658 to Rails 4
# Original author: Jose Valim
#
# Run this file with:
#
#   bundle exec RAILS_ENV=production rackup -p 3000 -s thin
#
# And access:
#
#   http://localhost:3000/hello/world
#
# We are using Bundler in this example, but we could also
# have used rubygems:
#
#   require "rubygems"
#
#   gem "actionpack"
#   gem "railties"
#
#   require "rails"
#   require "rails/all"

# The following lines should come as no surprise. Except by
# ActionController::Metal, it follows the same structure of
# config/application.rb, config/environment.rb and config.ru
# existing in any Rails 4 app. Here they are simply in one
# file and without the comments.
require "rails"
require "action_controller/railtie" # require more if needed

class MyApp < Rails::Application
  routes.append do
    get "/hello/world" => "hello#world"
  end

  # Enable cache classes. Production style.
  config.cache_classes = true

  # uncomment below to display errors
  # config.consider_all_requests_local = true

  # Here you could remove some middlewares, for example
  # Rack::Lock, ActionDispatch::Flash and  ActionDispatch::BestStandardsSupport below.
  # The remaining stack is printed on rackup (for fun!).
  # Rails API has config.middleware.api_only! to get
  # rid of browser related middleware.
  config.middleware.delete "Rack::Lock"
  config.middleware.delete "ActionDispatch::Flash"
  config.middleware.delete "ActionDispatch::BestStandardsSupport"

  # We need a secret token for session, cookies, etc.
  config.secret_token = "49837489qkuweoiuoqwehisuakshdjksadhaisdy78o34y138974xyqp9rmye8yrpiokeuioqwzyoiuxftoyqiuxrhm3iou1hrzmjk"
end

# This is a barebone controller. One good reference can be found here:
# http://piotrsarnacki.com/2010/12/12/lightweight-controllers-with-rails3/
class HelloController < ActionController::Metal
  include ActionController::Rendering

  def world
    render :text => "Hello world!"
  end
end

# Initialize the app (originally in config/environment.rb)
MyApp.initialize!

# Print the stack for fun!
puts ">> Starting Rails lightweight stack"
Rails.configuration.middleware.each do |middleware|
  puts "use #{middleware.inspect}"
end
puts "run #{Rails.application.class.name}.routes"

# Run it (originally in config.ru)
run MyApp
{% endcodeblock %}

<p>
  <strong>Your Gemfile is Fat</strong><br/>
  Including too many libraries is bad for weight of the app. You need to check your Gemfile, and make sure you do require gems like ‘pry’ only in :development group, ‘rspec’ in :test etc.
</p>

<p>
  There is general tendency of Rails developers of putting together applications together with ready-to-use blocks. You need to consider if that is what you really need. Writing simple authentication is a matter of controller and two actions, you might not need Devise for that. Handling simple file uploads can be almost as easily implemented as with use of Carrierwave, with much less boilerplate code. I do encourage you to build minimal solutions for your needs only, especially when you do not need to use majority of functionality provided by given gem. If there is a hole found in, say, Carrierwave, including the gem might make your application vulnerable too, and heavier than it needs to be, of course.
</p>

{% codeblock Gemfile lang:ruby %}
source 'https://rubygems.org'

gem 'rails', '~> 4.2.1'

gem 'psych', '~> 2.0.12'
gem 'builder'
gem 'dynamic_form'
gem 'aws-sdk-core'
gem 'gchartrb', require: 'google_chart'
gem 'gravtastic'
gem 'high_voltage'
gem 'highline'
gem 'honeybadger', '~> 1.0'
gem 'jquery-rails'
gem 'mail'
gem 'dalli'
gem 'multi_json'
gem 'paul_revere'
gem 'pg'
gem 'rack'
gem 'rdoc', '~> 3.12.2'
gem 'redis'
gem 'rest-client', require: 'rest_client'
gem 'statsd-instrument', '~> 2.0.6'
gem 'unicorn'
gem 'validates_formatting_of'
gem 'will_paginate'
gem 'xml-simple'
gem 'yajl-ruby', require: 'yajl'
gem 'autoprefixer-rails'
gem 'clearance'
gem 'daemons'
gem 'delayed_job'
gem 'delayed_job_active_record'

gem 'newrelic_rpm'
gem 'newrelic-redis'

gem 'sass-rails',   '~> 5.0.0'
gem 'coffee-rails', '~> 4.0.0'
gem 'uglifier', '>= 1.0.3'

group :development do
  gem 'capistrano', '~> 2.0'
  gem 'capistrano-notification'
  gem 'rails-erd'
end

group :test do
  gem 'minitest', require: false
  gem 'capybara'
  gem 'factory_girl_rails'
  gem 'launchy'
  gem 'rack-test', require: 'rack/test'
  gem 'rr', require: false
  gem 'shoulda', require: false
  gem 'timecop'
end

group :recovery do
  gem "fakeredis"
end

platforms :jruby do
  gem 'jruby-openssl'
end
{% endcodeblock %}

<p>
  So far so good, That' it!!! See ya!!! :)
</p>