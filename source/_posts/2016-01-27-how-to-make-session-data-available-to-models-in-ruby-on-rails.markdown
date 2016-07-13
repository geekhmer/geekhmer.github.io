---
layout: post
title: "How to Make Session Data Available to Models in Ruby on Rails?"
date: 2016-01-27 11:11
comments: true
categories: [Ruby on Rails]
keywords: How to Make Session Data Available to Models in Ruby on Rails?
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="How to Make Session Data Available to Models in Ruby on Rails?" />
</p>

<p>
  Ruby on Rails is implemented as the Model View Controller (MVC) pattern. This pattern separates the context of the Web Application (in the Controller and the View) from the core Model of the application. The Model contains the Domain objects which encapsulate business logic, data retrieval, etc. The View displays information to the user and allows them to provide input to the application. The Controller handles the interactions between the View and the Model.
</p>

<p>
  This separation is a very good design principle that generally helps prevent <a href="https://en.wikipedia.org/wiki/Spaghetti_code" target="_blank">spaghetti code</a>. Sometimes though the separation might break down.
</p>

<p>
  Rails provides the Active Record Callbacks which allows you to write code that will respond to the lifecycle events of the Model objects. For example you could log information every time a specific kind of Model object is saved. For example you could record some information every time an Account changed using the following:
</p>

{% codeblock account.rb lang:ruby %}
class Account < ActiveRecord::Base
  after_update :log_audit_change

  private
  def log_audit_change
    Audit.audit_change(self.id, self.new_balance)
  end
end
{% endcodeblock %}

<p>
  You might have noticed a limitation with the previous API though. You didn't notice? The only information passed to the model is the Object that is being changed. What if you want more context than this? For example, what if you want to audit not only the values that changed them, but the user who made the change?
</p>

{% codeblock account.rb lang:ruby %}
class Account < ActiveRecord::Base
  after_update :log_audit_change

  private
  def log_audit_change
    Audit.audit_change(current_user, self.id, self.new_balance)
  end
end
{% endcodeblock %}

<p>
  How do you get the <code>current_user</code> value? Well, you have to plan ahead a little bit. The User in this application is stored in the HTTP Session when the user is authenticated. The session isn't directly available to the Model level so you have to figure out a way around this. One way to accomplish this is by using a named <a href="http://ruby-doc.org/core-2.3.0/Thread.html#M000484" target="_blank">Thread local variable</a>. Each HTTP request is served by its own thread. That means that a variable stored as thread local will be available for the entire processing of a request.
</p>

<p>
  The UserInfo module encapsulates reading and writing the User object from/to the Thread local. This module can then be mixed in with other objects for easy access.
</p>

{% codeblock user_info.rb lang:ruby %}
module UserInfo
  def current_user
    Thread.current[:user]
  end

  def self.current_user=(user)
    Thread.current[:user] = user
  end
end
{% endcodeblock %}

<p>
  A <code>before_filter</code> set in the ApplicationController will be called before any action is called in any controller. You can take advantage of this to copy a value out of the HTTP session and set it in the Thread local:
</p>

{% codeblock user_info.rb lang:ruby %}
class ApplicationController < ActionController::Base
  before_filter :set_user

  protected
  def authenticate
    unless session[:user]
      redirect_to :controller => "login"
      return false
    end
  end

  # Sets the current user into a named Thread location so that it can be accessed by models and observers
  def set_user
    UserInfo.current_user = session[:user]
  end
end
{% endcodeblock %}

<p>
  At any point in a Model class that you need to have access to those values you can just mixin the helper module and then use its methods to access the data. In this final example we mixin the UserInfo module to our model and it will now have access to the <code>current_user</code> method:
</p>

{% codeblock account.rb lang:ruby %}
class Account < ActiveRecord::Base
  include UserInfo

  after_update :log_audit_change

  private
  def log_audit_change
    Audit.audit_change(current_user, self.id, self.new_balance)
  end
end
{% endcodeblock %}

<p>
  You generally shouldn't need this kind of trick outside of a model. In most cases the Controller should pass all of the information needed by a Model object to it through its methods. That will allow the Model objects to interact and the Controller to do the orchestration needed. But in a few special cases, this trick might be handy.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
