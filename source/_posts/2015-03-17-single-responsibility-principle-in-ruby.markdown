---
layout: post
title: "Single Responsibility Principle in Ruby"
date: 2015-03-17 20:55
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Single Responsibility Principle in Ruby
---

<p>
  <img src="/images/rights_and_wrongsof_ruby.jpg" width="400" alt="Single Responsibility Principle in Ruby" />
</p>

<p>
  Assume that we already had the code:
</p>

{% codeblock lang:ruby %}
class AuthenticatesUser
  def authenticate(email, password)
    if matches?(email, password)
     do_some_authentication
    else
      raise NotAllowedError
    end
  end

  private
  def matches?(email, password)
    user = find_from_db(:user, email)
    user.encrypted_password == encrypt(password)
  end
end
{% endcodeblock %}

<p>
  The AuthenticatesUser class is responsible for authenticating the user as well as knowing if the email and password match the ones in the database. It has two responsibilities, and according to the principle it should only have one, let's extract one.
</p>

{% codeblock lang:ruby %}
class AuthenticatesUser
  def authenticate(email, password)
    if MatchesPasswords.new(email, password).matches?
     do_some_authentication
    else
      raise NotAllowedError
    end
  end
end

class MatchesPasswords
  def initialize(email, password)
     @email = email
     @password = password
  end

  def matches?
    user = find_from_db(:user, @email)
    user.encrypted_password == encrypt(@password)
  end
end
{% endcodeblock %}

<p>
  I've used a refactoring technique called "Extract Class" and then use it on the class I already had, this is called sharing behaviour through "Composition".
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>