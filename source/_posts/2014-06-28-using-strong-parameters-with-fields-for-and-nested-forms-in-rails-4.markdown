---
layout: post
title: "Using Strong Parameters With Fields For &amp; Nested Forms in Rails 4"
date: 2014-06-28 11:48
comments: true
categories: [Ruby on Rails]

keywords: Using Strong Parameters With Fields For &amp; Nested Forms in Rails 4, Rails
description: Using Strong Parameters With Fields For &amp; Nested Forms in Rails 4
---

<p>
  With strong_parameters becoming the standard way of handling security in Rails 4, I played around with it. It works great except the documentation isn’t clear on how to handle nested forms inside Rails, specifically with the accepts_nested_attributes_for in the model and fields_for in views.<br/>
  So far so good, let take a look a short example below.
</p>

{% codeblock account.rb lang:ruby %}
class Account < ActiveRecord::Base
  has_many :people
  accepts_nested_attributes_for :people
end
{% endcodeblock %}

{% codeblock person.rb lang:ruby %}
class Person < ActiveRecord::Base
  belongs_to :account
end
{% endcodeblock %}

{% codeblock accounts_controller.rb lang:ruby %}
class AccountsController < ApplicationController
  def new
    @account = Account.new
    @account.people.build
  end

  def create
    @account = Account.new(new_account_params)
    if @account.save
      respond_to do |format|
        format.html {redirect_to root_path, notice: "Account created successfully."}
      end
    end
  end

  private
  def new_account_params
    params.require(:account).permit(:id, :name, people_attributes: [:id, :email, :password, :password_confirmation])
  end
end
{% endcodeblock %}