---
layout: post
title: "Implement Decorator Design Pattern in Rails with Draper to Cleanup Views"
date: 2014-07-26 21:29
comments: true
categories: [Ruby, Ruby on Rails]
keywords: decorator pattern, decorator design pattern, draper, implement decorator design pattern in rails with draper to cleanup views
---

<p>
  Ruby on Rails is an awesome framework to build web application. Rails relies on the MVC pattern, and lots of conventions, code becomes complex to maintain, productivity start to decrease, etc.
</p>

<p>
  We can find a lot of reasons to this(too much logic in controllers, fat models, lack of services layers, etc.), but I will just name one: logic in view, logic in view is bug prone, difficult to test, and can be a pain for front-end developers.
</p>

<p>
  Sample problem
</p>

{% codeblock articles_controller.rb lang:ruby %}
class ArticlesController < ApplicationController
  def show
    @article = Article.find(params[:id])
  end
end
{% endcodeblock %}

{% codeblock show.html.haml lang:ruby %}
- if can? :update, @article
  = link_to "Edit", edit_article_path(@article)
{% endcodeblock %}

<p>
  Well, you got it, logic in view. And since rails don't have view inheritance, this code will have to be copy-pasted over and over, I would rather have something like: <code>= @article.link_to_edit</code>
</p>

<p>
  So, how can we remove logic from the view?
</p>

<p>
  <strong>Decorator Design Pattern save the views</strong><br/>
  As usual, we can find a solution with a pattern. the decorator design pattern will help us remove logic from views in our Rails applications.<br/>
  The idea is to insert an object between the model and the view, well there is a good ruby gem is <a href="https://github.com/drapergem/draper" target="_blank">Draper</a> that work well.
</p>

<p>
  Let create a decorator for the Article class
</p>

{% codeblock article_decorator.rb lang:ruby %}
class ArticleDecorator < Draper::Decorator
  def link_to_edit
    if h.can? :update, object
      h.link_to "Edit", h.edit_article_path(object)
    end
  end
end
{% endcodeblock %}

{% codeblock articles_controller.rb lang:ruby %}
class ArticlesController < ApplicationController
  def show
    @article = ArticleDecorator.new(Article.find(params[:id]))
  end
end
{% endcodeblock %}

{% codeblock show.html.haml lang:ruby %}
= @article.link_to_edit
{% endcodeblock %}

<p>
  A few things to notice here. From the controller, we call the decorate method on a instance of article, this will return a decorated version of the article object.
</p>

<p>
  In the ArticleDecorator class, we have access to the decorated object with the object method and to all view helpers through the h object.<br/>
  We can now call the link_to_edit method on the article object from our view, and the decorator will take care of the logic, and return a formatted link if the user has the right edit this resource, and nothing if he does not.
</p>

<p>
  The code is easier to test, we just instantiate a new instance of the ArticleDecorator, and check the output of the link_to_edit method. No need to test whole renderd view, searching through the DOM object tree.
</p>

<p>
  So far so good, we gain code readablility, code reuse, better testing, and in addition, we even lost a few lines of duplicated code. :)
</p>