---
layout: post
title: "Translate Your Ruby on Rails Application"
date: 2015-04-15 08:58
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Translate Your Ruby on Rails Application
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Translate Your Ruby on Rails Application" />
</p>

<p>
  If you're planning to take over the world, you'll need to convince everyone to speak your language, or better yet, why don't you offer them a website that speaks their language. The goal is to have a website available in multiple languages, and for the visitors to be able to toggle over to the language they are most comfortable with.
</p>

<p>
  <strong>Routing</strong><br/>
  I like to nest all of my routes inside of a :locale scope. I also limit the locale options to a set that I have predefined in an initializer. You could also choose to deal with locale validity later on in the flow (before_action method), but it is up to you.
</p>

{% codeblock config/routes.rb lang:ruby %}
scope "(:locale)", locale: /#{MySite::ROUTE_LOCALES.keys.join("|")}/ do
  root :to => 'home#index'
  resources :pages
  # etc...
end
{% endcodeblock %}

{% codeblock onfig/initializers/my_site.rb lang:ruby %}
module MySite
end

MySite::LOCALES = {
  en:    "English",
  kh:    "Khmer",
  fr:    "French",
  en_us: "US English",
  en_uk: "UK English",
}

MySite::ROUTE_LOCALES = MySite::LOCALES.keys.each_with_object({}) do |locale, hsh|
  hsh[locale.to_s.tr("_","-")] = locale if locale.to_s.length > 2
end
{% endcodeblock %}

<p>
  This line of code will make sure all of your routes have the current locale in them.
</p>

{% codeblock app/controllers/application_controller.rb lang:ruby %}
def default_url_options(options={})
  { locale: I18n.locale }.merge(options)
end
{% endcodeblock %}

<p>
  <strong>Determining Locale</strong><br/>
  I usually have a before_action filter which does its best to determine the URL of the application. You might have other things in here too if you keep track of the preferred locale in a cookie, or if it is attached to the user's session data or account. You may also want to use the HTTP_ACCEPT_LANGUAGE to determine if there is a match.
</p>

{% codeblock app/controllers/application_controller.rb lang:ruby %}
class ApplicationController < ActionController::Base
  before_action :determine_locale

  protected 

  def determine_locale
    locale = if params.include?(:new_locale) && MySite::ROUTE_LOCALES.keys.include?(params[:new_locale])
      params[:new_locale]
    elsif params.include?(:locale)
      params[:locale]
    else
      locale_from_url(request.host) || I18n.default_locale
    end

    set_locale(locale)
  end

  def locale_from_url(host)
    # ... determine locale from host if you have different domains
    # for different locales
  end

  def set_locale(locale)
    I18n.locale = locale.to_s.gsub("-","_").to_sym
  end
end
{% endcodeblock %}

<p>
  <strong>Static Text</strong><br/>
  The developers should be entering their keys into the yml files located in the locales folder if your rails application. I normally only have one for English, and then use other I18n backends (Redis for example) serve up the other translations. If the translations are in Redis, you will obviously need code that puts them there. I am working on an engine for this called <a href="https://github.com/leighhalliday/idioma" target="_blank">Idioma</a> which persists the translations using ActiveRecord and also to Redis at the same time.
</p>

<p>
  <strong>Dynamic Content</strong><br/>
  Because I18n comes built in to Rails, you won't need to install many gems. But for dynamic content I recommend <a href="https://github.com/globalize/globalize" target="_blank">Globalize</a>. Along with this one is a handy gem called <a href="https://rubygems.org/gems/globalize-accessors" target="_blank">Globalize Accessors</a> which will help you when creating forms to enter this data in.
</p>

{% codeblock lang:ruby %}
class Page < ActiveRecord::Base
  translates :title, :body, fallbacks_for_empty_translations: true
  globalize_accessors locales: MySite::LOCALES.keys, attributes: [:title, :body]
end
{% endcodeblock %}

<p>
  <strong>Dynamic Content</strong><br/>
  One thing I set up are fallbacks... this is so you can translate English once, and only when there is a locale that differs from the default do you need to specifically translate it for that locale. Example, in the US colour is spelled like color.
</p>

{% codeblock config/application.rb lang:ruby %}
config.i18n.default_locale = :en_ca
config.i18n.fallbacks = {
  en_us: :en,
  kh   : :kh,
  en_uk: :en,
  fr_lu: :fr,
}
{% endcodeblock %}

<p>
  <strong>Having Your Website Translated</strong><br/>
  Because the translation team probably isn't the same as the dev team, and they probably don't have access to your code repository nor know how to edit yml files, you will want to have another way of giving them access to the translations. There is an established tool called <a href="https://github.com/tolk/tolk" target="_blank">Tolk</a>.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>