---
layout: post
title: "Using the Page Visibility API for Detecting Your Page is Being Viewed by the End User or Not"
date: 2015-07-13 22:51
comments: true
categories: [Javascript]
keywords: Using the Page Visibility API for Detecting Your Page is Being Viewed by the End User or Not, Page Visibility API, The Page Visibility API
---

<p>
  <img src="/images/page_visibility_api.jpg" width="400" alt="Using the Page Visibility API for Detecting Your Page is Being Viewed by the End User or Not" />
</p>

<p>
  HTML5 have many new features. One such feature is the new visibility API. This API allows you to easily detect your page is active, and in addition, you can perform specific events if it's active. In this article we will show you how implement this feature in your application.
</p>

<p>
  <strong>Visibility API</strong><br/>
  Without worrying about browser compatibility for the moment. There is an event called <code>visibilitychange</code> that gets fired every time window focus changes. In addition, there is a property called <code>document.hidden</code> that will tell your page is visible or not.
</p>

<p>
  For example, the following script will change the document title based on your page is active or not:
</p>

{% codeblock lang:ruby %}
document.addEventListener("visibilitychange", function(event){
  if(document.hidden) {
    document.title = "My Page - Inactive";
  }
  else {
    document.title = "My Page - Active";
  }
});
{% endcodeblock %}

<p>
  Some browsers still require prefixing, such as the Android 4.4 browser. For these browsers, a bit more code is required.
</p>

<p>
  We need some code to detect whether a prefixed version of the property. The code below does exactly that. It loops through each vendor prefix and sees if it is present in the document object. If so, it returns the correct prefixed version:
</p>

{% codeblock lang:ruby %}
function getHiddenProperty() {
  var prefixes = ['webkit','moz','ms','o'];

  if ('hidden' in document) return 'hidden';

  for(var prefix = 0; prefix < prefixes.length; prefixes++) {
    if ((prefixes[prefix] + "Hidden") in document) {
      return prefixes[i] + 'Hidden';
    }
  }
}
{% endcodeblock %}

<p>
  Next, we create a simple wrapper function that returns true if hidden and false if not:
</p>

{% codeblock lang:ruby %}
function isHidden() {
  return document[getHiddenProperty()]
}
{% endcodeblock %}

<p>
  Well now we can write a third function to get the name of the event to use by using the visibility change event:
</p>

{% codeblock lang:ruby %}
function getVisibilityChangeEvent() {
  var visProp = getHiddenProperty();

  if ( visProp ) {
    var eventName = visProp.replace(/[H|h]idden/,'') + 'visibilitychange';
    return eventName;
  }
}
{% endcodeblock %}

<p>
  Now can modify our original function to use the functions:
</p>

{% codeblock lang:ruby %}
document.addEventListener(getVisibilityChangeEvent(), function(event){
  if(isHidden()) {
    document.title = "My Page - Inactive";
  }
  else {
    document.title = "My Page - Active";
  }
});
{% endcodeblock %}

<p>
  <strong>The Complete Codes.</strong>
</p>

{% codeblock lang:ruby %}
function getHiddenProperty() {
  var prefixes = ['webkit','moz','ms','o'];

  if ('hidden' in document) return 'hidden';

  for(var prefix = 0; prefix < prefixes.length; prefixes++) {
    if((prefixes[prefix] + "Hidden") in document ) {
      return prefixes[i] + 'Hidden';
    }
  }
}

function isHidden() {
  return document[getHiddenProperty()];
}

function getVisibilityChangeEvent() {
  var visProp = getHiddenProperty();

  if(visProp) {
    var eventName = visProp.replace(/[H|h]idden/,'') + 'visibilitychange';
    return eventName;
  }
}

document.addEventListener(getVisibilityChangeEvent(), function(event){
  if(isHidden()) {
    document.title = "My Page - Inactive";
  }
  else {
    document.title = "My Page - Active";
  }
});
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
