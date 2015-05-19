---
layout: post
title: "Awesome jQuery Tricks"
date: 2015-05-13 12:30
comments: true
categories: Javascript
keywords: Awesome jQuery Tricks
---

<p>
  <img src="/images/jquery_logo.png" width="500" alt="Awesome jQuery Tricks" />
</p>

<p>
  As we all know, jQuery is an awesome library and has transformed the internet as we know it. In this article we will cover 5 extremely useful jQuery tricks that you can use in your sites. Let's get started.
</p>

<p>
  <strong>Gracefully Degrade Broken Images</strong><br/>
  Broken images can be a pain to deal with. They degrade the user experience and can be difficult to find without scanning your site regularly with a 3rd party tool. Luckily it's easy to substitute broken images with an image that you specify using jQuery. the <code>error</code> callback of an image gets fired any time an image can't be successfully loaded by the browser. For example:
</p>

{% codeblock lang:javascript %}
$('img').error(function(){
  $(this).attr('src', 'images/missing.png’);
});
{% endcodeblock %}

<p>
  The code above would replace any broken images with an image in the images folder called missing.png. You could also do something like make an AJAX request to your server to log the location of the broken image.
</p>

<p>
  <strong>Wait For an Image to Be Loaded</strong><br/>
  Sometimes you need to wait for an image to be loaded before continuing to process events. This is most often useful with libraries like the <a href="http://masonry.desandro.com/" target="_blank">jQuery Masonry library</a>, where images can affect the size of the overall content. Fortunately this is easy with the load callback. For example:
</p>

{% codeblock lang:javascript %}
$('img').load(function() {
  // Do stuff here...
});
{% endcodeblock %}

<p>
  The above code would wait for the image to be loaded and then execute the code specified in the callback.
</p>

<p>
  <strong>Zebra Stripe a Table</strong><br/>
  Sometimes we want our tables to alternating odd/even colors. While this is possible using CSS3, older browsers don't support most CSS3 features. Using this script allows our tables to be striped across all browsers.
</p>

{% codeblock lang:javascript %}
$('table tr:even').css('background', '#f7f7f7'); // Strip every even row
$('table tr:odd').css('background', '#f7f7f7'); // Stripe every odd row
$('table > tbody > tr:even').css('background', '#f7f7f7'); // Only stripe the table body
{% endcodeblock %}

<p>
  The above code will do striping as noted in the comments.
</p>

<p>
  <strong>Preloading Images</strong><br/>
  If your page uses a lot of images that aren't visible initially, it might be worth it to preload them. This simple script listed below does exactly that.
</p>

{% codeblock lang:javascript %}
$.preloadImages = function() {
  for (var i = 0; i < arguments.length; i++) {
    $("<img />").attr("src", arguments[i]);
  }
}

$.preloadImages("images/myimage1.jpg","images/myimage2.jpg");
{% endcodeblock %}

<p>
  <strong>Detect Mobile Devices</strong><br/>
  Sometimes we need a 'check all' checkbox for our web pages that selects every checkbox in a fieldset. The code below lets you easily accomplish this.
</p>

{% codeblock lang:javascript %}
$('.checkall').click(function () {
  $(this).parents('fieldset:eq(0)').find(':checkbox').attr('checked', this.checked);
});
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
