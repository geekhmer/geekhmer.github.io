---
layout: post
title: "How to set Facebook Page Tab content auto resize height?"
date: 2013-12-17 16:02
comments: true
categories: [Facebook]
keywords: facebook, javascript, facebook fan page, how to develop facebook page tab?, how to set facebook page tab content auto resize height?
---

<p>
  The height of Facebook Page Tab is fixed at 800px, so to set Facebook Page Tab content auto resize height you must add code below at the bottom of body tag.
</p>

<p>
  <a class="fancybox" href="/images/page_tab_not_yet_resize.png"><img src="/images/page_tab_not_yet_resize.png" width="680" /></a>
</p>

{% codeblock code for auto resize height lang:html %}
<div id="fb-root"></div>
<script type="text/javascript" src="http://connect.facebook.net/de_DE/all.js1">
</script>
<script type="text/javascript">
  window.fbAsyncInit = function() {
    FB.init({
      appId : 'APP_ID',
      cookie : true
    });
  }

  window.onload = function() {
    FB.Canvas.setAutoGrow(10);
  }
</script>
{% endcodeblock %}

<p>
  Enjoy it
</p>

<p>
  <a class="fancybox" href="/images/page_tab_resize_already.png"><img src="/images/page_tab_resize_already.png" width="680" /></a>
</p>