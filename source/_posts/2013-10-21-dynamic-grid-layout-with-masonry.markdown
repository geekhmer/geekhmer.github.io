---
layout: post
title: "Dynamic Grid Layout with Masonry"
date: 2013-10-21 00:35
comments: true
categories: [Layout]
keywords: layout,grid layout,dynamic grid layout,masonry,dynamic grid layout with masonry
description: dynamic grid layout with masonry
---

<!-- **Content start here** -->
<p>
  <strong>What is Masonry?</strong><br/>
  Masonry is a JavaScript grid layout library. It works by placing elements in optimal position based on available vertical space.
</p>
<p>
  <strong>Add JQuery & Masonry script</strong><br/>
  Add JQuery and Masonry script in head tag.
</p>
{% codeblock Add JQuery & Masonary script lang:html %}
<script type="text/javascript" src="jquery-1.9.1.js"></script>
<script type="text/javascript" src="masonry.pkgd.js"></script>
{% endcodeblock %}
<p>
  <strong>Create Item</strong><br/>
  All items are wrapped in a container.
</p>
{% codeblock Create Item lang:html %}
<div id="container">
  <div class="box-item">Item1</div>
  <div class="box-item">Item2</div>
  <div class="box-item">Item3</div>
</div>
{% endcodeblock %}
<p>
  <strong>Create Item CSS style</strong><br/>
  Item style should be floated.
</p>
{% codeblock Create Item CSS style lang:html %}
.box-item {
  width: 220px;
  height: 220px;
  margin: 10px;
  float: left;
}
{% endcodeblock %}
<p>
  <strong>Run Masonry</strong><br/>
  The script to run Masonry.
</p>
{% codeblock Masongry script lang:html %}
$(function(){
  $('#container').masonry({
    itemSelector: '.box-item'
  });
});
{% endcodeblock %}
<p>
  <strong>Full Code</strong><br/>
</p>
{% codeblock Full Code lang:html %}
<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <title></title>
  <style type="text/css">
    .box-item {
      width: 220px;
      padding: 10px;
      margin: 5px;
      float: left;
      border: 1px solid #CCC;
      border-radius: 4px;
      -moz-border-radius: 4px;
    }
  </style>
  <script type="text/javascript" src="jquery-1.9.1.js"></script>
  <script type="text/javascript" src="masonry.pkgd.js"></script>
  <script type="text/javascript">
    $(function(){
      $('#container').masonry({
        itemSelector: '.box-item'
      });
    });
  </script>
</head>
<body>
  <div id="container">
    <div class="box-item">
      Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Donec odio. Quisque volutpat mattis eros. Nullam malesuada erat ut turpis. Suspendisse urna nibh, viverra non, semper suscipit, posuere a, pede.
    </div>
    <div class="box-item">
      Morbi purus libero, faucibus adipiscing, commodo quis, gravida id, est. Sed lectus. Praesent elementum hendrerit tortor. Sed semper lorem at felis.
      Sed ac risus. Phasellus lacinia, magna a ullamcorper laoreet, lectus arcu pulvinar risus, vitae facilisis libero dolor a purus. Sed vel lacus. Mauris nibh felis, adipiscing varius, adipiscing in, lacinia vel, tellus. Suspendisse ac urna. Etiam pellentesque mauris ut lectus. Nunc tellus ante, mattis eget, gravida vitae, ultricies ac, leo. Integer leo pede, ornare a, lacinia eu, vulputate vel, nisl.
    </div>
    <div class="box-item">
      Sit amet mi ullamcorper vehicula Ut convallis, sem sit amet interdum consectetuer, odio augue aliquam leo, nec dapibus tortor nibh sed augue. Sed vel lacus. Mauris nibh felis, adipiscing varius, adipiscing in, lacinia vel, tellus. Suspendisse ac urna. Etiam pellentesque mauris ut lectus. Nunc tellus ante, mattis eget, gravida vitae, ultricies ac, leo. Integer leo pede, ornare a, lacinia eu, vulputate vel, nisl.
    </div>
    <div class="box-item">
      usce accumsan mollis eros. Pellentesque a diam sit amet mi ullamcorper vehicula
      Morbi interdum mollis sapien. Sed ac risus. Phasellus lacinia, magna a ullamcorper laoreet, lectus arcu pulvinar risus, vitae facilisis libero dolor a purus.
    </div>
    <div class="box-item">
      Ut convallis, sem sit amet interdum consectetuer, odio augue aliquam leo, nec dapibus tortor nibh sed augue.
      Ut condimentum mi vel tellus. Suspendisse laoreet. Fusce ut est sed dolor gravida convallis. Morbi vitae ante. Vivamus ultrices luctus nunc. Suspendisse et dolor. Etiam dignissim. Proin malesuada adipiscing lacus. Donec metus. Curabitur gravida.
    </div>
    <div class="box-item">
      Ut condimentum mi vel tellus. Suspendisse laoreet. Fusce ut est sed dolor gravida convallis. Morbi vitae ante. Vivamus ultrices luctus nunc. Suspendisse et dolor.
      Phasellus pede arcu, dapibus eu, fermentum et, dapibus sed, urna.
    </div>
    <div class="box-item">
      Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Donec odio. Quisque volutpat mattis eros. Nullam malesuada erat ut turpis. Suspendisse urna nibh, viverra non, semper suscipit, posuere a, pede.
    </div>
    <div class="box-item">
      Morbi purus libero, faucibus adipiscing, commodo quis, gravida id, est. Sed lectus. Praesent elementum hendrerit tortor. Sed semper lorem at felis.
      Sed ac risus. Phasellus lacinia, magna a ullamcorper laoreet, lectus arcu pulvinar risus, vitae facilisis libero dolor a purus. Sed vel lacus. Mauris nibh felis, adipiscing varius, adipiscing in, lacinia vel, tellus. Suspendisse ac urna. Etiam pellentesque mauris ut lectus. Nunc tellus ante, mattis eget, gravida vitae, ultricies ac, leo. Integer leo pede, ornare a, lacinia eu, vulputate vel, nisl.
    </div>
    <div class="box-item">
      Sit amet mi ullamcorper vehicula Ut convallis, sem sit amet interdum consectetuer, odio augue aliquam leo, nec dapibus tortor nibh sed augue. Sed vel lacus. Mauris nibh felis, adipiscing varius, adipiscing in, lacinia vel, tellus. Suspendisse ac urna. Etiam pellentesque mauris ut lectus. Nunc tellus ante, mattis eget, gravida vitae, ultricies ac, leo. Integer leo pede, ornare a, lacinia eu, vulputate vel, nisl.
    </div>
    <div class="box-item">
      usce accumsan mollis eros. Pellentesque a diam sit amet mi ullamcorper vehicula
      Morbi interdum mollis sapien. Sed ac risus. Phasellus lacinia, magna a ullamcorper laoreet, lectus arcu pulvinar risus, vitae facilisis libero dolor a purus.
    </div>
    <div class="box-item">
      Ut convallis, sem sit amet interdum consectetuer, odio augue aliquam leo, nec dapibus tortor nibh sed augue.
      Ut condimentum mi vel tellus. Suspendisse laoreet. Fusce ut est sed dolor gravida convallis. Morbi vitae ante. Vivamus ultrices luctus nunc. Suspendisse et dolor. Etiam dignissim. Proin malesuada adipiscing lacus. Donec metus. Curabitur gravida.
    </div>
    <div class="box-item">
      Ut condimentum mi vel tellus. Suspendisse laoreet. Fusce ut est sed dolor gravida convallis. Morbi vitae ante. Vivamus ultrices luctus nunc. Suspendisse et dolor.
      Phasellus pede arcu, dapibus eu, fermentum et, dapibus sed, urna.
    </div>
  </div>
</body>
</html>
{% endcodeblock %}
<p>
  You can download the <a href="https://github.com/Bunlong/masonry" target="_blank">source code</a> and try it out.
</p>