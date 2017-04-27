---
layout: post
title: "Copy to Clipboard without Flash with Clipboard.js"
date: 2017-03-19 23:34
comments: true
categories: [Javascript]
keywords: Copy to Clipboard without Flash with Clipboard.js
---

<p>
  <img src="/images/clipboardjs.png" width="400" alt="Copy to Clipboard without Flash with Clipboard.js" />
</p>

<p>
  Well, in this article I gonna show you how to easily implement the ability to copy data to the <a href="https://clipboardjs.com/" target="_blank">clipboard</a>. In order to implement it I will clipboard.js javascript library, let get started with me:
</p>

<h3>Installation</h3>

<p>
  The first thing I need to do is include the clipboard.js library file in head tag of the application: 
</p>

{% codeblock lang:ruby %}
<script src="https://cdn.jsdelivr.net/clipboard.js/1.5.12/clipboard.min.js"></script>
{% endcodeblock %}

<p>
  If you don't wish to use a CDN, you can download the clipboard in other ways:<br/>
  - Using npm, by running <code>npm install clipboard --save</code><br/>
  - Using bower, by running <code>bower install clipboard --save</code><br/>
  - By downloading a zip file from the <a href="https://github.com/zenorocha/clipboard.js/archive/master.zip" target="_blank">clipboard.js github page</a> and referencing it in your HTML.
</p>

<h3>Usage</h3>

<p>
  <strong>Copy text from another element:</strong> A pretty common use case is to copy content from another element. You can do that by adding a <code>data-clipboard-target</code> attribute in your trigger element. The value you include on this attribute needs to match another's element selector.
</p>

{% codeblock lang:ruby %}
<!-- Target -->
<input id="foo" value="https://github.com/zenorocha/clipboard.js.git">

<!-- Trigger -->
<button class="btn" data-clipboard-target="#foo">
    <img src="assets/clippy.svg" alt="Copy to clipboard">
</button>
{% endcodeblock %}

<p>
  <strong>Cut text from another element:</strong> Additionally, you can define a <code>data-clipboard-action</code> attribute to specify if you want to either <code>copy</code> or <code>cut</code> content. If you omit this attribute, <code>copy</code> will be used by default.
</p>

{% codeblock lang:ruby %}
<!-- Target -->
<textarea id="bar">Mussum ipsum cacilds...</textarea>

<!-- Trigger -->
<button class="btn" data-clipboard-action="cut" data-clipboard-target="#bar">
    Cut to clipboard
</button>
{% endcodeblock %}

<p>
  As you may expect, the cut action only works on input or textarea elements.
</p>

<p>
  <strong>Copy text from attribute:</strong> Truth is, you don't even need another element to copy its content from. You can just include a <code>data-clipboard-text</code> attribute in your trigger element.
</p>

{% codeblock lang:ruby %}
<!-- Trigger -->
<button class="btn" data-clipboard-text="Just because you can doesn't mean you should — clipboard.js">
    Copy to clipboard
</button>
{% endcodeblock %}

<h3>Events</h3>

<p>
  There are cases where you'd like to show some user feedback or capture what has been selected after a copy/cut operation.
</p> 

<p>
  That's why we fire custom events such as success and error for you to listen and implement your custom logic.
</p>

{% codeblock lang:ruby %}
var clipboard = new Clipboard('.btn');

clipboard.on('success', function(e) {
  console.info('Action:', e.action);
  console.info('Text:', e.text);
  console.info('Trigger:', e.trigger);

  e.clearSelection();
});

clipboard.on('error', function(e) {
  console.error('Action:', e.action);
  console.error('Trigger:', e.trigger);
});
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
