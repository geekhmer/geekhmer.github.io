---
layout: post
title: "Import CSS Files into Nextjs"
date: 2018-04-01 21:10
comments: true
categories: [Next.js]
keywords: Import CSS Files into Nextjs, React, Reactjs, Next, Nextjs, Next.js
---

<p>
  <img src="/images/nextjs.png" width="500" alt="Import CSS Files into Nextjs" />
</p>

<p>
  The way to import css files into Nextjs is very simple:
</p>

<p>
  1. Create a <code>/static</code> folder at the same level of <code>/pages</code> folder.<br/>
  2. In <code>/static</code> folder put your <code>.css</code> files.<br/>
  3. In your pages components import Head and add a CSS <link />.
</p>

{% codeblock lang:ruby %}
import Head from 'next/head'

export default () => (
  <div>
    <Head>
      <title>My styles pages</title>
      <link href="/statics/styles.css" rel="stylesheet" />
    </Head>
    <p className="some-class-name">
      Welcome to my styles pages!
    </p>
  </div>
)
{% endcodeblock %}

<p>
  This way Nextjs render the link tag in the head of the page and the browser will download the CSS and applied it.
</p>

<p>
  So far so good, That’s it!!! See ya!!! :)
</p>
