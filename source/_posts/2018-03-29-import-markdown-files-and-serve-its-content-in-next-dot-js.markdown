---
layout: post
title: "Import Markdown Files and Serve Its Content in Next.js"
date: 2018-03-29 22:26
comments: true
categories: [Other]
keywords: Import Markdown Files and Serve Its Content in Next.js, React, Reactjs, Next, Nextjs, Next.js
---

<p>
  <img src="/images/nextjs.png" width="600" alt="Import Markdown Files in Next.js" />
</p>

<p>
  As <a href="https://github.com/arunoda" target="_blank">@arunoda</a> (Next.js founder) said Next.js does not support importing markdown files yet. But you can configure the Next.js webpack loaders to load <code>raw-loader</code> modules and import markdown files and return them as strings.
</p>

<p>
  Let get started!
</p>

<p>
  Open the terminal, run the command below to install <code>raw-loader</code> and <code>react-markdown</code> modules (noted: use react-markdown to renders markdown as pure React components):
</p>

{% codeblock lang:ruby %}
npm install --save raw-loader

npm install --save react-markdown
{% endcodeblock %}

<p>
  Create <code>next.config.js</code> file with content below:
</p>

{% codeblock next.config.js lang:ruby %}
module.exports = {
  webpack: (config) => {
    config.module.rules.push(
      {
        test: /\.md$/,
        use: 'raw-loader'
      }
    )

    return config
  },
}
{% endcodeblock %}

<p>
  Create <code>docs/about.md</code> file with content below:
</p>

{% codeblock about.md lang:ruby %}
# About

Welcome to **GeeKhmer**!
{% endcodeblock %}

<p>
  Create <code>pages/about.js</code> file with content below:
</p>

{% codeblock about.js lang:ruby %}
import React from 'react'
import ReactMarkdown from 'react-markdown'

export default class extends React.Component {
  static async getInitialProps({ req }) {
    const content = await require(`../docs/about.md`)
    return { content }
  }

  render() {
    return (
      <div>
        <ReactMarkdown source={this.props.content} />
      </div>
    )
  }
}
{% endcodeblock %}

<p>
  So far so good, That’s it!!! See ya!!! :)
</p>
