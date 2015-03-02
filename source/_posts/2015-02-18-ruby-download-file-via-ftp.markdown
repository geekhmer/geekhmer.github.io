---
layout: post
title: "Ruby Download File via FTP"
date: 2015-02-18 23:24
comments: true
categories: [Ruby]
keywords: Ruby Download File via FTP
---

<p>
  <img src="/images/rights_and_wrongsof_ruby.jpg" width="400" alt="Ruby Download File via FTP" />
</p>

<p>
  Download files from FTP server is quite easy using Ruby.
</p>

<p>
  Example:
</p>

{% codeblock lang:ruby %}
require 'net/ftp'

ftp = Net::FTP.new
ftp.connect("your_host_name.com", 21)
ftp.login("your_username","your_password")
ftp.chdir("/your_directory")
ftp.passive = true
ftp.getbinaryfile("your_source_file", "your_dest_file")
{% endcodeblock %}

<p>
  So far so good, That's it! See ya! :)
</p>