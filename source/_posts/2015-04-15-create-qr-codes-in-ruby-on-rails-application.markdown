---
layout: post
title: "Create QR Codes in Ruby on Rails Application"
date: 2015-04-15 11:34
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Create QR Codes in Ruby on Rails Application
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Create QR Codes in Ruby on Rails Application" />
</p>

<p>
  <a href="https://rubygems.org/gems/barby" target="_blank">Barby</a> is a great gem for generating a barcode or QR code. You can choose to output it as any number of barcode types or as a QR code. This example will use a QR code but I have successfully used the Code128 barcode which is fairly common in the retail space.
</p>

<p>
  Add Barby to your gem file:
</p>

{% codeblock Gemfile lang:ruby %}
gem 'barby',  '~> 0.6.2'
gem 'rqrcode','~> 0.4.2'
{% endcodeblock %}

<p>
  Here is an example helper for generating the QR code as base64 encoded png data:
</p>

{% codeblock lang:ruby %}
def generate_qr(text)
  require 'barby'
  require 'barby/barcode'
  require 'barby/barcode/qr_code'
  require 'barby/outputter/png_outputter'

  barcode = Barby::QrCode.new(text, level: :q, size: 5)
  base64_output = Base64.encode64(barcode.to_png({ xdim: 5 }))
  "data:image/png;base64,#{base64_output}"
end
{% endcodeblock %}

<p>
  And an example call from your view:
</p>

{% codeblock lang:ruby %}
%img{src: generate_qr("http://geekhmer.github.io/"), class: "qr-code"}
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>