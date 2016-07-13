---
layout: post
title: "Hash Data in Ruby"
date: 2015-04-18 17:25
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Hash Data in Ruby
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Hash Data in Ruby" />
</p>

<p>
  Somethings we need to utilize hashing algorithms in Ruby and/or Rails. Examples of this include verifying file integrity, hashing passwords, and much more. In this brief article we will show you how to hash data in various formats.
</p>

<p>
  <strong>Calculate an MD5 Hash</strong><br/>
  The venerable MD5 algorithm is an older and simpler hashing algorithm. Please note that MD5 is considered insecure due to the fact it can be brute forced relatively quickly, so you should use this algorithm with extreme care. To use MD5, simply use the following code:
</p>

{% codeblock lang:ruby %}
hash = Digest::MD5.hexdigest("this is a test") # => "54b0c58c7ce9f2a8b551351102ee0938"
{% endcodeblock %}

<p>
  <strong>Calculate a SHA-1 Hash</strong><br/>
  SHA-1 hashes are much more secure. SHA-1 can be used for passwords and file hashing, but it's recommended that you move to SHA-2 (SHA256, SHA384, and SHA512) as soon as you can as they are even more secure. SHA-1 is also an excellent choice for maintaining file integrity.
</p>

{% codeblock lang:ruby %}
hash = Digest::SHA1.hexdigest("this is a test") # => "fa26be19de6bff93f70bc2308434e4a440bbad02"
{% endcodeblock %}

<p>
  <strong>Calculate a SHA-2 hash: SHA256, SHA384, and SHA512</strong><br/>
  HA-2 includes a number of different digest lengths, with SHA-512 in theory being the most secure. Calculating these hashes is just as easy.
</p>

{% codeblock lang:ruby %}
Digest::SHA256.hexdigest("this is a test") # => "2e99758548972a8e8822ad47fa1017ff72f06f3ff6a016851f45c398732bc50c"

hash = Digest::SHA384.hexdigest("this is a test") # => "43382a8cc650904675c9d62d785786e368f3a99db99aeaaa7b76b02530677154d09c0b6bd2e21b4329fd41543b9a785b"

hash = Digest::SHA512.hexdigest("this is a test") # => "7d0a8468ed220400c0b8e6f335baa7e070ce880a37e2ac5995b9a97b809026de626da636ac7365249bb974c719edf543b52ed286646f437dc7f810cc2068375c"
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
