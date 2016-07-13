---
layout: post
title: "How to uninstall RVM?"
date: 2015-04-12 15:05
comments: true
categories: [Ruby, Ruby on Rails]
keywords: How to uninstall RVM?
---

<p>
  <img src="/images/rights_and_wrongsof_ruby.jpg" width="400" alt="How to uninstall RVM?" />
</p>

<p>
  There are times when you may need to completely remove RVM. For example, lets say that your installation is corrupt, or you don't wish to use RVM anymore. Luckily this is easy to do. The first thing we need to do is to tell RVM to remove itself. This can be accomplished with the rvm impode command. Simply type:
</p>

{% codeblock lang:ruby %}
rvm implode
{% endcodeblock %}

<p>
  Once you do that, RVM should remove itself. The next thing you need to do is uninstall the 'rvm' gem. Simply type:
</p>

{% codeblock lang:ruby %}
gem uninstall rvm
{% endcodeblock %}

<p>
  This will uninstall the RVM gem.
</p>

<p>
  The next thing you need to do is check to make sure that RVM is removed from your path. You need to check files like .bashrc, .bash_profile, and .profile to make sure all traces of the path are removed.
</p>

<p>
  The final thing you need to do is make sure both the .rvm and .rvmrc files have been removed. simply type:
</p>

{% codeblock lang:ruby %}
ls -a ~
{% endcodeblock %}

<p>
  This will list all of the files in your home directory. Type the following lines to remove the .rvm and .rvmrc files if they exist:
</p>

{% codeblock lang:ruby %}
rm -rf .rvm
rm -rf .rvmrc
{% endcodeblock %}

<p>
  So far so good, if you no longer have a Ruby installation on your system you may also want to remove the .gem folder if it exists. Once you are finished, make sure to log out/back into your system for all changes to take effect. That's it!!! See ya!!!
</p>
