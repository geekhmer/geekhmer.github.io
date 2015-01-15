---
layout: post
title: "Install Emacs Erlang Editor/IDE"
date: 2015-01-14 23:17
comments: true
categories: [Erlang]
keywords: Install Emacs Erlang Editor/IDE, Installing Emacs Erlang Editor/IDE, Install Emacs Erlang Editor, Installing Emacs Erlang Editor, Install Erlang Editor/IDE Emacs, Installing Erlang Editor/IDE Emacs, Erlang Editor
---

<p>
  <img src="/images/logo_erlang.png" alt="Install Emacs Erlang Editor/IDE" />
</p>

<p>
  Currently I personally use Emacs for programming in Erlang. There is an Erlang editing mode in Emacs. Well, I can help you to install Emacs and set Erlang mode in Emacs. So, let's do it!
</p>

<p>
  <strong>Install Emacs</strong><br/>
  Run commands below to install Emacs:
</p>

{% codeblock lang:ruby %}
sudo apt-add-repository ppa:cassou/emacs
sudo apt-get install emacs24 emacs24-el emacs24-common-non-dfsg
{% endcodeblock %}

<p>
  <strong>Set Erlang Mode in Emacs</strong><br/>
  To set Erlang mode in Emacs add the following codes to file ~/.emacs:
</p>

{% codeblock .emacs lang:ruby %}
(defun my-erlang-mode-hook ()
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  (imenu-add-to-menubar "imenu")
)
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
{% endcodeblock %}

<p>
  So far so good, hope the article could helped you. see ya! :)
</p>
