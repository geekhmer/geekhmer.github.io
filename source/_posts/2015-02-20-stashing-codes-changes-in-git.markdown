---
layout: post
title: "Stashing Codes Changes in Git"
date: 2015-02-20 19:53
comments: true
categories: [Other]
keywords: Stashing Codes Changes in Git, Stashing Your Changes in Git
---

<p>
  <img src="/images/logo_git.png" alt="Stashing Codes Changes in Git" />
</p>

<p>
  Using version control, is best to commit your code in small, discrete chunks rather than making large commits. However, What happens when you are working on a large change, and your boss comes to you and tells you they need an urgent bug fixed? With the git stash command you can quickly and easily store your code away and recall it for later use. Let’s run through this with me.
</p>

<p>
  <strong>Stash the Code</strong>
</p>

{% codeblock lang:ruby %}
git stash
{% endcodeblock %}

<p>
  <strong>Reapply the Changes You Sent to the Stash Type</strong><br/>
  What if you want to reapply your changes? Use <code>git stash apply</code> to reapply all the changes you stashed away with git stash.
</p>

{% codeblock lang:ruby %}
git stash apply
{% endcodeblock %}

<p>
  <strong>List All of the Stashes You've Made Type</strong><br/>
  Git stash supports stashing multiple times. To see a list of all the code you've stashed, use <code>git stash list</code>. The git stash list will show you a list of all the stashes you've made.
</p>

{% codeblock lang:ruby %}
git stash list
{% endcodeblock %}

<p>
  <strong>Stash Apply From the Earlier List</strong><br/>
  If the first stash is named stash@{1}, you can type <code>git stash apply stash@{1}</code> to apply the changes from that stash.
</p>

{% codeblock lang:ruby %}
git stash apply stash@{1}
{% endcodeblock %}

<p>
  So far so good, That's it! See ya! :)
</p>