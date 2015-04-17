---
layout: post
title: "Ruby on Rails _filter vs _action"
date: 2015-04-16 09:13
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Ruby on Rails _filter vs _action, _filter vs _action, _filter, _action
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails _filter vs _action" />
</p>

<p>
  If you are coming to Rails 4 from earlier versions of Rails, you may have noticed the new <code>_action</code> methods floating around.
</p>

<p>
  Seeing these for the first time may have left you scratching your head a bit and asking "Aren't these the same as the <code>_filter</code> functions in previous versions of Rails?" The answer is a resounding YES. For Rails 4.x, the rails team decided to rename the functions to better describe what they are doing. Don't worry, the old functions still work. However it is recommended that you use the new syntax going forward, as the <code>_filter</code> functions may be deprecated in the future.
</p>

<p>
  Below shows a list of the old and new functions:
</p>

Old Function | New Function
--- | ---
before_filter | Abefore_action
after_filter | after_action
around_filter | around_action
append_after_filter | append_after_action
append_around_filter | append_around_actionor DOM classes.
prepend_around_filter | prepend_around_action
append_before_filter | append_before_action
skip_before_filter | skip_before_action
skip_around_filter | skip_around_action
skip_after_filter | skip_after_action
skip_filter | skip_action_callback

<br/>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
