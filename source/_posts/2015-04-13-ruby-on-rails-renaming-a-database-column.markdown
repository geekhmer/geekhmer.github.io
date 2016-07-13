---
layout: post
title: "Ruby on Rails Renaming a Database Column"
date: 2015-04-13 13:01
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Ruby on Rails Renaming a Database Column
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails Renaming a Database Column" />
</p>

<p>
  To rename a database column, first you need to create a migration:
</p>

<p>
  Terminal commands line:
</p>

{% codeblock lang:ruby %}
rails g migration RenameColumnXinTableYtoZ
{% endcodeblock %}

<p>
  Next you need to edit the migration and add the following line:
</p>

{% codeblock lang:ruby %}
rename_column :table, :old_column, :new_column
{% endcodeblock %}

<p>
  Finally, please run a <code>rake db:migrate</code> and you are all set.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
