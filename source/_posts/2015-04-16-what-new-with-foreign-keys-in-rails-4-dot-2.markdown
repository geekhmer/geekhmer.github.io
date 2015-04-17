---
layout: post
title: "What New with Foreign Keys in Rails 4.2?"
date: 2015-04-16 15:43
comments: true
categories: [Ruby, Ruby on Rails]
keywords: What New with Foreign Keys in Rails 4.2?, What New with Foreign Keys in Ruby on Rails 4.2?, Foreign Keys in Rails 4.2, Foreign Keys in Ruby on Rails 4.2?
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="What New with Foreign Keys in Rails 4.2?" />
</p>

<p>
  Previous versions of Rails did not have the ability to create foreign keys without resorting to executing SQL statements directly. This lead to messy, database dependent solutions that weren't very nice. 
</p>

<p>
  Luckily, Rails 4.2 rectifies this with new features to add and remove foreign keys. In this article we will discuss what foreign keys are, and how to add them to your database.
</p>

<p>
  If you don't know what a foreign key is, well, quite simply, a foreign key links one table to another's primary key. For example, if I have an authors table and a books table, I could create a foreign key that points from books back to authors, linking the two tables together. Then, these two tables become linked. It means that I cannot add or update the record with invalid data for the author_id field in the books table.  I can also tell the database server what to do when I update or delete the record. For example, I can tell rails to automatically delete child records when the parent is deleted (delete an author from the authors table and all the author's books are deleted). I could also tell Rails to set the author_id column to null or to simply not let me delete the author while child records exist.
</p>

<p>
  <strong>Add Foreign Keys</strong><br/>
  To add a foreign key to the books table:
</p>

{% codeblock lang:ruby %}
add_foreign_key :books, :authors
{% endcodeblock %}

<p>
  We can also give the foreign key a custom name:
</p>

{% codeblock lang:ruby %}
add_foreign_key :books, :authors, name: :my_foreign_key
{% endcodeblock %}

<p>
  If we are using a non Rails friendly database, we can specify the column names we wish to use for the foreign key. For example, let's say the primary key of the authors table is author_id and the key in the books table is auid. We could simply use the following code to create the foreign key:
</p>

{% codeblock lang:ruby %}
add_foreign_key :books, :authors, column: :auid, primary_key: :author_id
{% endcodeblock %}

<p>
  We can control the behavior of the foreign key as well. A list of the available behaviors are below. Independent behaviors can be set for both update as well as delete.<br/>
  - :restrict (default) - Prevents changes from being made.<br/>
  - :nullify - Sets the child columns to null.<br/>
  - :cascade - Cascades down to the child records. For instance, deleting an author deletes the author's books.
</p>

<p>
  To specify behaviors, you simply use the <code>on_delete</code> or <code>on_update</code> parameters. For example, the following code would cascade deletes so that the books get deleted when the parent gets deleted.
</p>

{% codeblock lang:ruby %}
add_foreign_key :books, :authors, on_delete: cascade
{% endcodeblock %}

<p>
  <strong>Delete Foreign Keys</strong><br/>
  It is just as easy to delete foreign keys:
</p>

{% codeblock lang:ruby %}
remove_foreign_key :books, :authors
{% endcodeblock %}

<p>
  You can optionally specify the name of the foreign key to delete:
</p>

{% codeblock lang:ruby %}
remove_foreign_key :books, name: :my_foreign_key
{% endcodeblock %}

<p>
  If you have non Rails friendly column names, you can specify the column name that contains the foreign key you wish to remove:
</p>

{% codeblock lang:ruby %}
remove_foreign_key :books, column: :auid
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
