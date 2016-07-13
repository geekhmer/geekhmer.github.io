---
layout: post
title: "Rails 5 timestamps will be changed"
date: 2015-03-31 19:52
comments: true
categories: [Ruby on Rails]
keywords: Rails 5 timestamps will be changed
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Rails 5 timestamps will be changed" />
</p>

<p>
  If you are running Rails 4.2, you will notice the generated migration add a default option to timestamps:
</p>

{% codeblock lang:ruby %}
class CreateFoods < ActiveRecord::Migration
  def change
    create_table :products do |t|

      t.timestamps null: false
    end
  end
end
{% endcodeblock %}

<p>
  Without the <code>null: false</code> it will emit a warning:
</p>

{% codeblock lang:ruby %}
'#timestamp' was called without specifying an option for `null`. In Rails 5, this behavior will
 change to `null: false`. You should manually specify 'null: true' to prevent the behavior of 
 your existing migrations from changing.
{% endcodeblock %}

<p>
  This null option is:
</p>

<p>
  :null - allows or disallows NULL values in the column. This option could have been named :null_allowed.
</p>

<p>
  <code>null: false</code> means you cannot give NULL values for created_at and updated_at on Rails 5.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>