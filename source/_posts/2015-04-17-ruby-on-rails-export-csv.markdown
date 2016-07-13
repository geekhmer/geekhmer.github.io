---
layout: post
title: "Ruby on Rails Export CSV"
date: 2015-04-17 22:25
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Ruby on Rails Export CSV, Ruby on Rails Exporting CSV, Rails Export CSV, 1Rails Exporting CSV
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails Export CSV" />
</p>

<p>
  Sometime you will want to provide a CSV export option for your users. This article will show you how.
</p>

<p>
  The first thing we need to do is open up the config/application.rb file and add:
</p>

{% codeblock config/application.rb lang:ruby %}
require 'csv'
{% endcodeblock %}

<p>
  Next, we need to add some code to the model that will export the data in the CSV format. Add the following code to the model you wish to export to CSV:
</p>

{% codeblock lang:ruby %}
def self.as_csv
  CSV.generate do |csv|
    csv << column_names
    all.each do |item|
      csv << item.attributes.values_at(*column_names)
    end
  end
end
{% endcodeblock %}

<p>
  This code will export both the column headers as well as the data in the csv format and return the result.
</p>

<p>
  Finally, we need to add a bit of extra code to our controller in order to return the CSV data to our user. Assuming your model and controller are named posts, add the following code to your posts controller:
</p>

{% codeblock lang:ruby %}
def index
  @posts = Post.order(:created_at)
  
  respond_to do |format|
    format.html
    format.csv { send_data @posts.as_csv }
  end
end
{% endcodeblock %}

<p>
  Now, if you visit http://localhost:3000/posts.csv, you will be prompted to download the CSV.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
