---
layout: post
title: "Fast Rails API"
date: 2015-05-06 10:41
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Fast Rails API, Rails API, Ruby on Rails API
---

<p>
  <img src="/images/fast_rails_api.png" alt="Fast Rails API" />
</p>

<p>
  Rendering JSON is pretty easy in Rails.
</p>

{% codeblock lang:ruby %}
render json: @statuses
{% endcodeblock %}

<p>
  It works well if there are small number of records to be returned. But what happens when we need to return 10,000 records at once? Things slow down dramatically and the most time-consuming parts are JSON serialization and database operations.
</p>

<p>
  <strong>Include only Required Attributes</strong><br/>
  The first obvious thing is generating JSON with only attributes that we need.
</p>

{% codeblock lang:ruby %}
render json: @statuses, methods: [:latitude, :longitude, :timestamp, :virtual_odometer]
{% endcodeblock %}

<p>
  Tidying JSON gives over 20% performance
</p>

{% codeblock lang:ruby %}
default    5.940000   0.080000   6.020000 (  6.094221)
attrs      4.820000   0.010000   4.830000 (  4.932337)
{% endcodeblock %}

<p>
  <strong>Select only Required Columns</strong><br/>
  Second, we should consider selecting only required columns when we don’t need all of them.
</p>

{% codeblock lang:ruby %}
render json: @statuses.select([:latitude, :longitude, :timestamp, :virtual_odometer])
{% endcodeblock %}

<p>
  It’ll help us to avoid transferring a huge amount of data to the application from the database and gives 2x speed up.
</p>

{% codeblock lang:ruby %}
default    5.940000   0.080000   6.020000 (  6.094221)
attrs      4.820000   0.010000   4.830000 (  4.932337)
select     2.170000   0.020000   2.190000 (  2.222277)
{% endcodeblock %}

<p>
  <strong>Don’t Instantiate ActiveRecord Objects If Possible</strong><br/>
  Let’s implement a method to return “lightning” array of hashes instead of ActiveRecord instances.
</p>

{% codeblock lang:ruby %}
def self.lightning
  connection.select_all(select([:latitude, :longitude, :timestamp, :virtual_odometer]).arel).each do |attrs|
    attrs.each_key do |attr|
      attrs[attr] = type_cast_attribute(attr, attrs)
    end
  end
end
{% endcodeblock %}

<p>
  Returns array of hashes instead of array of single column values. Invoke a new method in controller.
</p>

{% codeblock lang:ruby %}
render json: @statuses.lightning
{% endcodeblock %}

<p>
  Using lightweight hashes makes JSON rendering 2x faster.
</p>

{% codeblock lang:ruby %}
default    5.940000   0.080000   6.020000 (  6.094221)
attrs      4.820000   0.010000   4.830000 (  4.932337)
select     2.170000   0.020000   2.190000 (  2.222277)
lightning  1.120000   0.010000   1.130000 (  1.148763)
{% endcodeblock %}

<h3>
  Fastest JSON
</h3>

<p>
  There are several JSON libraries available:<br/>
  - <a href="https://github.com/flori/json" target="_blank">JSON</a> - The default JSON gem with C-extensions (ships with Ruby 1.9).<br/>
  - <a href="https://github.com/brianmario/yajl-ruby" target="_blank">YAJL</a> - Yet Another JSON Library.<br/>
  - <a href="https://github.com/ohler55/oj" target="_blank">OJ</a> - Optimized JSON.
</p>

<p>
  It’s a good idea to use the fastest dumper of them.
</p>

{% codeblock lang:ruby %}
json       0.810000   0.020000   0.830000 (  0.841307)
yajl       0.760000   0.020000   0.780000 (  0.809903)
oj         0.640000   0.010000   0.650000 (  0.666230)
{% endcodeblock %}

<p>
  But we prefer <a href="https://github.com/rails-api/active_model_serializers" target="_blank">active_model_serializers</a> which it run faster than OJ.
</p>

<p>
  Summarized benchmark results are:
</p>

{% codeblock lang:ruby %}
user     system      total        real
default    5.940000   0.080000   6.020000 (  6.094221)
attrs      4.820000   0.010000   4.830000 (  4.932337)
select     2.170000   0.020000   2.190000 (  2.222277)
lightning  1.120000   0.010000   1.130000 (  1.148763)
json       0.810000   0.020000   0.830000 (  0.841307)
yajl       0.760000   0.020000   0.780000 (  0.809903)
oj         0.640000   0.010000   0.650000 (  0.666230)
ams        0.270000   0.000000   0.270000 (  0.272239)
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
