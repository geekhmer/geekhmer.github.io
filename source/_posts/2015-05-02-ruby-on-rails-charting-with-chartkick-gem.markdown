---
layout: post
title: "Ruby on Rails Charting with Chartkick Gem"
date: 2015-05-02 20:30
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Ruby on Rails Charting with Chartkick Gem
---

<p>
  <img src="/images/chartkick.png" alt="Ruby on Rails Charting with Chartkick Gem" />
</p>

<p>
  Chartkick is a charting library for Ruby on Rails that allows you to easily create nice looking charts. Chartkick is compatible with all major browsers and can easily be used to give your Ruby on Rails application some additional flair. In this article we will cover the basics of using Chartkick.
</p>

<p>
  <strong>Rails Application Setup</strong><br/>
  Before we can use Chartkick, we will need to add it to our Gemfile. Open up your Gemfile and add the following code.
</p>

{% codeblock Gemfile lang:ruby %}
gem 'chartkick', '~> 1.2.4'
{% endcodeblock %}

<p>
  In addition, let's include a couple of helper gems that will make life easier when dealing with data. Add the following gems to your Gemfile.
</p>

{% codeblock Gemfile lang:ruby %}
gem 'groupdate', '~> 2.1.1'
gem 'active_median', '~> 0.1.0'
{% endcodeblock %}

<p>
  Now let's run a bundle install to install all three gems.
</p>

{% codeblock lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  Now we need to create a model that we will use with our charts. For this example app we will create a simple model called Visit. This model will represent a single user's 'visit' to a website. Run the command below to create this model now.
</p>

{% codeblock lang:ruby %}
rails g model Visit country:string visited_at:datetime load_time:decimal
{% endcodeblock %}

<p>
  Now let's run a rake db:migrate to create the table associated with this model.
</p>

{% codeblock lang:ruby %}
rake db:migrate
{% endcodeblock %}

<p>
  Now we need to add some data to our seeds file in order to give us some data to play with. Open up your seeds.rb file and add in the code listed below.
</p>

{% codeblock db/seeds.rb lang:ruby %}
Visit.delete_all
Visit.create  country: 'United States', visited_at: DateTime.now, load_time: 3.5
Visit.create  country: 'United States', visited_at: DateTime.now, load_time: 1.5
Visit.create  country: 'United States', visited_at: DateTime.now, load_time: 1.0
Visit.create  country: 'United States', visited_at: DateTime.now - 1.day, load_time: 4.5
Visit.create  country: 'United States', visited_at: DateTime.now - 1.day, load_time: 4.0
Visit.create  country: 'United States', visited_at: DateTime.now - 2.days, load_time: 3.5
Visit.create  country: 'United States', visited_at: DateTime.now - 2.days, load_time: 1.0
Visit.create  country: 'United States', visited_at: DateTime.now - 2.days, load_time: 3.5
Visit.create  country: 'United States', visited_at: DateTime.now - 3.days, load_time: 4.5
Visit.create  country: 'United States', visited_at: DateTime.now - 3.days, load_time: 3.0
Visit.create  country: 'Germany', visited_at: DateTime.now, load_time: 1.0
Visit.create  country: 'Germany', visited_at: DateTime.now, load_time: 2.0
Visit.create  country: 'Germany', visited_at: DateTime.now, load_time: 1.0
Visit.create  country: 'Germany', visited_at: DateTime.now, load_time: 3.0
Visit.create  country: 'Germany', visited_at: DateTime.now - 1.day, load_time: 4.0
Visit.create  country: 'Germany', visited_at: DateTime.now - 2.days, load_time: 2.0
Visit.create  country: 'Germany', visited_at: DateTime.now - 2.days, load_time: 1.0
Visit.create  country: 'Germany', visited_at: DateTime.now - 2.days, load_time: 3.0
Visit.create  country: 'Germany', visited_at: DateTime.now - 3.days, load_time: 3.5
Visit.create  country: 'South Korea', visited_at: DateTime.now, load_time: 2.0
Visit.create  country: 'South Korea', visited_at: DateTime.now, load_time: 2.5
Visit.create  country: 'South Korea', visited_at: DateTime.now, load_time: 1.0
Visit.create  country: 'South Korea', visited_at: DateTime.now, load_time: 1.5
Visit.create  country: 'South Korea', visited_at: DateTime.now - 1.day, load_time: 2.5
Visit.create  country: 'South Korea', visited_at: DateTime.now - 1.day, load_time: 4.0
Visit.create  country: 'South Korea', visited_at: DateTime.now - 1.day, load_time: 3.0
Visit.create  country: 'South Korea', visited_at: DateTime.now - 2.days, load_time: 1.0
Visit.create  country: 'South Korea', visited_at: DateTime.now - 3.days, load_time: 5.0
Visit.create  country: 'South Korea', visited_at: DateTime.now - 3.days, load_time: 4.0
Visit.create  country: 'South Korea', visited_at: DateTime.now - 3.days, load_time: 5.0
{% endcodeblock %}

<p>
  Now let's run a rake db:seed to seed our database.
</p>

{% codeblock lang:ruby %}
rake db:seed
{% endcodeblock %}

<p>
  Now let's create a controller to give us a place to play around. Run the commands below to create the Homes controller.
</p>

{% codeblock lang:ruby %}
rails g controller homes show
{% endcodeblock %}

<p>
  Now open up your routes file and add in the route listed below.
</p>

{% codeblock lang:ruby %}
Rails.application.routes.draw do
  root to: "homes#show"
end
{% endcodeblock %}

<p>
  Next, open up your homes controller and modify it so that it looks like the code listed below.
</p>

{% codeblock app/controllers/homes_controller.rb lang:ruby %}
class HomesController < ApplicationController
  def show
    @visits = Visit.all
  end
end
{% endcodeblock %}

<p>
  Finally, we need to modify our application layout. Open up your application's layout and modify it so that it looks like the following code. Note the inclusion of the Google API javascript file below. Chartkick can use either Google Charts or Highcharts for charting. In this example we use Google Charts. We also utilize Bootstrap for a cleaner look and feel.
</p>

{% codeblock app/views/layouts/application.rb lang:ruby %}
<!DOCTYPE html>
<html>
<head>
  <title>ChartKickExample</title>
  
  <%= stylesheet_link_tag    'application', media: 'all' %>
  <%= javascript_include_tag 'application' %>
  <%= javascript_include_tag "http://www.google.com/jsapi", "chartkick" %>
  <%= stylesheet_link_tag 'http://yandex.st/bootstrap/3.1.1/css/bootstrap.min.css' %>
  <%= javascript_include_tag 'http://yandex.st/bootstrap/3.1.1/js/bootstrap.min.js' %>
  
  <%= csrf_meta_tags %>
</head>
<body>
  <div class="container">
    <%= yield %>
  </div>
</body>
</html>
{% endcodeblock %}

<p>
  <strong>Line Charts</strong><br/>
  The first chart type we will work with is the line charts Line charts are handy for doing things like plotting out events over time. Open up your show view for the Homes controller and modify it so that it looks like the code listed below.
</p>

{% codeblock app/views/homes/show.html.erb lang:ruby %}
<div class="row">
  
  <!-- Line Chart - Single Series -->
  <div class="col-xs-6">
    <h3>Visits By Day</h3>
    <%= line_chart @visits.group_by_day(:visited_at, format: "%B %d, %Y").count, discrete: true %>
  </div>

</div>
{% endcodeblock %}

<p>
  Now if you start a <code>rails s</code> and navigate to http://localhost:3000 you will see a chart being rendered showing the number of vists by day.
</p>

<p>
  Great, now lets make a slightly more complex line chart. Open up the show view again and modify it so that it looks like the code listed below.
</p>

{% codeblock app/views/homes/show.html.erb lang:ruby %}
<div class="row">

  <!-- Line Chart - Single Series -->
  <div class="col-xs-6">
    <h3>Visits By Day</h3>
    <%= line_chart @visits.group_by_day(:visited_at, format: "%B %d, %Y").count, discrete: true %>
  </div>

  <!-- Line Chart - Multiple Series -->
  <div class="col-xs-6">
    <h3>Visits By Country Per Day</h3>
    <%= line_chart   Visit.pluck("country").uniq.map { |c| { name: c, data: @visits.where(country: c).group_by_day(:visited_at, format: "%B %d, %Y").count }  }, discrete: true %>
  </div>
  
</div>
{% endcodeblock %}

<p>
  Refreshing the page results in a new line chart with multiple series listed. You see a line for each country with each point corresponding to the number of visits for that day.
</p>

<p>
  <strong>Pie Charts and Area Charts</strong><br/>
  We can also render a pie chart. Open up your show view again and append the following code to the end of the view.
</p>

{% codeblock app/views/homes/show.html.erb lang:ruby %}
<div class="row">
  <div class="col-xs-6">
    <h3>Total Visits by Country</h3>
    <%= pie_chart @visits.group(:country).count %>
  </div>
</div>
{% endcodeblock %}

<p>
  If you refresh the page a pie chart will render with each slice representing the visits for that particular country. This information can also be represented via a geo chart. Modify your homes view to look like the code listed below.
</p>

{% codeblock app/views/homes/show.html.erb lang:ruby %}
<div class="row">

  <!-- Line Chart - Single Series -->
  <div class="col-xs-6">
    <h3>Visits By Day</h3>
    <%= line_chart @visits.group_by_day(:visited_at, format: "%B %d, %Y").count, discrete: true %>
  </div>

  <!-- Line Chart - Multiple Series -->
  <div class="col-xs-6">
    <h3>Visits By Country Per Day</h3>
    <%= line_chart   Visit.pluck("country").uniq.map { |c| { name: c, data: @visits.where(country: c).group_by_day(:visited_at, format: "%B %d, %Y").count }  }, discrete: true %>
  </div>
  
</div>

<div class="row">

  <!-- Pie Chart -->
  <div class="col-xs-6">
    <h3>Total Visits by Country</h3>
    <%= pie_chart @visits.group(:country).count %>
  </div>

  <!-- Geo Chart --> 
  <div class="col-xs-6">
    <h3>Visits By Day</h3>
    <%= geo_chart @visits.group(:country).count %>
  </div>

</div>
{% endcodeblock %}

<p>
  Awesome! Now if you refresh the page, you'll notice a geo chart in the bottom right.
</p>

<p>
  <strong>Area Charts</strong><br/>
  You can also create an area chart. Add the code listed below to the end of your homes/show view.
</p>

{% codeblock app/views/homes/show.html.erb lang:ruby %}
<!-- area chart --> 
<div class="row">
  <div class="col-xs-12">
    <h3>Total Load Time By Day</h3>
    <%= area_chart @visits.group_by_day(:visited_at).maximum(:load_time) %>
  </div>
</div>
{% endcodeblock %}

<p>
  Great, now if you refresh the page you will see that the area chart has been added.
</p>

<p>
  <strong>Bar Charts and Column Charts</strong><br/>
  Both bar charts and column charts can be easily created. Modify your homes/show view so that it looks like the code listed below.
</p>

{% codeblock app/views/homes/show.html.erb lang:ruby %}
<div class="row">

  <!-- Line Chart - Single Series -->
  <div class="col-xs-6">
    <h3>Visits By Day</h3>
    <%= line_chart @visits.group_by_day(:visited_at, format: "%B %d, %Y").count, discrete: true %>
  </div>

  <!-- Line Chart - Multiple Series -->
  <div class="col-xs-6">
    <h3>Visits By Country Per Day</h3>
    <%= line_chart   Visit.pluck("country").uniq.map { |c| { name: c, data: @visits.where(country: c).group_by_day(:visited_at, format: "%B %d, %Y").count }  }, discrete: true %>
  </div>
  
</div>

<div class="row">

  <!-- Pie Chart -->
  <div class="col-xs-6">
    <h3>Total Visits by Country</h3>
    <%= pie_chart @visits.group(:country).count %>
  </div>

  <!-- Geo Chart --> 
  <div class="col-xs-6">
    <h3>Visits By Day</h3>
    <%= geo_chart @visits.group(:country).count %>
  </div>

</div>

<!-- area chart --> 
<div class="row">
  <div class="col-xs-12">
    <h3>Total Load Time By Day</h3>
    <%= area_chart @visits.group_by_day(:visited_at).maximum(:load_time) %>
  </div>
</div>

<div class="row">
  <!-- Column Chart --> 
  <div class="col-xs-6">
    <h3>Total Visits Per Country</h3>
    <%= column_chart @visits.group(:country).count %>
  </div>
  <!-- Bar Chart --> 
  <div class="col-xs-6">
    <h3>Total Visits Per Country</h3>
    <%= bar_chart @visits.group(:country).count %>
  </div>
</div>
{% endcodeblock %}

<p>
  If you refresh the page you'll see both column and bar charts at the bottom.
</p>

<p>
  <strong>Remote Charts</strong><br/>
  You can quickly and easily build remote AJAX driven charts. Let's create a simple series chart. First, we will create a new method on your homes controller called visits_by_day. Open up your homes controller and modify it so that it looks like the code listed below.
</p>

{% codeblock app/controllers/homes_controller.rb lang:ruby %}
class HomesController < ApplicationController
  def show
    @visits = Visit.all
  end

  def visits_by_day
   render json: Visit.group_by_day(:visited_at, format: "%B %d, %Y").count
  end
end
{% endcodeblock %}

<p>
  Next, add in the code listed below to your homes/show view.
</p>

{% codeblock app/views/homes/show.html.erb lang:ruby %}
<!-- Line Chart - Single Series -->
<div class="col-xs-12">
  <h3>Visits By Day</h3>
  <%= line_chart visits_by_day_home_path %>
</div>
{% endcodeblock %}

<p>
  If you refresh the page, you'll notice that the new chart has been rendered. A quick look at your browser's developer console will show you that the visits_by_day data is pulled down via AJAX and then the site is rendered.
</p>

<p>
  <strong>Global Options</strong><br/>
  You can customize the global options for ChartKick. Create an initializer called chartkick.rb and add in the code listed below.
</p>

{% codeblock config/initializers/chartkick.rb lang:ruby %}
Chartkick.options = {
  height: "300px",
  colors: ["#ff0000", "#00ff00", "#0000ff", "#ffff00", "#ff00ff", "0000ff"]
}
{% endcodeblock %}

<p>
  If you restart your Rails server and refresh the page, you will notice that the colors have changed.
</p>

<p>
  For more information on ChartKick you can check <a href="http://www.chartkick.com" target="_blank">Chartkick</a>.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
