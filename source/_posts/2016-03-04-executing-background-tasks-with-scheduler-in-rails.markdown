---
layout: post
title: "Executing Background Tasks with Scheduler in Rails"
date: 2016-03-04 16:42
comments: true
categories: [Ruby on Rails]
keywords: Executing Background Tasks with Scheduler in Rails, Scheduler
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="Executing Background Tasks with Scheduler in Rails" />
</p>

<p>
  <a href="https://rubygems.org/gems/rufus-scheduler/versions/3.2.0" target="_blank">rufus-scheduler</a> is a handy gem that allows you to perform background jobs. In this article we will show you how to use rufus-scheduler in your Rails applications. Let's get started:
</p>

<h3>Setup</h3>

<p>
  In order to use rufus-scheduler, we must first add the rufus-scheduler gem to our Gemfile. To do this, open up your Gemfile and add in the line listed below:
</p>

{% codeblock Gemfile lang:ruby %}
gem 'rufus-scheduler'
{% endcodeblock %}

<p>
  Run a bundle install to install the gem:
</p>

{% codeblock lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  Great, we need to create an initializer that will contain the jobs we wish to schedule. Create a new initializer called scheduler.rb in the <code>config/initializers</code> directory and add in the code listed below:
</p>

{% codeblock config/initializers/scheduler.rb lang:ruby %}
require 'rufus-scheduler'

scheduler = Rufus::Scheduler::singleton

# jobs go below here.
{% endcodeblock %}

<p>
  Now we are ready to schedule jobs.
</p>

<h3>Scheduling the Jobs</h3>

<p>
  rufus-scheduler provides a simple syntax for scheduling jobs. You can schedule both one time jobs and recurring jobs. One time jobs can be either scheduled at a specific time, or you can tell rufus to wait a specific amount of time before running the job (a delayed job). For recurring jobs and delayed jobs, you can either use a simple plain English (30m = 30 minutes, etc.) format or you can use the cron format for scheduling jobs. For this article we'll focus on the English format:
</p>

<p>
  <strong>Recurring the Jobs</strong><br/>
  Scheduling recurring jobs is easy. Simply use the following format in your scheduler.rb file:
</p>

{% codeblock config/initializers/scheduler.rb lang:ruby %}
scheduler.every '5s' do
  # do stuff
end
{% endcodeblock %}

<p>
  The code above would perform your background task every 5 seconds. The list below will give you an idea of what time units are supported:
</p>

<p>
  <strong>s</strong> Example: 5s - seconds, specifies the number of seconds you wish to wait.<br/>
  <strong>m</strong> Example: 5m - the number of minutes you wish to wait.<br/>
  <strong>h</strong> Example: 5h - the number of hours you wish to wait.<br/>
  <strong>d</strong> Example: 5d - the number of days you wish to wait.<br/>
  <strong>w</strong> Example: 5w - the number of weeks you wish to wait.<br/>
  <strong>M</strong> Example: 5M - the number of months you wish to wait.<br/>
  <strong>y</strong> Example: 1y - the number of years you wish to wait.
</p>

<p>
  For example, the following code would tell rufus you wish to schedule a job for every 11 and a half hours:
</p>

{% codeblock config/initializers/scheduler.rb lang:ruby %}
scheduler.every '11h30m' do
  # do stuff
end
{% endcodeblock %}

<p>
  Scheduling a delayed job is easy. The syntax is similar to the recurring syntax listed above, but we use the <code>.in</code> method instead of <code>.every</code>. For example, the following code would run a task 4 hours after the server starts:
</p>

{% codeblock config/initializers/scheduler.rb lang:ruby %}
scheduler.in '4h' do
  # do stuff
end
{% endcodeblock %}

<p>
  <strong>Scheduling the Jobs for Specific Dates/Times</strong><br/>
  You can also schedule a job for a specific date and time. To do this we use the at method. For example, the following code would run at 12:01am on December 1st, 2017:
</p>

{% codeblock config/initializers/scheduler.rb lang:ruby %}
scheduler.at '2017/12/01 00:01:00' do
  # do stuff
end
{% endcodeblock %}

<h3>Important Caveats</h3>

<p>
  1. Because rufus-scheduler runs in process, the scheduler will reset if your Rails server gets restarted. This means that the timer for your jobs will get reset, so don't count on any monthly or yearly jobs getting called. If you need to persist jobs across server resets, use a job backend. We will show you how to do this in another article.<br/>
  2. Rufus does not work with Active Job.<br/>
  3. Some additional setup is needed for production environments (see below).
</p>

<h3>Production Setup</h3>

<p>
  Production servers require a bit of additional setup. On most production web servers, idle Ruby processes are killed. In order for rufus to work, you'll need to stop this from happening. For Passenger/Nginx you can copy the following code below to your <code>nginx.conf</code> config file for your website after the line that says <code>passenger_enabled on;</code>.
</p>

{% codeblock nginx.conf lang:ruby %}
passenger_spawn_method direct;
passenger_min_instances 1;
passenger_pool_idle_time 0;
{% endcodeblock %}

<p>
  rufus-scheduler is a simple, easy to use scheduler that provides great functionality. It can be used for stuff like sending emails, cleaning up temp files, and much more.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
