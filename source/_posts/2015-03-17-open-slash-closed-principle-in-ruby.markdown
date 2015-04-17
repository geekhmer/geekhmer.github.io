---
layout: post
title: "Open/Closed Principle in Ruby"
date: 2015-03-17 22:32
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Open/Closed Principle in Ruby
---

<img src="/images/rights_and_wrongsof_ruby.jpg" width="400" alt="Open/Closed Principle in Ruby" />

<p>
  Assume that we already had the code:
</p>

{% codeblock lang:ruby %}
class Report
  def body
    generate_reporty_stuff
  end

  def print
    body.to_json
  end
end
{% endcodeblock %}

<p>
  The code above violates Open/Closed Principle as if we want to change the format the report gets printed, we need to change the code of the class. Let make it be better then:
</p>

{% codeblock lang:ruby %}
class Report
  def body
    generate_reporty_stuff
  end

  def print(formatter: JSONFormatter.new)
    formatter.format body
  end
end
{% endcodeblock %}

<p>
  This way changing the formatter is as easy as:
</p>

{% codeblock lang:ruby %}
report = Report.new
report.print(formatter: XMLFormatter.new)
{% endcodeblock %}

<p>
  So far so good, I have extended the functionality without modifying the code. In this example, I have used a technique called "Dependency Injection". That's it!!! See ya!!! :)
</p>
