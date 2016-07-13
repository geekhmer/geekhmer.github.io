---
layout: post
title: "Dependency Inversion Principle in Ruby"
date: 2015-03-18 23:54
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Dependency Inversion Principle in Ruby
---

<p>
  <img src="/images/rights_and_wrongsof_ruby.jpg" width="400" alt="Dependency Inversion Principle in Ruby" />
</p>

<p>
  Simply Dependency Inversion Principle states that: Abstractions should not depend upon details. Details should depend upon abstractions.
</p>

<p>
  Let's go back to the first example on the Open/Closed Principle and change it a bit:
</p>

{% codeblock lang:ruby %}
class Report
  def body
    generate_reporty_stuff
  end

  def print
    JSONFormatter.new.format(body)
  end
end

class JSONFormatter
  def format(body)
    ...
  end
end
{% endcodeblock %}

<p>
  Now there is a formatter class, but I've hardcoded it on the Report class, thus creating a dependency from the Report to the JSONFormatter. Since the Report is a more abstract (high-level) concept than the JSONFormatter, we're effectively breaking the DIP.
</p>

<p>
  We can solve it the exact same way with solved it on the Open/Closed Principle problem, with dependency injection:
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
  So far so good, this way the Report does not depend on the JSONFormatter and can use any type of formatter that has a method called format (this is known as duck typing). That's it!!! See ya!!! :)
</p>