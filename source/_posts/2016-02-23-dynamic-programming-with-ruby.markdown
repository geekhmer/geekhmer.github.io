---
layout: post
title: "Dynamic Programming with Ruby"
date: 2016-02-23 10:32
comments: true
categories: [Ruby]
keywords: Dynamic Programming with Ruby
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="Dynamic Programming with Ruby" />
</p>

<p>
  I had a task to analyse the value of a cookie set by a tool on the subdomain. The value of the cookie set looked something like this:
</p>

{% codeblock lang:ruby %}
"18337|20120404|True#Q0A|18342|21-30|20120404|18344#Q1|18349|NO PARTNER|20120404|18351#Q2|18352|EMPLOYED|20120404|18353#Q4|18432|STRUGGLING|18539|WANT|20120404|18541#Q3|18358|EMPLOYED|20120404|18359#"
{% endcodeblock %}

<p>
  My task was to try to decipher the values of set inside the cookie and how they changed depending on the progress through the application that set the values. I was told the answers to questions sat next to the ID of the question. For example, the answer to the "age" question (21-30) was related to the question 18342 and would always appear as |18342|21-30|. I started out with some tests and ended up with a method like this:
</p>

{% codeblock lang:ruby %}
class Answers
  attr_accessor :age, :employment_status, :has_partner

  def parse(cookie_contents)
    items = cookie_contents.split("|")
    items.each_index do | i |
       current = items[i]
       if (current == "18342")
         @age = items[i+1]
       elsif (current == "18352")
         @employment_status = items[i+1]
       elsif (current == "18349")
         @current_partner = items[i+1]
       end
    end
  end
end
{% endcodeblock %}

<p>
  Of course, I wanted to avoid the growing if-else statement so found a way that I could do it dynamically and focus just on the mapping from an ID to an attribute. The resulting code looked like this:
</p>

{% codeblock lang:ruby %}
class Answers
  attr_accessor :age, :employment_status, :has_partner
   
  Mappings = {
    "18342" => :age=,
    "18352" => :employment_status=,
    "18349" => :has_partner=
  }

  def parse(cookie_contents)
    items = cookie_contents.split("|")
    items.each_index do | i |
       current = items[i]
       if (Mappings.has_key?(current))
          self.public_send(Mappings[current], items[i+1])
       end
    end
  end
end
{% endcodeblock %}

<p>
  By the end of the analysis, I discovered that multiple IDs mapped to the same properties and all I had to do was add another entry into the map defining which ID mapped to what property on the object.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
