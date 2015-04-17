---
layout: post
title: "Ruby on Rails Currency Formatting"
date: 2015-03-21 00:30
comments: true
categories: [Ruby on Rails, Ruby]
keywords: Ruby on Rails Currency Formatting
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Ruby on Rails Currency Formatting" />
</p>

<p>
  Well, Ruby on Rails framework provide some various ways for formatting currency. Formatting a number as currency is pretty easy thanks to the <code>number_to_currency</code> helper. This helper will take a valid number and format it as currency. It accept a number of different options that allow complete customization of the displayed value:
</p>

{% codeblock lang:ruby %}
number_to_currency(number, options = {})
{% endcodeblock %}

Options | Description
--- | ---
:locale | The locale option allows you to specify the locale you wish to use. Defaults to the current locale.
:precision | Sets the level of precision. Defaults to 2. Precision is the number of decimal places after the decimal.
:unit | Sets the denomination of the currency. example: $ for the United States.
:separator | Specifies the separator to use between the units. example the "." separates dollars and cents in the US.
:delimiter | Specifies the separator to use for the thousands delimiter. example: 123,456.
for | For loop.
:format | Sets the format for non-negative numbers. The current default is "%u%n". The %u specifies the currency ("$") and the %n specifies the number ("123,456")
:negative_format | Same as format, but applies to negative numbers.
:raise | If raise is set to true, an exception will be raised if the number isn't a valid number.


<p>
  <br/>
  Example:
</p>

{% codeblock lang:ruby %}
number_to_currency 123456.50                        # => $123,456.50
number_to_currency 123456.506                       # => $123,456.51
number_to_currency 123456.506, precision: 3         # => $123,456.506
number_to_currency "123a456"                        # => $123a456
number_to_currency "123a456", raise: true           # => ActionView::Helpers::NumberHelper::InvalidNumberError error
number_to_currency -123456.50, :negative_format => "(%u%n)"                               # => ($123,456.50)
number_to_currency 123456.50, unit: "£", seperator: ",", delimiter: ""                    # => £123456.50
number_to_currency 123456.50, unit: "£", separator: ",", delimiter: "", format: "%n %u"   # => 123456,50 £
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>