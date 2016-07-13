---
layout: post
title: "How the Hash Works in Ruby?"
date: 2015-06-16 23:43
comments: true
categories: [Ruby]
keywords: How the Hash Works in Ruby?, Ruby Hash
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="How the Hash Works in Ruby?" />
</p>

<p>
  A brief overview of the hash data structure, how it is implemented in Ruby and a peek at the history of changes made to the hash in MRI Ruby.
</p>

<p>
  <strong>What is a Hash?</strong><br/>
  A Hash is a data structure that organizes data in key-value pairs. It is also referred to as a dictionary or associative array. It stores these key-value pairs of associated data in a way that enables efficient insertion and lookup, in constant O(1) time. These properties of a hash make it is one of the most useful tools in a programmer’s toolbox and it is available in the core libraries of most if not all, programming languages.
</p>

<p>
  In Ruby a hash can be declared literally as:
</p>

{% codeblock lang:ruby %}
h = {color: "black", font: "Monaco"}
h = {:color=>"black", :font=>"Monaco"}
{% endcodeblock %}

<p>
  Or declaratively with the new method:
</p>

{% codeblock lang:ruby %}
h = Hash.new
h[:color] = "black"
h[:font] = "Monoco"
{% endcodeblock %}

<p>
  <strong>How does a Hash store data and why is it efficient?</strong><br/>
  o understand how data is stored in a hash and why it is efficient, let’s revisit the basic linear data structure, the array. An array allows us to randomly access any element that it stores if we know the index of that element beforehand.
</p>

{% codeblock lang:ruby %}
a = [1,2,4,5]
puts a[2]  # => 4
{% endcodeblock %}

<p>
  If the key-value pairs we were trying to store were integers within a limited range such as 1-20 or 1-100, we would simply use an array and our key would be the integer.
</p>

<p>
  For example, given that:<br/>
  - we need to store the names of the students in a classroom with 20 students<br/>
  - each student has a student id between 1 and 20<br/>
  - no two students have the same student id.
</p>

<p>
  We could simply store their names represented by the table below in an array:
</p>

Key | Value
--- | ---
1 | Bunlong
2 | Jinglong
3 | Mickey Mouse

<br/>

{% codeblock lang:ruby %}
students= ['Belle', 'Ariel', 'Peter Pan', 'Mickey Mouse']
{% endcodeblock %}

<p>
  But, what if the student id was a 4-digit number? Then we would have to assign a 10000 element table to access the names by the id. To solve this we simplify the key to the last 2 digits of the 4 digit number and use that as the location inside the table, so we can get random access to the record. Now, if we have another student with id “3221”, also ending in “21”, we would have to save two values at that location resulting in a collision.
</p>

Key | Hash(key) = last 2 digits | Value
--- | ---   | ---
4221, 3221| 21 | Bunlong
1357 | 57 | Jinglong
4612 | 12 | Mickey Mouse 

<br/>

{% codeblock lang:ruby %}
students= Array.new(100)
students[21]=['Belle','Sofia']
students[57]='Ariel'
students[12]='Peter Pan'
students[14]='Mickey Mouse'
{% endcodeblock %}

<p>
  What if the id was a 10-digit number or an alphanumeric string? This gets inefficient and unwieldy quickly. Unfortunately, too simple a hashing strategy can lead to problems.
</p>

<p>
  <strong>How Ruby’s hash function works?</strong><br/>
  So, now we have an understanding that the purpose of a hash function is to convert a given a key into an integer of limited range. In order to reduce the range, a commonly used technique is the division method. In the division method, the key is divided by the size of the storage or table and the remainder is the location inside that table where a record can be stored. Therefore, in the example above, if the table size was 20, the locations would be 1, 17, 12, 14 derived from the computation below.
</p>

<p>
  <code>4221 % 20 = 1</code><br/>
  <code>1357 % 20 = 17</code><br/>
  <code>4612 % 20 = 12</code><br/>
  <code>1514 % 20 = 14</code><br/>
</p>

<p>
  But in real life programming the keys are not always nice integers, they are strings, objects, or some other data type. This is solved by using a one-way hash function(digest) over the key and then applying the division method to get the location. The hash function is a mathematical function that takes a string of any length and produces a fixed length integer value. The hash data structure derives it’s name from this hashing mechanism. Ruby uses the <a href="https://en.wikipedia.org/wiki/MurmurHash" target="_blank">murmur hash function</a> and then applies the division method with a prime number M, which Ruby determines based on the table size that is needed for storage.
</p>

<p>
  <code>murmur_hash(key) % M</code>
</p>

<p>
  The code for this can be found in Ruby language’s source code in <a href="https://github.com/ruby/ruby/blob/1b5acebef2d447a3dbed6cf5e146fda74b81f10d/st.c" target="_blank">st.c file</a>.
</p>

<p>
  In case two keys return the same number, also known as a hash collision, the value is chained on to the same location or bucket in the table.
</p>

<p>
  <strong>How Ruby handles hash collisons and growth?</strong><br/>
  One of the problems faced by hash functions is distribution. What if most remainders fall into the same bucket? We would have to first find the bucket in the table from computing over the key, and then look through all the chained data in the location to find the matching record. That would defeat the purpose of creating a hash data structure for random access and O(1) time, because now we have to iterate over all these values to find the record which puts us back to O(n) time.
</p>

<p>
  It has been found that if divisor M is prime, the results are not as biased and more evenly distributed. But, even with the best divisor, collisions will occur as the number of records being added increases. Ruby adjusts the value of M based the density. Density is the number of records chained at a location in the table. In the above example, the density is 2, since we have 2 records that have the index/location 1.
</p>

<p>
  Ruby sets the maximum allowed density value to 5.
</p>


{% codeblock lang:ruby %}
#define ST_DEFAULT_MAX_DENSITY 5
{% endcodeblock %}

<p>
  When the density of the records reaches 5, then Ruby adjusts the value of M, re-computes and adjust the hash for all the records that are in that hash. “The algorithm that computes M is one that generates prime numbers near powers of 2”, from <a href="http://www.amazon.com/Data-Structures-Using-Aaron-Tenenbaum/dp/0131997467" target="_blank">Data Structures using C</a>. Look at the function new_size in <a href="https://github.com/ruby/ruby/blob/1b5acebef2d447a3dbed6cf5e146fda74b81f10d/st.c" target="_blank">st.c</a> at line 158. This is where the size of the new hash is computed.
</p>

{% codeblock lang:ruby %}
new_size(st_index_t size) {
  st_index_t i;
  for (i=3; i<31; i++) {
    if ((st_index_t)(1<<i) > size) return 1<<i;
  }
  return -1;
}
{% endcodeblock %}

<p>
  This is easier to read in the <a href="https://github.com/jruby/jruby/blob/master/core/src/main/java/org/jruby/RubyHash.java" target="_blank">JRuby’s</a> implementation of Hash where the prepared prime number values are statically used from an int array. As you can see the next values are 11, 19 and so on.
</p>

{% codeblock lang:ruby %}
private static final int MRI_PRIMES[] = {
8 + 3, 16 + 3, 32 + 5, 64 + 3, 128 + 3, 256 + 27, ....};
{% endcodeblock %}

<p>
  This rehashing as the data grows larger causes a spike in the performance when the hash reaches certain specific sizes.
</p>

<p>
  <strong>Ruby hashes are unique for each process</strong><br/>
  One interesting thing to note is hashes are unique for each Ruby process. The murmur hash seeds it with a random value, which results in a different hash for a particular key for each Ruby process.
</p>

<p>
  <strong>Ruby has packed hashes for up to 6 entries since Ruby 2.0</strong><br/>
  Another interesting thing to note is that Ruby hashes that are very small(less than or equal to 6) are saved in one bucket and not spread over several buckets based on computed hash of the key. Instead, they are simply an array of values. This was added recently and is referred to as a packed entry in the code in st.c. On the <a href="https://github.com/ruby/ruby/pull/84" target="_blank">pull request</a> in which this change was submitted, the commiter has made the following comment.
</p>

<p>
  “Investigation shows, that typical rails application allocates tons of small hashes. Up to 40% of whole allocated hashes never grows bigger than 1 element size.”
</p>

<p>
  <strong>Ruby hash keys are ordered</strong><br/>
  Starting with Ruby 1.9.3, a property of the hash is that the keys are ordered based on how they are inserted into the hash. An interesting question was posted on the <a href="https://www.ruby-forum.com/topic/166075" target="_blank">Ruby-forum</a> questioning the reasoning behind it and how it would effect performance. The reply by Matz, creator of Ruby, on that thread, which explains the change is below.
</p>

<p>
  Could anybody explain why this feature was added?
</p>

<p>
  "Useful for some cases, especially for keyword arguments."
</p>

<p>
  Isn’t it going to slow down the operations on the Hash?
</p>

<p>
  "No. hash reference operation does not touch order information, only for iteration. Memory consumption increased a bit."
</p>

<p>
  Note: Keyword arguments were added to Ruby in 2.0, and an example is below:
</p>

{% codeblock lang:ruby %}
def books(title: 'Programming Ruby')
  puts title
end

books # => 'Programming Ruby'
books(title: 'Eloquent Ruby') # => 'Eloquent Ruby'
{% endcodeblock %}

<p>
  <strong>Two potential upcoming changes in Hash</strong><br/>
  1- The next version of Ruby, will most likely introduce syntax sugar for a literal declaration of the hash that will allow spaces in symbols. Take a look at this commit on <a href="https://bugs.ruby-lang.org/issues/4276" target="_blank">ruby-trunk</a>. You may recall, that the first change from hashrocket to colon was introduced in Ruby 1.9 bringing the syntax closer to JSON’s syntax. With this upcoming change the hash will looks even more so like <a href="https://en.wikipedia.org/wiki/JSON" target="_blank">JSON</a>.
</p>

<p>
  Currently we need to declare a symbol with a space using a hash rocket:
</p>

{% codeblock lang:ruby %}
h = {:"a perfect color" => "vermilion"} #=> {:"a perfect color"=>"vermilion"}
{% endcodeblock %}

<p>
  With the change it will simply be the symbol within quotes followed by a colon:
</p>

{% codeblock lang:ruby %}
h = {"a perfect color": "vermilion"}
{% endcodeblock %}

<p>
  2- Another interesting change that is in the works is a method that will allow returning <a href="https://bugs.ruby-lang.org/issues/10017" target="_blank">default values for missing keys in a hash</a>.
</p>

<p>
  Currently you can return the default value of only one key using <code>hash.fetch</code>, however the <code>hash.values_at</code> method allows returning multiple values for keys:
</p>

{% codeblock lang:ruby %}
h = {color: "black", font: "monaco"}
h.fetch(:fontsize, "12pt") #=> "12pt"
h.values_at(:color, :font) #=> ["black", "monaco"]
{% endcodeblock %}

<p>
  The change proposed is to combine these two methods into one. It might work something like the <code>fetch_values</code> method shown below. Please note the new method name is still being voted on and the example is hypothetical.
</p>

{% codeblock lang:ruby %}
h.fetch_values(:color, :font, :fontsize, :border) do |k|
k == "fontsize" ? "12pt" : "#{k} is missing"
end
#=> ["black", "monaco", "12pt", "border is missing"]
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
