---
layout: post
title: "Mark Methods Private When You Don’t Test Them"
date: 2015-06-25 20:58
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Mark Methods Private When You Don’t Test Them
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="Mark Methods Private When You Don’t Test Them" />
</p>

<p>
  In Ruby and many other languages, you write private methods to implement internal logic you don’t want to expose. You want the freedom to rename, repurpose or even delete them without worrying about impacting anything else. The <code>private</code> keyword signals other developers: Don’t rely on this; don’t call it; it might change. This is especially important when writing framework or library code that many other developers will use.
</p>

<p>
  But which methods should you make private? Sometimes this is obvious; sometimes it isn't. A good rule of thumb to use is: If you’re not testing a method, it should be private.
</p>

<p>
  But wait a minute! Aren't we supposed to test everything? Isn't 100% code coverage the every Ruby developer seeks? Let me clarify. You should mark methods private when you test them indirectly by calling the other, public methods in the same class. Use the <code>private</code> keyword to help organize your code, to remind yourself what you still need to test, and what you don't.
</p>

<p>
  <strong>Three Paintings</strong><br/>
  A simple example will make this clear. Suppose I have a class that describes a painting:
</p>

{% codeblock lang:ruby %}
Painting = Struct.new(:name, :year)
{% endcodeblock %}

<p>
  Now I can create a list of three paintings in a Minitest::Spec file like this:
</p>

{% codeblock lang:ruby %}
let(:one) { Painting.new("Spanish Couple In Front Of Inn", 1900) }
let(:two) { Painting.new("Guernica", 1937) }
let(:three) { Painting.new("Petite Fleurs", 1958) }
let(:paintings) { [one, two, three] }
{% endcodeblock %}

<p>
  Suppose my first requirement is to return the first painting from the list. Simple enough:
</p>

{% codeblock lang:ruby %}
def first(list)
  list.first
end
{% endcodeblock %}

{% codeblock lang:ruby %}
it "should return the first element" do
  first(paintings).must_equal one
end
{% endcodeblock %}

<p>
  I just call Array#first and I’m done. Returning the rest of the list is slightly more interesting:
</p>

{% codeblock lang:ruby %}
def rest(list)
  _, *rest = list
  rest
end
{% endcodeblock %}

{% codeblock lang:ruby %}
it "returns the rest of the elements" do
  rest(paintings).must_equal [two, three]
end
{% endcodeblock %}

<p>
  Rest always returns an array even if the input list was empty or had only one element. So far, so good. I’ve written two methods and two tests:
</p>

<p>
  <img src="/images/two_tests.png" width="400" alt="Mark Methods Private When You Don’t Test Them" />
</p>

<p>
  <strong>A New Requirement</strong><br/>
  Now suppose my business requirement changes slightly and I instead need to return the first painting sorted alphabetically by name. Once again, it’s not hard to do:
</p>

{% codeblock lang:ruby %}
def first(list)
  list.sort do |p1, p2|
    p1.name <=> p2.name
  end.first
end
{% endcodeblock %}

{% codeblock lang:ruby %}
it "should return the first element" do
  first(paintings).name.must_equal "Guernica"
end
{% endcodeblock %}

<p>
  And I need rest to use the same sort order, so I repeat the call to sort:
</p>

{% codeblock lang:ruby %}
def rest(list)
  _, *rest = list.sort do |p1, p2|
    p1.name <=> p2.name
  end 
  rest
end
{% endcodeblock %}

{% codeblock lang:ruby %}
it "returns the rest of the elements" do
  rest(paintings).map(&:name).must_equal [
    "Petite Fleurs",
    "Spanish Couple In Front Of Inn"
  ]
end
{% endcodeblock %}

<p>
  I’ve implemented new behavior, but still have two methods and two tests:
</p>

<p>
  <img src="/images/two_tests.png" width="400" alt="Mark Methods Private When You Don’t Test Them" />
</p>

<p>
  <strong>Extracting a Method</strong><br/>
  Because both of my methods are covered by tests, I’m free to refactor them. I decide to extract a new method, sorted_by_name:
</p>

{% codeblock lang:ruby %}
def first(list)
  sorted_by_name(list).first
end

def rest(list)
  _, *rest = sorted_by_name(list)
  rest
end

def sorted_by_name(list)
  list.sort do |p1, p2|
    p1.name <=> p2.name
  end
end
{% endcodeblock %}

{% codeblock lang:ruby %}
it "returns the element with the first name" do
  first(paintings).name.must_equal "Guernica"
end

it "returns the rest after the first name" do
  rest(paintings).map(&:name).must_equal [
    "Petite Fleurs",
    "Spanish Couple In Front Of Inn"
  ]
end
{% endcodeblock %}

<p>
  Here I’ve simply moved the call to sort into a utility method called sorted_by_name. Now first and rest both call sorted_by_name, making the code a bit clearer and DRY-er. But now I have three methods and only two tests:
</p>

<p>
  <img src="/images/two_tests_three_methods.png" width="400" alt="Mark Methods Private When You Don’t Test Them" />
</p>

<p>
  <strong>Mark Methods Private When You Don’t Test Them</strong><br/>
  Notice I didn’t bother writing a test for sorted_by_name. I know it works because my other tests still pass. The existing tests are sufficient; I am testing sorted_by_name indirectly. Because I extracted sorted_by_name from first and rest, because I refactored my code without adding any new behavior, no new test were required.
</p>

<p>
  In this scenario, take the time to mark the new, untested method as private:
</p>

{% codeblock lang:ruby %}
def first(list)
  sorted_by_name(list).first
end

def rest(list)
  _, *rest = sorted_by_name(list)
  rest
end

private
def sorted_by_name(list)
  list.sort do |p1, p2|
    p1.name <=> p2.name
  end
end
{% endcodeblock %}

{% codeblock lang:ruby %}
it "returns the element with the first name" do
  first(paintings).name.must_equal "Guernica"
end

it "returns the rest after the first name" do
  rest(paintings).map(&:name).must_equal [
    "Petite Fleurs",
    "Spanish Couple In Front Of Inn"
  ]
end
{% endcodeblock %}

<p>
  The private keyword here reminds me I’ve already tested sorted_by_name, that I don’t need to write new tests for it. Now private is helping me organize my code; it’s helping me remember which methods I don’t need to test… and which methods are missing important tests.
</p>

<p>
  <img src="/images/two_tests_three_methods_private.png" width="400" alt="Mark Methods Private When You Don’t Test Them" />
</p>

<p>
  f my tests don’t need to know about sorted_by_name, then certainly other developers don’t. It should be private. Marking it private reminds me that it is being tested indirectly, that I didn’t just forget to write a test for it. Marking it private tells other developers about what I’ve learned from my own test suite.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
