---
layout: post
title: "How to Spend Less Time and Money on Growing Your Business?"
date: 2015-06-05 21:33
comments: true
categories: [Other]
keywords: How to Spend Less Time and Money on Growing Your Business?
---

<p>
  <img src="/images/how_to_spend_less_time_and_money_on_growing_your_business.png" alt="How to Spend Less Time and Money on Growing Your Business?" />
</p>

<p>
  We all have ideas we would like to validate yesterday if possible but of course in reality that rarely happens. The constant lack of developer time makes our wishes become dreams most of which will never see the light of day.
</p>

<p>
  Being able to move fast so that you can put your ideas thru the build/measure/learn loop and iterate all the way to product market fit requires having a solid software foundation that will allow you to achieve fast release cycles.
</p>

<p>
  In this post I will show you how to:<br/>
  1. Speed up the development cycle<br/>
  2. Spend less money on developer time<br/>
  3. Build a more productive team
</p>

<p>
  <strong>We Call It Automated Testing</strong><br/>
  Testing — or automated testing — means you put the machine at work by having it test your application often and making sure everything works as expected. You can imagine someone clicking thru the buttons and links on your website, filling in forms etc. and making sure everything is in good shape.
</p>

<p>
  So by having every corner of your application checked at will and in record time, you will have the confidence that a feature you've previously built will stay functional forever — or at least until you decide to nuke it — by being notified immediately when something stopped working for whatever the reason.
</p>

<p>
  <strong>How Can Testing Help My Business?</strong><br/>
  It's less obvious until you've done software development for a while but, the most time spent by developers is not on building new and cool stuff — nor on playing video games. Actually, the most time is spent on fixing bugs (previously built features that somehow got broken in the process of increasing the complexity of your application).
</p>

<p>
  Knowing immediately when things got broken, where and why, means developers will spend a lot less time searching for the source of evil before they can actually do the work involved in fixing the issue — which usually amounts to much less then the digging, staring, guessing and searching process.
</p>

<p>
  So having to spend less and less time on fixing broken features as your app becomes more and more complex translates into improving development speed and cutting costs greatly in the long run.
</p>

<p>
  Automated testing gives you a new power; the confidence of changing existing features and adding ones with minimal overhead.
</p>

<p>
  <strong>Let’s Get the Product Managers Involved</strong><br/>
  Another very important aspect of testing is that features need to be described in great detail before the developer’s work can start so in other words, the developer knows exactly what he needs to do in order to deliver the required features to the stakeholders and match their expectations. This is also a good time for the developers to actually see the product from a different perspective.
</p>

<p>
  Instead of going back and forth about what the product manager wanted and what the developer thought he wanted and thus building the wrong feature, more time is spent communicating using a common language, the real expectations and making sure the developer's efforts are well spent.
</p>

<p>
  The automated testing language is written in plain english — or any other language of your choosing — and it requires both the product manager and the developer to sit down, think thru each step of every feature and put everything on paper before writing the first line of code.
</p>

<p>
  <strong>Planing for the Future</strong><br/>
  In the developer's world, technology changes extremely fast and thus new tools or new versions emerge with new and shiny features that your business could benefit from. But there’s one thing that stands in the way of all that goodness and that is… the fear of change. Changing a tool or upgrading one to a newer version increases the chance of breaking existing functionality.
</p>

<p>
  As you might’ve expected, this is another area where testing can save you a ton of time. By having a fully tested application, you can have your cake and eat it too — just get the new stuff, run your tests and see precisely what got broken in the process so you can start fixing. The horrible alternative would be to have your developers and QA team go over each and every feature — that they remember — and manually test it, or even worse, users discovering bugs and probably not telling you about them.
</p>

<p>
  <strong>Show Me an Example</strong><br/>
  Let’s see how an automated test might look like in real life.
</p>

{% codeblock lang:ruby %}
Feature: User login
  In order to see history that’s unique to my account
  As a user
  I want to be able to log in

  Scenario: User logs in
    Given I have a user account
    And I am currently logged out
    When I log in
    Then I should see my dashboard
{% endcodeblock %}

<p>
  What you see here is a valid automated test that makes sure that a user can log into your site by opening a browser, clicking on the login link, filling in the sign-in form and making sure that when the log in was successful, the user will be on his dashboard page.
</p>

<p>
  As you can see the test is very readable and thus it can be easily used as a communication tool between the product team — or the stakeholders — and the development team.
</p>

<p>
  So my advice to you is to use testing as much as possible and as early in the development process as you can. It saved me and my business countless hours and I can guarantee it will do the same for yours.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
