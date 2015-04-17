---
layout: post
title: "The Developer View"
date: 2015-03-10 19:55
comments: true
categories: [Other]
keywords: The Developer View
---

<p>
  <img src="/images/professional_programmer.jpg" width="550" alt="The Developer View" />
</p>

<p>
  A developer might be someone who writes code, or it may be an interaction designer, a graphic designer, or a tester. In this document describes the things that happend in between the daily standup meetings.
</p>

<p>
  <strong>1. Tools and Practices</strong><br/>
  Tools don't solve your problem; people do. Tools just help. It's crucial to choose the right practices to go with your tools -- the right ways to use your tools so that they do the most for you.
</p>

<p>
  And day-to-day as required, we discuss how our tools are helping or hurting us, and how they can work better for us. When things aren't working quite right, we'll first try to look at the way we're using the tool; changing our practices is less costly and disruptive than changing a tool. But we occasionally change the tool as well.
</p>

<p>
  <strong>2. Pair Programming</strong><br/>
  Pair programming involves two programmers at a single computer (each with a monitor and keyboard of their own) working together on a single task. As a unit, they maintain a hight level of quality as well as ensuring a broad perspective on problem solving. For many complex problems and architectural design exercises, pair programming can drastically reduce defects and helps ensure that code requires a minimum amount of rewriting.
</p>

<p>
  When used effectively, pair programming reduces the overall cost of development and maintenance.
</p>

<p>
  We look at each coding and design task determine if pairing is appropriate, and at the discretion of the project team, many coding tasks are performed solo. When this happens, the code must pass code review before moving to QA.
</p>

<p>
  <strong>3. Test-Driven Development</strong><br/>
</p>

<p>
  <strong>4. Behavior-Driven Development</strong><br/>
</p>

<p>
  <strong>5. Code Review</strong><br/>
  Although many tasks call for pair programming, it is simply not practical or efficient to pair on every task. Then tasks are tackled by a solo developer, their work must always be reviewed by another developer on the team.
</p>

<p>
  Our task manangement system is configured to give a task two paths out of development. One path is "Completed by Pair, Ready for QA" Tasks handled by a solo developer follow an alternate path: "Completed Solo, Ready for Code Review". That allows us to ensure that every piece of code that gets delivered to our customers thas been seen by at least two developers. Of course, that doesn't mean there aren't any problems, but it does shorten an important feedback loop, catching most problems early, when fixing them is cheapest.
</p>

<p>
  <strong>6. Continuous Integration</strong><br/>
  Continuous Integration (CI) ensures that an application is healthy by making sure that automated test pass on a neutral machine (not just a specific developer's machine). Whenever anybody commits code to the central code repository, a CI server downloads the code and runs the build to ensure that the tests are still passing. If any of the automated tests fail, the build fails and the entire team is notified.
</p>

<p>
  We believe that the build should always be passing, and if somebody breaks the build, it needs to be fixed immediately before futher development can task place.
</p>

<p>
  <strong>7. Code Coverage</strong><br/>
  Code coverage has many definitions, but it normally represents the percentage of lines (or branches) in a software project that are covered by automated tests. A low score probably means bad code, but a hight score doesn't necessarily mean good code.
</p>

<p>
  At the start of a project, the project team can choose what code coverage standard to enforce.
</p>

<p>
  <strong>8. Distributed Source Control</strong><br/>
  We uses a distributed source control system called Git (and Github) to store and track source code and other development-related artifacts.
</p>

<p>
  This is more important than it may first appear. Traditional, centralized source control systems inhibit agile teams by making branch and merge operations expensive. This make parallel, distributed development more costly. So much so, in fact, that source control can become a bottleneck on overall productivity of the team.
</p>

<p>
  <strong>9. Fridays (20% Time)</strong><br/>
  On Fridays, we don't do billable client work. We spend our time on things that improve our lives as developer, our own pet project or passions, and for giving back to the rich sea of open-source software (espcially development tools) in which our business swims and thrives.
</p>
