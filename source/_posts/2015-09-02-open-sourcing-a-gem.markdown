---
layout: post
title: "Open Sourcing a Gem"
date: 2015-09-02 20:24
comments: true
categories: [Ruby]
keywords: Open Sourcing a Gem
---

<p>
  <img src="/images/rubygems_logo.png" alt="Open Sourcing a Gem" />
</p>

<p>
  <strong>Considerations when Open Sourcing</strong><br/>
  When open sourcing a gem (or any project, this post can be generalized to not just focus on gems), there are a number of things to consider. The first check is to ensure that the use case is general enough that it is likely to be helpful to other parties. Though the project might never gain many external users, it is a good sanity check before taking on the work to open source the project. Following this there are a number of things to think out, with the purpose of determining a track of work that needs to be taken (if any) before the gem can be open sourced.
</p>

<p>
  <strong>Generalize the implementation</strong><br/>
  - Remove coupled business logic and application specific code.<br/>
  - Ensure no keys/account information were stored in the repo (even in past commits). Typically not considered a good practice, but it is something to be aware of when releasing the source of a project publicly.
</p>

<p>
  <strong>Set bounds on the responsibilities</strong><br/>
  - What is the purpose of this gem and what needs is it fulfilling?<br/>
  - Take a step back from how you currently use the library. What other potential use cases might others use it for? Is it worth generalizing the implementation to aid these other use cases?<br/>
  - Set bounds on the responsibilities that the gem will take care of.<br/>
  - What will the gem provide and what is left up to the user? In the case of Signalman, we allow users to customize the URL of the Signalman admin page, but it is up to the user to provide additional authentication besides obscurity. Signalman is a gem designed to aid with A/B testing, not authentication.<br/>
  - Try to stick to the gem's core competencies. Do not add features at a whim to quell user requests. This can cause a gem to grow out of control and become hard to manage or even discern its original purpose/intent.
</p>

<p>
  <strong>Consider strategies to manage repository ownership</strong><br/>
  - Like it or not, becoming a maintainer is a big responsibility. If you are open sourcing a project with a team, this will greatly ease the burden, but you must still consider processes to manage external pull requests and issues that arise.<br/>
  - Giving the user power and encouraging pull requests and a community that supports each other's issues can also relieve the burden of being a project maintainer. You can help this process by adding documentation for contributing and making it easy for other parties to contribute to the project. This also means providing help to newer developers; you never know who might become a leading contributor in the future.
</p>
