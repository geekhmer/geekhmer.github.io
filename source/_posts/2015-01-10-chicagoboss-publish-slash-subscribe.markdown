---
layout: post
title: "Chicagoboss Publish/Subscribe"
date: 2015-01-10 23:08
comments: true
categories: [Erlang]
keywords: Chicagoboss Publish/Subscribe, Chicagoboss Publish Subscribe, Chicagoboss Publish and Subscribe, Chicagoboss Pub/Sub, Chicagoboss Pub and Sub, Chicagoboss Publish/Subscribe Pattern, Chicagoboss Pub/Sub Pattern
---

<p>
  <img src="/images/logo_erlang.png" alt="Chicagoboss Publish/Subscribe" />
</p>

<p>
  In software architecture, publish–subscribe is a messaging pattern where senders of messages, called publishers, do not program the messages to be sent directly to specific receivers, called subscribers. Instead, published messages are characterized into classes, without knowledge of what, if any, subscribers there may be. Similarly, subscribers express interest in one or more classes, and only receive messages that are of interest, without knowledge of what, if any, publishers there are.
</p>

<p>
  Pub/sub is a sibling of the message queue paradigm, and is typically one part of a larger message-oriented middleware system. Most messaging systems support both the pub/sub and message queue models in their API.
</p>

<p>
  Actually Chicagoboss ships with a message queue service called <strong>BossMQ</strong>. The service consists of named channels which follow a <strong>publish/subscribe</strong> architecture; any Erlang process can publish or subscribe to any channel, and Erlang term can be sent as a message. Channels need not be explicitly created or destroyed; they are created on demand for publishers or subscribers, and automatically destroyed after a certain (configurable) amount of time. <strong>BossMQ</strong> runs in clustered configurations just as well as a single-machine setup.
</p>

<p>
  <strong>Process</strong>
</p>

<p>
  <a class="fancybox" href="/images/chicagoboss_publish_subscribe.png"><img src="/images/chicagoboss_publish_subscribe.png" width="680" /></a>
</p>
