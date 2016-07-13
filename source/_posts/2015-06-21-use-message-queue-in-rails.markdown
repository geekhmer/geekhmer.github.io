---
layout: post
title: "Use Message Queue in Rails"
date: 2015-06-21 10:53
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Use Message Queue in Rails
---

<p>
  <img src="/images/message_queue .png" width="400" alt="Use Message Queue in Rails" />
</p>

<p>
  This article describes the application architecture pattern which is (in general) nothing new, but (from my experience) rarely applied in the Rails world. I’m talking about the nice and simple abstraction – message queue. But let me start by describing the goals I want to achieve and some alternative solutions.
</p>

<h3>
  Goals
</h3>

<p>
  <strong>Split</strong> application into a few smaller applications<br/>
  Smaller applications are easier to reason about. You don’t have to go through 50 classes, you can just read 10, because it’s all you’ve got. When a new developer joins the team he has nice onboarding if you can tell him: “hey, start with this small piece of code, everything you need to know to implement this new feature is encapsulated here”.
</p>

<p>
  <strong>Separate</strong> code for concepts which are not logically connected<br/>
  Smaller applications are easier to reason about. You don’t have to go through 50 classes, you can just read 10, because it’s all you’ve got. When a new developer joins the team he has nice onboarding if you can tell him: "hey, start with this small piece of code, everything you need to know to implement this new feature is encapsulated here".
</p>

<p>
  Use <strong>new</strong> languages and frameworks<br/>
  We, developers, want to try and learn new languages, libraries, frameworks and technologies. If you make a small application with a shiny new tool and fail – the consequences are less severe, because you can quickly rewrite this small application. If you are going to make one big application, you will think twice before introducing a new tool. So, in some way, smaller applications minimize the risk.
</p>

<h3>
  Solution1 - One database, multiple apps
</h3>

<p>
  This is the very first idea which may come to your mind – just point multiple applications to the one shared database. Been there, done that, won’t do that again! Data is associated with validation logic. Either you duplicate this logic in every app or you extract it to Rails engine gem. Both solutions are hard to maintain (think about running migrations…) and you still have strong coupling in your system.
</p>

<p>
  One case when this approach may work – one read-write app and many read-only apps, but I haven’t tried it.
</p>

<h3>
  Solution2 - Expose REST API
</h3>

<p>
  As Rails devs we are pretty familiar with REST, so we can expose REST API in one of our apps and call this API in the other. This approach has many solid use cases so here I’m just listing some weak points to take into consideration:
</p>

<p>
  - Usually requests in Ruby are blocking – calling app has to wait for the response even if it’s not interested in it.<br/>
  - Requires authentication – we have to somehow ensure that our internal API is not used… well, externally.<br/>
  - Everything happens in server process – if you are calling your internal API you may end up using the same server process which is used for handling requests of your “real users”. You would like to give your “real users” priority.<br/>
  - Calling app has knowledge about receiving app – you have to know which endpoints should be called and which parameters be passed. This introduces coupling.<br/>
</p>

<h3>
  Solution3 - Message queue
</h3>

<p>
  Message queue is a really nice abstraction. Publisher just leaves messages at one end of the "pipe", consumer reads messages from the other end of the "pipe". It is asynchronous, because publisher does not wait for his message to be processed. Moreover, it decouples publisher from consumer, because publisher does not care what happens with his message and who will read it.
</p>

<p>
  This architecture is also resistant to outages, at least when we assume that the queue service rarely breaks. If the consumer is not processing messages, nothing prevents publisher from adding more of them to the queue. When consumer starts to function again, it will process messages from the buffer (if they didn’t take all of your memory).
</p>

<h3>
  When it shines?
</h3>

<p>
  Message queue is really useful if we have some processing which happens out of the main business flow and the main business flow does not have to wait for the results of this processing. The most common example is custom event tracking – own analytics module. We just publish an event and continue execution without slowing anything down.
</p>

<h3>
  RabbitMQ
</h3>

<p>
  RabbitMQ is a popular choice for message queue service, especially in Rails world. Honestly, I haven’t tried different implementations, because RabbitMQ really has everything I need.
</p>

<p>
  There are Ruby gems for communicating with RabbitMQ and it’s also easy to install and configure.
</p>

<p>
  <img src="/images/rabbitmq_concepts.png" width="400" alt="Use Message Queue in Rails" />
</p>

<p>
  In this diagram there are presented some concepts introduced by RabbitMQ. Publisher leaves messages in the exchange. Then they are routed from the exchange to multiple queues. There are many routing algorithms available – <a href="https://www.rabbitmq.com/tutorials/amqp-concepts.html#exchanges" target="_blank">https://www.rabbitmq.com/tutorials/amqp-concepts.html#exchanges</a>
</p>

<p>
  Workers grab messages from queue. If there are multiple workers connected to one queue, they will be load balanced and the message will be delivered only to one of them.
</p>

<h3>
  Easy case
</h3>

<p>
  If you feel overwhelmed – don’t worry. Here is what you should start with:
</p>

<p>
  <img src="/images/rabbitmq_easy_case.png" width="400" alt="Use Message Queue in Rails" />
</p>

<h3>
  Publishing
</h3>

<p>
  Now it’s time for some code. It’s really simple, because integration with RabbitMQ is simple. We will use two gems – <code>bunny</code> and <code>sneakers</code>.
</p>

{% codeblock lang:ruby %}
# Gemfile
gem 'bunny'

# an initializer
connection = Bunny.new(host: 'localhost')
connection.start
channel = connection.create_channel

# a service
class RabbitPublisher

  def initialize(channel)
    self.channel = channel
  end

  def publish(exchange_name, message)
    exchange = channel.fanout(exchange_name, durable: true)
    exchange.publish(message.to_json)
  end

  private
  attr_accessor :channel
end
{% endcodeblock %}

<h3>
  Receiving
</h3>

{% codeblock lang:ruby %}
# Gemfile
gem 'sneakers'

# an initializer
Sneakers.configure  daemonize: true,
                    amqp: "amqp://localhost",
                    log: "log/sneakers.log",
                    pid_path: "tmp/pids/sneakers.pid",
                    threads: 1,
                    workers: 1

# app/workers/events_worker.rb
class EventsWorker
  include Sneakers::Worker
  from_queue "events", env: nil

  def work(raw_event)
    event_params = JSON.parse(raw_event)
    SomeWiseService.build.call(event_params)
    ack!
  end
end
{% endcodeblock %}

<p>
  For details refer to documentation of <a href="https://github.com/ruby-amqp/bunny" target="_blank">bunny</a> and <a href="https://github.com/jondot/sneakers" target="_blank">sneakers</a>.
</p>

<p>
  If you enjoyed this article you can consider ping me for more details.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
