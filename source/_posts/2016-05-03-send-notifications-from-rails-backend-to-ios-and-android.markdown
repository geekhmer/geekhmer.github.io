---
layout: post
title: "Send Notifications from Rails Backend to iOS and Android"
date: 2016-05-03 12:29
comments: true
categories: [Ruby on Rails]
keywords: Send Notifications from Rails Backend to iOS and Android
---

<p>
  <img src="/images/gcm-app.png" width="600" alt="Send Notifications from Rails Backend to iOS and Android" />
</p>

<p>
  Written for a project. I developed the backend for a social network app - My own startup business, will release soon.
</p>

<p>
  One of the most common uses for a backend connected to a mobile application is to use it to send push notifications to users. Once you've wrapped your head around it, it's pretty straightforward, but it's also a common source of confusion for developers new to the topic. This frequent confusion is also the reason I've decided to cover it for my introductory technical story for my own startup business app, where I'm a CEO.
</p>

<p>
  Before I continue, you should know that there are plug-n-play services that will provide you with a notification-sending backend, if you're willing to pay for it (Parse, mobDB, Pushwoosh, Urban Airship, etc.), but since that's not the way of the warrior, let's see how you do it from scratch (and for free).
</p>

{% blockquote %}
"If you wish to make an apple pie from scratch, you must first invent the universe." - Carl Sagan
{% endblockquote %}

<p>
  There are several components in my own startup business app that play various roles in the notification sending feature:<br/>
  1. API endpoint for getting tokens from mobile devices.<br/>
  2. <a href="https://github.com/resque/resque" target="_blank">Resque</a> worker which remains connected to Apple/Google notification servers and feeds off a queue of notifications in Redis.<br/>
  3. The code that does the actual sending and feedback processing in the worker.
</p>

<p>
  Before anything else, you need to ask the user if they'd like to receive push notifications (<a href="https://developer.apple.com/notifications/" target="_blank">iOS notifications</a>, <a href="http://developer.android.com/guide/topics/ui/notifiers/notifications.html" target="_blank">Google Cloud Messaging</a>) and if they say yes, get their device token and send it from the mobile device to the backend server. We store these tokens in a simple <a href="http://api.rubyonrails.org/classes/ActiveRecord/Base.html" target="_blank">ActiveRecord</a> model called Device:
</p>

{% codeblock lang:ruby %}
# ====================
# Schema Information
# Table name: devices
#  id         :integer          not null, primary key
#  user_id    :integer
#  token      :string(255)
#  enabled    :boolean          default(TRUE)
#  created_at :datetime         not null
#  updated_at :datetime         not null
#  platform   :string(255)
# ====================

class Device < ActiveRecord::Base  
  attr_accessible :enabled, :token, :user, :platform
  belongs_to :user
  validates_uniqueness_of :token, scope: :user_id
end
{% endcodeblock %}

<p>
  Instances of Device get created when the mobile app calls an API endpoint, which looks something like this (we're using <a href="https://github.com/ruby-grape/grape" target="_blank">grape</a> gem for our API needs):
</p>

{% codeblock lang:ruby %}
resource :devices do  
  post do
    @device = Device.create(user: current_user, token: params[:token], platform: params[:platform])
    present @device, with: WellWithMe::Entities::Device
  end
end  
{% endcodeblock %}

<p>
  With our mobile app user having a stored device token, we're now ready to queue notifications for her, which we do through a simple Redis list backed Notification model, which ensures data validity among other things. If a user has multiple devices, the Notification model also ensures they get sent to all of them. Queuing notifications is then as easy as:
</p>

{% codeblock lang:ruby %}
notification = Notification.new(user, "#{activity.user.name} just started the challenge!", 'status_change')
{% endcodeblock %}

<p>
  In an essence, the Notification model is a Redis list, which serves as the queue for a background worker (NotificationSender):
</p>

{% codeblock lang:ruby %}
class NotificationSender
  @queue = :notifications

  def self.perform
    @list = Redis::List.new(Notification.key_name)
    while notification = @list.pop do
      notification_json = JSON.parse(notification)
      if notification_json['platform'] == 'iOS'
        note = Grocer::Notification.new(
          device_token: notification_json['token'],
          alert: notification_json['message'],
          sound: 'default',
          badge: 0
        )

        PUSHER.push(note)
      elsif notification_json['platform'] == 'Android'
        gcm = GCM.new(ENV['gcm_key'])
        registration_id = [notification_json['token']]
        options = {
          'data' => {
            'message' => notification_json['message']
          },
            'collapse_key' => 'updated_state'
        }
        response = gcm.send_notification(registration_id, options)
      end
    end
  end
end
{% endcodeblock %}

<p>
  Having NotificationSender be a queued job constantly running on a worker as opposed to a synchronous connection has the advantage of not trying to establish a connection to Apple's notification servers for every notification, which is something Apple actively discourages: <a href="https://developer.apple.com/news/?id=03212012a" target="_blank">Apple's note about notification servers connections</a>.
</p>

<p>
  Thus NotificationSender is a Resque job, which is run every minute and just gobbles up the Redis list of notifications, and sends them according to the platform. We're using the awesome <a href="https://github.com/grocer/grocer" target="_blank">grocer</a> gem for iOS and the <a href="https://github.com/spacialdb/gcm" target="_blank">GCM</a> gem for Android. Both are working perfectly and the documentation is very good. The only caveat is that you should watch out for certificates magic in iOS, as you need to have your Apple certificates on the server as well, and you need to export them in a certain way (including the private key) - just follow instructions for the grocer gem to the letter, and you'll be fine.
</p>

<p>
  With this, you should have a working and easily extendible system for sending important notifications about in-app events to your users. Notifications can drastically increase user retention, just don't abuse them or they'll have an opposite effect.
</p>
