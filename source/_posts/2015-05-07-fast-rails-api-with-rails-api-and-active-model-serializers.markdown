---
layout: post
title: "Fast Rails API with Rails-api and Active_model_serializers"
date: 2015-05-07 13:50
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Fast Rails API with Rails-api and Active_model_serializers
---

<p>
  <img src="/images/happy_ruby_on_rails.jpg" width="400" alt="Fast Rails API with Rails-api and Active_model_serializers" />
</p>

<p>
  Dealing with JSON in Rails is pretty easy and straight forward. The support is built into the framework, request data is automatically available in params hash. Then we just have to say <code>render :json</code> and we are done.
</p>

<p>
  If JSON is one of the formats which we use. For instance, we can render both JSON and HTML in the same controller action. Or we may create an action intended for AJAX call which changes some model and returns JSON with status field.
</p>

<p>
  On the other hand, we may use Ruby on Rails for building JSON API – app with the assumption that JSON is the only format we support. In such a case we know that the app won’t render HTML. There is no View layer (or you can say that JSON is your View layer). We probably won’t need cookies support and the code specific to handling browser requests.
</p>

<h3>
  Rails-API
</h3>

<p>
  Ruby on Rails has modular structure – you can opt out of the things which you don’t want to use. But if we are talking about API only Rails app already did it for us and published in the gem called <a href="https://github.com/rails-api/rails-api/blob/master/README.md" target="_blank">rails-api</a>.
</p>

<p>
  What I care is "Don’t require things which you won’t use". There might be bugs hiding there or security vulnerabilities connected with it. And of course there are other reasons:
</p>


<p>
  <strong>More Lightweight Application</strong><br/>
  I generated two fresh applications – one using <code>rails-api new</code> and the second using <code>rails new</code>. The environment was consistent – Ruby 2.1.0, Rails 4.0.2. Then I started the server with rails s and measured RAM memory taken by server process. The results are following:
</p>

<p>
  This is 15% less. Obviously when you start adding new gems the relative difference will be smaller.
</p>

<p>
  <strong>Faster Application</strong><br/>
  The same benchmarking environment as above. I created controller action which loads 100 User records from the database and renders them as JSON. I placed exactly the same code in rails-api application and regular rails application. Then I measured the server response time for a bunch of requests and took the avarage value. It looks like:
</p>

<p>
  This is 12% faster.
</p>

<p>
  <strong>Useful Generator</strong><br/>
  The controller scaffold which comes with rails-api is really cool. It disables new and edit actions (they don’t make sense because we do not display forms) and provides sensible default code inside other actions.
</p>

<p>
  <strong>Easy Installation and No Maintenance Costs</strong><br/>
  Last but not least, rails-api is super easy to install and learn. All you have to do to bootstrap new app is:
</p>

{% codeblock lang:ruby %}
gem install rails-api
rails-api new my_app
{% endcodeblock %}

<p>
  It’s worth to spend 10 minutes of your time for learning it. <a href="https://github.com/rails-api/rails-api/blob/master/README.md" target="_blank">Check out the docs</a>.
</p>

<h3>
  Active Model Serializers
</h3>

<p>
  If you want to build JSON API it’s good to have control over the generated JSON. And here we need the second gem is <a href="https://github.com/rails-api/active_model_serializers" target="_blank">active_model_serializers</a>.
</p>

<p>
  Let’s look at a sample serializer:
</p>

{% codeblock app/serializers/user_serializer.rb lang:ruby %}
class UserSerializer < ActiveModel::Serializer
  attributes :id, :first_name, :last_name, :email

  has_one :address
  has_many :packages
end
{% endcodeblock %}

<p>
  As you can see UserSerializer is used to serialize User model. Using attributes we define what should be included in the generated JSON. You may think that this is repeating yourself (the fields are already defined in database schema), but in my opinion you rarely render all the fields, more often you want to hide some internals.
</p>

<p>
  As you can see we embed associated records using familiar syntax: has_one and has_many. Serializing Address and Package will be handled using AddressSerializer and PackageSerializer, respectively.
</p>

{% codeblock app/serializers/user_serializer.rb lang:ruby %}
class UserSerializer < ActiveModel::Serializer
  [...]

  attributes :full_name, :email_address

  def full_name
    "#{object.first_name} #{object.last_name}"
  end

  def email_address
    object.email
  end

  [...]
end
{% endcodeblock %}

<p>
  Serializers are just classes which inherit from ActiveModel::Serializer. You can define regular methods and the model being serialized is accessed by object method.
</p>

<p>
  How does the code inside UsersController look like?
</p>

{% codeblock app/controllers/users_controller.rb lang:ruby %}
[...]

  def index
    @users = User.includes(:address, :packages)

    render json: @users
  end

[...]
{% endcodeblock %}

<p>
  Pretty simple, You just say: “I want JSON” and you have JSON rendered with proper serializer.
</p>

{% codeblock lang:ruby %}
{
  "users":
    [
      {
        "id": 1,
        "first_name": "Some String",
        "last_name": "Another String",
        [...]
        "address": { "street": "Yet another string" },
        "packages": [
          { "id": 2, "status": "delivered" },
          { "id": 5, "status": "lost" }
        ]
      }
    ]
}
{% endcodeblock %}

<p>
  <strong>More about Associations</strong><br/>
  Let’s imagine following use case, we want to get information about a package given its id. And we want it to contain information about the owner (user).
</p>

{% codeblock package_serializer.rb lang:ruby %}
class PackageSerializer < ActiveModel::Serializer
  attributes :id, :status

  has_one :user
end
{% endcodeblock %}

<p>
  What’s different here from model code? Package <code>belongs_to</code> user, but here it says <code>has_one</code> user. From the point of view of serializers <code>belongs_to</code> is exactly the same as <code>has_one</code>, hence we have only has_one and has_many.
</p>

<p>
  Now let’s go back to <code>UsersController#index</code> after our changes. We hit the action again and what do we get this time?
</p>

{% codeblock lang:ruby %}
Failure/Error: Unable to find matching line from backtrace
  SystemStackError:
    stack level too deep
{% endcodeblock %}

<p>
  That’s an infinite loop: user contains package, package contains user, user contains package... How can we solve this?
</p>

<p>
  <strong>Solution 1</strong>
</p>

{% codeblock user_serializer.rb lang:ruby %}
class UserSerializer < ActiveModel::Serializer
  [...]

  has_many :packages, :embed => :ids

  [...]
end
{% endcodeblock %}

{% codeblock lang:ruby %}
{
  "users":
    [
      {
        "id": 1,
        [...]
        "package_ids": [2, 5]
      }
    ]
}
{% endcodeblock %}

<p>
  We can use <code>:embed</code> option and include only ids of the packages. This has a drawback: if a client of our API wants not only id of the package, but also its status then he will have to make a separate request for each package. Certainly this is a situation which we want to avoid.
</p>

<p>
  <strong>Solution 2</strong>
</p>

{% codeblock user_serializer.rb lang:ruby %}
class UserSerializer < ActiveModel::Serializer
  [...]

  has_many :packages, :serializer => ShortPackageSerializer

  [...]
end
{% endcodeblock %}

{% codeblock short_package_serializer.rb lang:ruby %}
class ShortPackageSerializer < ActiveModel::Serializer
  attributes :id, :status
end
{% endcodeblock %}

<p>
  We use <code>:serializer</code> option to specify different serializer than that inferred from naming conventions. We create <code>ShortPackageSerializer</code>, which doesn’t contain user embeded. What’s more you can put <code>ShortPackageSerializer</code> and <code>PackageSerializer</code> in the inheritance hierarchy so you DRY.
</p>

<p>
  In my opinion this solution is pretty clean. We have separate class for each representation and we are able to share some pieces of code by using inheritance. Of course, this may become too complex if the inheritance hierarchy grows very big. However if we limit ourselves to 2-3 serializers per model the code should stay clear and maintainable.
</p>

<p>
  Use rails-api if you are building API-only application. It’s easy to learn and maintenance won’t hit you later, because you can opt out without any problems.
</p>

<p>
  Use active_model_serializers for better control over the generated JSON.
</p>
  