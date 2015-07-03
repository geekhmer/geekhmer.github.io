---
layout: post
title: "AngularJS on Rails"
date: 2015-06-30 23:43
comments: true
categories: [Javascript, Ruby on Rails]
keywords: AngularJS on Rails, AngularJS, Getting Start with AngularJS and Rails
---

<p>
  <img src="/images/angularjs_on_rails.png" width="400" alt="AngularJS on Rails" />
</p>

<p>
  AngularJS is an extremely popular JavaScript library that enables you to quickly and easily create rich web applications. In this article we will show you how to integrate AngularJS with your Rails app. We will also build a simple AngularJS application called VisitorsCenter. The VisitorsCenter application allows the user to track visitors that are coming and going from a building such as an office building. 
</p>

<p>
  <strong>Setup Rails Application</strong><br/>
  Before we begin, we will need to add a couple gems to our Gemfile. The <code>angularJs-rails</code> gem provides integration with the AngularJS library and our Rails application. The bootstrap-sass gem merely adds bootstrap support so we can focus on the code rather than the style of the app. Add these gems to your gemfile now as listed below.
</p>

{% codeblock Gemfile lang:ruby %}
gem 'angularjs-rails', '~> 1.2.25'
gem 'bootstrap-sass', '~> 3.2.0.2'
{% endcodeblock %}

<p>
  Now run a bundle install to install the gems:
</p>

{% codeblock lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  Next, we need to create a model called <code>Visitor</code>. The <code>Visitor</code> model will represent a visitor that visits. Run the command below to create the visitor model now:
</p>

{% codeblock lang:ruby %}
rails g model Visitor first_name:string last_name:string reason:string
rake db:migrate
{% endcodeblock %}

<p>
  Great, now we need to create a <code>Visitors</code> controller that will give us a way to interact with our model. The <code>Visitors</code> controller will have 3 different actions in this example application. The first action, <code>index</code> will return either the visitors page or a json list of visitors depending on how it is accessed. The second action, <code>create</code> will be responsible for creating the visitor. The final action, <code>destroy</code> will destroy the visitor. Run the command below to create this controller now:
</p>

{% codeblock lang:ruby %}
rails g controller Visitors index create destroy
{% endcodeblock %}

<p>
  Now let's modify our routes file to set up the proper paths and add a site root. Open up your routes file and modify it so that it looks like the code listed below:
</p>

{% codeblock routes.rb lang:ruby %}
Rails.application.routes.draw do
  resources :visitors, only: [:index, :create, :destroy], defaults: {format: :json}
  root to: "visitors#index"
end
{% endcodeblock %}

<p>
  The code fragment that says <code>defaults: {format: :json}</code> tells Rails that we wish to return json by default for our actions. We do this because most of the interaction in our application will be via JSON.
</p>

<p>
  By default, AngularJS knows nothing of the cross site request forgery (CSRF) protections in our applications. We need a way to tell AngularJS how to interact with our application while obeying the CSRF protections that we have in place. Luckily we have a way to do this. Open up your <code>ApplicationController</code> and add in the code listed below.
</p>

<p>
  If you are using Rails 4.2 and up, use the code below:
</p>

{% codeblock application_controller.rb lang:ruby %}
class ApplicationController < ActionController::Base
  protect_from_forgery with: :exception

  after_action :set_csrf_cookie_for_ng

  def set_csrf_cookie_for_ng
    cookies['XSRF-TOKEN'] = form_authenticity_token if protect_against_forgery?
  end

  protected
  def verified_request?
    super || valid_authenticity_token?(session, request.headers['X-XSRF-TOKEN'])
  end
end
{% endcodeblock %}

<p>
  If you are still using Rails 4.1, use the code below instead:
</p>

{% codeblock application_controller.rb lang:ruby %}
class ApplicationController < ActionController::Base
  protect_from_forgery with: :exception

  after_action :set_csrf_cookie_for_ng

  def set_csrf_cookie_for_ng
    cookies['XSRF-TOKEN'] = form_authenticity_token if protect_against_forgery?
  end

  protected
  def verified_request?
    super || form_authenticity_token == request.headers['X-XSRF-TOKEN']
  end
end
{% endcodeblock %}

<p>
  The code listed above will create a cookie called <code>XSRF-TOKEN</code> that will contain our <code>form_authenticity_token</code>. Any time a request is made, AngularJS will present that token in the HTTP headers for the request.
</p>

<p>
  Now let's modify our <code>VisitorsController</code> to allow for access to the <code>Visitor</code> model. Open up your <code>VisitorsController</code> and modify it so that it looks like the code listed below:
</p>

{% codeblock visitors_controller.rb lang:ruby %}
class VisitorsController < ApplicationController
  respond_to :json
  
  def index
    respond_to do |format|
      format.json { render json: Visitor.all }
      format.html
    end
  end

  def create
    respond_with Visitor.create(visitor_params)
  end

  def destroy
    respond_with Visitor.destroy(params[:id])
  end

  private
  def visitor_params
    params.require(:visitor).permit(:first_name, :last_name, :reason)
  end
end
{% endcodeblock %}

<p>
  The code above is typical Rails code, with the exception being that we return JSON as a result. Since our application will be communicating primarily via AJAX we have no need for HTML other than the index action, which will return either html or json depending on the request type.
</p>

<p>
  Next we need to add support for both AngularJS and Bootstrap to our application.js file. Open up your application.js file and modify it so that it looks like the code listed below.
</p>

{% codeblock application.js lang:ruby %}
//= require jquery
//= require jquery_ujs
//= require turbolinks
//= require angular
//= require angular-resource
//= require bootstrap-sprockets
//= require_tree .
{% endcodeblock %}

<p>
  In the code above we add support for AngularJS as well as Bootstrap. We also add support for a library called <code>angular-resource</code> which allows us to easily talk to our Rails application.
</p>

<p>
  Now let's add a bit of CSS for bootstrap. Create a new file called <code>bootstrap_config</code>.scss and add in the code listed below:
</p>

{% codeblock bootstrap_config.scss lang:ruby %}
@import "bootstrap-sprockets";
@import "bootstrap";
{% endcodeblock %}

<p>
  The next thing we need to do is create our AngularJS application. AngularJS applications typically consists of JavaScript code that glues together various bits of HTML. To get started doing this, the first thing we must do is rename our <code>visitors.js.coffee</code> file to <code>visitors.js</code> and modify it so that it looks like the code listed below. You can also rewrite this in CoffeeScript, but I use JavaScript for those that haven't yet learned CoffeeScript.
</p>

{% codeblock visitors.js lang:ruby %}
var visitorCenter = angular.module('VisitorCenter', ['ngResource']);

visitorCenter.factory("Visitor", function($resource) {
  return $resource("visitors/:id", { id: '@id' }, {
    index:   { method: 'GET', isArray: true, responseType: 'json' },
    update:  { method: 'PUT', responseType: 'json' }
  });
})

visitorCenter.controller("visitorsController", function($scope, Visitor) {
  $scope.visitors = Visitor.index()

  $scope.addVisitor = function() {
    visitor = Visitor.save($scope.newVisitor)

    $scope.visitors.push(visitor)
    $scope.newVisitor = {}
  }

  $scope.deleteVisitor = function(index) {
    
    visitor = $scope.visitors[index]
    Visitor.delete(visitor)
    $scope.visitors.splice(index, 1);
  }
})
{% endcodeblock %}

<p>
  There is a lot going on here, so i'm going to break it down into pieces. The first line:
</p>

{% codeblock lang:ruby %}
var visitorCenter = angular.module('VisitorCenter', ['ngResource']);
{% endcodeblock %}

<p>
  defines an AngularJS module. AngularJS modules can be thought of as individual components in your application. You'll notice we include <code>ngResource</code> as an argument. <code>ngResource</code> provides easy access to RESTful resources such as our Rails application.
</p>

<p>
  The next set of lines:
</p>

{% codeblock lang:ruby %}
visitorCenter.factory("Visitor", function($resource) {
  return $resource("visitors/:id", { id: '@id' }, {
    index:   { method: 'GET', isArray: true, responseType: 'json' },
    update:  { method: 'PUT', responseType: 'json' }
  });
})
{% endcodeblock %}

<p>
  defines a service, in this case, it ties in the <code>ngResource</code> service mentioned earlier and tells AngularJS how to talk to our application.
</p>

<p>
  The next set of lines:
</p>

{% codeblock lang:ruby %}
visitorCenter.controller("visitorsController", function($scope, Visitor) {
  $scope.visitors = Visitor.index()

  $scope.addVisitor = function() {
    visitor = Visitor.save($scope.newVisitor)

    $scope.visitors.push(visitor)
    $scope.newVisitor = {}
  }

  $scope.deleteVisitor = function(index) {
    
    visitor = $scope.visitors[index]
    Visitor.delete(visitor)
    $scope.visitors.splice(index, 1);
  }
})
{% endcodeblock %}

<p>
  define a controller. Controllers tell AngularJS how to interact with our application similar to how Rails controllers are used to tell Rails how our views interact with our models.
</p>

<p>
  ow that we've written the JavaScript application, we need to create our view to tie everything together. Open up the index view for your <code>Visitors</code> controller and modify it so that it looks like the code listed below:
</p>

{% codeblock index.html.erb lang:ruby %}
<div class="container" ng-app="VisitorCenter">
  <h1>Visitors</h1>

  <div ng-controller="visitorsController">
    <div class="well">
      <h3>Add a new Visitor</h3>
      <form ng-submit="addVisitor()">
        <div class="row">
          <div class="col-xs-6">
            <input type="text" ng-model="newVisitor.first_name" class="form-control" placeholder="First Name" />
          </div>
          <div class="col-xs-6">
            <input type="text" ng-model="newVisitor.last_name" class="form-control" placeholder="Last Name" />
          </div>
        </div>
        <div class="row">
          <div class="col-xs-12">
            <br />
            <input type="text" ng-model="newVisitor.reason" class="form-control" placeholder="Reason for Visit" />
          </div>
        </div>
        <div class="row">
          <div class="col-xs-12 text-center">
            <br />
            <input type="Submit" value="Add Visitor" class="btn btn-primary" />
          </div>
        </div>
      </form>
    </div>

    <h3>Currently Visiting</h3>
    <hr />
    <table class="table table-bordered table-striped">
      <thead>
        <tr>
          <th>First Name</th>
          <th>Last Name</th>
          <th>Reason for Visit</th>
          <th>&nbsp;</th>
        </tr>
      </thead>
      <tbody>
        <tr ng-show="!visitors.length">
          <td colspan="4">No visitors in the building.</td>
        </tr>
        <tr ng-repeat="visitor in visitors">
          <td>{{ visitor.first_name }}</td>
          <td>{{ visitor.last_name }}</td>
          <td>{{ visitor.reason }}</td>
          <td><a class="btn btn-danger" ng-click="deleteVisitor($index)">Remove</a></td>
        </tr>
      </tbody>
    </table>
  </div>
</div>
{% endcodeblock %}

<p>
  Let's break this down a bit:
</p>

{% codeblock lang:ruby %}
<div class="container" ng-app="VisitorCenter">
  ...
</div>
{% endcodeblock %}

<p>
  The outer div on the first line has an attribute called <code>ng-app</code>. The <code>ng-app</code> attribute tells AngularJS that this is part of our AngularJS application. In this case we specify the name of our AngularJS module, <code>VisitorCenter</code>.
</p>

{% codeblock lang:ruby %}
<div ng-controller="visitorsController">
  ...
</div>
{% endcodeblock %}

<p>
  The next inner div contains an attribute called <code>ng-controller</code>. This attribute tells AngularJS that we wish to use our visitorsController as the controller for this portion of the application.
</p>

{% codeblock lang:ruby %}
<form ng-submit="addVisitor()">
  <div class="row">
    <div class="col-xs-6">
      <input type="text" ng-model="newVisitor.first_name" class="form-control" placeholder="First Name" />
    </div>
    <div class="col-xs-6">
      <input type="text" ng-model="newVisitor.last_name" class="form-control" placeholder="Last Name" />
    </div>
  </div>
  <div class="row">
    <div class="col-xs-12">
      <br />
      <input type="text" ng-model="newVisitor.reason" class="form-control" placeholder="Reason for Visit" />
    </div>
  </div>
  <div class="row">
    <div class="col-xs-12 text-center">
      <br />
      <input type="Submit" value="Add Visitor" class="btn btn-primary" />
    </div>
  </div>
</form>
{% endcodeblock %}

<p>
  The <code>ng-submit</code> attribute on our form tells AngularJS that we wish to use the addVisitor() method on our controller to process the form request. Each of the input elements contain an <code>ng-model</code> attribute. This attribute maps the input elements to our model.
</p>

{% codeblock lang:ruby %}
<tr ng-show="!visitors.length">
  <td colspan="4">No visitors in the building.</td>
</tr>
{% endcodeblock %}

<p>
  The <code>ng-show</code> attribute on the first row tells AngularJS that we only want to show the row if the condition mentioned is matched. In this case we only want to show the first row if there are no visitors.
</p>

{% codeblock lang:ruby %}
<tr ng-repeat="visitor in visitors">
  ...
</tr>
{% endcodeblock %}

<p>
  The <code>ng-repeat</code> attribute is a loop. This particular loop tells AngularJS that we want to loop through each visitor.
</p>


{% codeblock lang:ruby %}
{% raw %}
<td>{{ visitor.first_name }}</td>
<td>{{ visitor.last_name }}</td>
<td>{{ visitor.reason }}</td>
{% endraw %}
{% endcodeblock %}


<p>
  Text contained within {% raw %}<code>{{ .... }}</code>{% endraw %} are AngularJS expressions. In this case we are telling AngularJS to render the fields mentioned in each expression.
</p>


{% codeblock lang:ruby %}
{% raw %}
<td><a class="btn btn-danger" ng-click="deleteVisitor($index)">Remove</a></td>
{% endraw %}
{% endcodeblock %}


<p>
  The <code>ng-click</code> button tells AngularJS to run the specified controller function when the html tag in question is clicked. In this case we run the code to delete the specified user.
</p>

<p>
  So far so good, That's it!!! for this introduction to AngularJS and Rails. See ya!!! :)
</p>
