---
layout: post
title: "Laravel 5.x.x Create Custom Helper"
date: 2017-07-02 20:08
categories: [Laravel]
keywords: Laravel 5.x.x Create Custom Helper, Laravel 5 Create Custom Helper, Laravel Create Custom Helper
---

<p>
  <img src="/images/laravel_5.jpg" width="600" alt="Laravel 5.x.x Create Custom Helper" />
</p>

<p>
  In this article I will show you how to create your own custom helpers in Laravel framework.
</p>

<p>
  <strong>Create Project</strong><br/>
  Run the following composer command to create a new Laravel project:
</p>

{% codeblock lang:php %}
composer create-project laravel/laravel laravel_helper
{% endcodeblock %}

<p>
  <strong>Customer Helpers’ Dir</strong><br/>
  Customer helpers files will be located in the <code>app</code> dir.
</p>

<p>
  Create a new directory Helpers in <code>app/Helpers</code>
</p>

<p>
  <strong>Define Helper Class</strong><br/>
  Let’s create a simple helper function that will return the user's full name format.
</p>

<p>
  Create a new file <code>UserHelper.php</code> in <code>app/Helpers</code> and add the following codes:
</p>

{% codeblock UserHelper.php lang:php %}
<?php

namespace App\Helpers;

class UserHelper {
  public static function full_name($first_name, $last_name) {
    return $first_name . ', '. $last_name;   
  }
}
{% endcodeblock %}

<p>
  - <code>namespace App\Helpers;</code>: defines the Helpers namespace.<br/>
  - <code>public static function full_name($first_name, $last_name) {...}</code>: defines a static function which return the user's full name.
</p>

<p>
  <strong>Helpers Service Provider Class</strong><br/>
  Service providers are used to auto load classes in Laravel framework. 
</p>

<p>
  We will need to define a service provider that will load all of our helpers classes in <code>app/Helpers</code> directory.
</p>

<p>
  Run the following artisan command to create <code>HelperServiceProvider.php</code> in <code>app/Providers</code> directory:
</p>

{% codeblock lang:php %}
php artisan make:provider HelperServiceProvider
{% endcodeblock %}

<p>
  And then add the following code below in <code>HelperServiceProvider.php</code> file:
</p>

{% codeblock HelperServiceProvider.php lang:php %}
<?php 

namespace App\Providers;

use Illuminate\Support\ServiceProvider;

class HelperServiceProvider extends ServiceProvider {

  /**
   * Bootstrap the application services.
   *
   * @return void
   */
  public function boot()
  {
    //
  }

  /**
   * Register the application services.
   *
   * @return void
   */
  public function register()
  {
    foreach (glob(app_path().'/Helpers/*.php') as $filename){
      require_once($filename);
    }
  }
}
{% endcodeblock %}

<p>
  - <code>namespace App\Providers;</code>: defines the namespace provider.<br/>
  - <code>use Illuminate\Support\ServiceProvider;</code>: imports the ServiceProvider class namespace.<br/>
  - <code>class HelperServiceProvider extends ServiceProvider {...}</code>: defines a HelperServiceProvider class that extends/inherite the ServiceProvider class.
  - <code>public function register() {...}</code> is the function that is used to loads the helpers.<br/>
  - <code>foreach (glob(app_path().'/Helpers/*.php') as $filename) {...}</code>: loops through all the files in <code>app/Helpers</code> directory and loads them.
</p>

<p>
  <strong>Configure Helper Service Provider and Class Alias</strong><br/>
  We need to register the HelperServiceProvider and create an alias for the helpers.
</p>

<p>
  Open up <code>config/app.php</code> file and add the following line in providers array variable.
</p>

{% codeblock app.php lang:php %}
App\Providers\HelperServiceProvider::class,
{% endcodeblock %}

<p>
 And then add the following line in aliases array variable.
</p>

{% codeblock app.php lang:php %}
'UserHelper' => App\Helpers\UserHelper::class,
{% endcodeblock %}

<p>
  <strong>Using the Custom Helper</strong><br/>
  Let create a route that will the custom helper function. Open up <code>routes/web.php</code> and add the following codes:
</p>

{% codeblock web.php lang:php %}
Route::get('/users', function () {
  return UserHelper::full_name("Bunlong", "Van");
});
{% endcodeblock %}

<p>
  - <code>return UserHelper::full_name("Bunlong", "Van");</code> calls the static function full_name in UserHelper class.
</p>

<p>
  Open up your browser and type the uri <code>http://localhost:8000/users</code> you will see "Bunlong, Van" text.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
