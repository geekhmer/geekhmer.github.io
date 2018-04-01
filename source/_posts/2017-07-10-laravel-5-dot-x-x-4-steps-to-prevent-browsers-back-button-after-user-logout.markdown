---
layout: post
title: "Laravel 5.x.x - 4 Steps to Prevent Browser's Back Button After User Logout"
date: 2017-07-10 23:12
categories: [Laravel]
keywords: Laravel 5.x.x 4 Steps to Prevent Broswer's Back Button After User Logout
---

<p>
  <img src="/images/laravel_news_letter.png" width="600" alt="Laravel 5.x.x 4 Steps to Prevent Browser's Back Button After User Logout" />
</p>

<p>
  Well, have you found out an issue with user logout? If you observe deeply then you can found out this issue that you can logout properly after you click logout link otherwise than if you click on browser's back button you still able to see the content of the page which actually should not be seen with respect to auth middleware process.
</p>

<p>
  We can prevent this issue by using Laravel middleware. We will create one middleware and prevent back button history. So we have to create new middleware and use that middleware in the route.
</p>

<p>
  Like so, I am going to do from scratch so:
</p>

<p>
  <strong>1. Create New Middleware</strong><br/>
  Create a new middleware using following command:
</p>

{% codeblock lang:php %}
php artisan make:middleware PreventBackHistory
{% endcodeblock %}

<p>
  <strong>2. Middleware Configuration</strong><br/>
  Open up <code>PreventBackHistory.php</code> file in <code>app/Http/Middleware</code> folder and replace codes with the following codes below:
</p>

{% codeblock PreventBackHistory.php lang:php %}
<?php

namespace App\Http\Middleware;

use Closure;

class PreventBackHistory {
  /**
   * Handle an incoming request.
   *
   * @param  \Illuminate\Http\Request  $request
   * @param  \Closure  $next
   * @return mixed
   */
  public function handle($request, Closure $next) {
    $response = $next($request);

    return $response->header('Cache-Control','nocache, no-store, max-age=0, must-revalidate')
            ->header('Pragma','no-cache')
            ->header('Expires','Sun, 02 Jan 1990 00:00:00 GMT');
  }
}
{% endcodeblock %}

<p>
  <strong>3. Register Middleware</strong><br/>
  Open <code>Kernel.php</code> in <code>app/Http</code> folder and add a new middleware in $routeMiddleware variable array as below:
</p>

{% codeblock Kernel.php lang:php %}
<?php

namespace App\Http;

use Illuminate\Foundation\Http\Kernel as HttpKernel;

class Kernel extends HttpKernel {
  .....
  .....

  /**
   * The application's route middleware.
   *
   * These middleware may be assigned to groups or used individually.
   *
   * @var array
   */
  protected $routeMiddleware = [
    .....

    'prevent-back-history' => \App\Http\Middleware\PreventBackHistory::class,
  ];

}
{% endcodeblock %}

<p>
  <strong>4. Use Middleware in Route</strong><br/>
  Now we are ready to use "prevent-back-history" middleware in route file as below:
</p>

{% codeblock web.php lang:php %}
Route::group(['middleware' => 'prevent-back-history'],function(){
  Auth::routes();
  Route::get('/home', 'HomeController@index');
});
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
