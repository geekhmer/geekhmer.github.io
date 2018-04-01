---
layout: post
title: "Hello Laravel 5.x.x"
date: 2017-06-30 21:01
categories: [Laravel]
keywords: Laravel, Hello Laravel 5.x.x, Hello Laravel
---

<p>
  <img src="/images/laravel_5.jpg" width="600" alt="Hello Laravel 5.x.x" />
</p>

<p>
  In the previous article, We installed and configured a Laravel application. And in this article We will build on the same project to create a simple Hello Laravel application and look at the key components of Laravel framework.
</p>

<p>
  <strong>Artisan Command Line</strong><br/>
  Artisan is the command line that automates common tasks in Laravel framework. The artisan command line can be used to perform the following tasks and much more:
</p>

<p>
  - Generate boilerplate code – it easily create controllers, models... etc.<br/>
  - Database migrations – migrations is used to manipulate database objects and can be used to create and drop tables etc.<br/>
  - Seeding – seeding is a term used to add dummy records to the database.<br/>
  - Routing<br/>
  - Run unit tests.
</p>

<p>
  <strong>The Way to Use the Artisan Command</strong><br/>
  Open the terminator and run the following command to view the list of available commands:
</p>

{% codeblock lang:php %}
php artisan list
{% endcodeblock %}

<p>
  <strong>Artisan Command To Generate Codes for a Controller</strong><br/>
  Open the terminator and run the following command to generate codes for Hello Laravel controller:
</p>

{% codeblock lang:php %}
php artisan make:controller HelloLaravelController
{% endcodeblock %}

<p>
  - <code>php artisan</code> is used to run the artisan command line.<br/>
  - <code>make:controller HelloLaravelController</code> specifies the command that the should run. This command will create codes for a controller HelloLaravelController in /app/Http/Controllers/HelloLaravelController.php.
</p>

<p>
  And then open up the file <code>HelloLaravelController.php</code> in folder <code>/app/Http/Controllers</code>.
</p>

<p>
  And you will get the following code:
</p>

{% codeblock HelloLaravelController.php lang:php %}
<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use App\Http\Requests;
use App\Http\Controllers\Controller;

class HelloLaravelController extends Controller
{
  /**
   * Display a listing of the resource.
   *
   * @return Response
   */
  public function index()
  {
    //
  }

  /**
   * Show the form for creating a new resource.
   *
   * @return Response
   */
  public function create()
  {
    //
  }

  /**
   * Store a newly created resource in storage.
   *
   * @param  Request  $request
   * @return Response
   */
  public function store(Request $request)
  {
    //
  }

  /**
   * Display the specified resource.
   *
   * @param  int  $id
   * @return Response
   */
  public function show($id)
  {
    //
  }

  /**
   * Show the form for editing the specified resource.
   *
   * @param  int  $id
   * @return Response
   */
  public function edit($id)
  {
    //
  }

  /**
   * Update the specified resource in storage.
   *
   * @param  Request  $request
   * @param  int  $id
   * @return Response
   */
  public function update(Request $request, $id)
  {
    //
  }

  /**
   * Remove the specified resource from storage.
   *
   * @param  int  $id
   * @return Response
   */
  public function destroy($id)
  {
    //
  }
}
{% endcodeblock %}

<p>
  - <code>namespace App\Http\Controllers;</code>: defines the namespace for the controller.<br/>
  - <code>use Illuminate\Http\Request;</code>: imports namespaces with the required classes to use in the controller.<br/>
  - <code>class HelloLaravelController extends Controller</code>: defines the HelloLaravelController class which extends/inherit the base controller.<br/>
  - <code>public function index(){}</code>: defines the default function for the controller.<br/>
  - <code>public function create(){}</code>: defines the function that is used to render the create form view.<br/>
  - <code>public function store(Request $request)</code>: defines the function that is used to store/save a newly recode into the table/database.<br/>
  - <code>public function show($id)</code>: defines the function that is used to retrieves a single recode/resource based on the id.<br/>
  - <code>public function edit($id)</code>: defines the function that is used to render the edit form based on the id.<br/>
  - <code>public function update(Request $request, $id)</code> defines a function that is used to update a record in the table/database base on the id.<br/>
  - <code>public function destroy($id)</code>: defines the function that is used to remove a recode based on the id.
</p>

<p>
  <strong>Routing</strong><br/>
  We will create a new route that will render Hello Laravel in the browser.
</p>

<p>
  Open up file <code>web.php</code> in folder <code>routes</code> and add the following codes below:
</p>

{% codeblock web.php lang:php %}
Route::get('/hello_laravel',function(){
  return 'Hello Laravel!';
});
{% endcodeblock %}

<p>
  <code>Route::get('/hello',function(){...});</code>: responds to the GET method of the URI hello. function() defines an anonymous function that does the actual work for the requested URI.<br/>
  <code>return 'Hello Laravel!';</code>: returns and render Hello Laravel! to the requested browser.
</p>

<p>
  And then go to ther browser and type the uri <code>http://localhost:8000/hello</code> you will get the output "Hellow Laravel!".
</p>

<p>
  <strong>Route To Controller</strong><br/>
  Add the following codes in <code>routes/web.php</code>.
</p>

{% codeblock web.php lang:php %}
Route::get('hello', 'HelloLaravelController@index');
{% endcodeblock %}

<p>
  And then open up <code>app/Http/Controllers/HelloLaravelController.php</code> file and add the following codes below:
</p>

{% codeblock HelloLaravelController.php lang:php %}
public function index()
{
  return 'Hello Laravel!';
}
{% endcodeblock %}

<p>
  And then go to ther browser and type the uri <code>http://localhost:8000/hello</code> you will get the output "Hello Laravel!".
</p>

<p>
  <strong>Loading the View from the Controller</strong><br/>
  Open up <code>app/Http/Controllers/HelloLaravelController.php</code> file and edit the following codes below:
</p>

{% codeblock HelloLaravelController.php lang:php %}
public function index()
{
  return view('home');
}
{% endcodeblock %}

<p>
  <code>return view('home');</code>: loads a view named hello.blade.php.
</p>

<p>
  And then create a new file <code>home.blade.php</code> in folder <code>/resources/views</code> and add the following codes below:
</p>

{% codeblock home.blade.php lang:php %}
Hello Laravel!
{% endcodeblock %}

<p>
  And then go to ther browser and type the uri <code>http://localhost:8000/hello</code> you will get the output "Hello Laravel!".
</p>  

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
