---
layout: post
title: "Laravel 5.x.x Template"
date: 2017-08-06 00:40
comments: true
categories: [Laravel]
keywords: Laravel 5.x.x Template
---

<p>
  <img src="/images/laravel_news_letter.png" width="600" alt="Laravel 5.x.x Template" />
</p>

<p>
  Blade is a powerful easy to use template that comes with Laravel. Blade templates can be mixed with plain php code.
</p>

<p>
  Well, in this articles I will cover the following sections: Template inheritance, Master layout, Extending the master layout, Displaying variables, Blade conditional statements, Blade Loops and Executing PHP functions in blade template.
</p>

<p>
  <strong>Template Inheritance</strong><br/>
  In a nutshell, template inheritance allows us to define a master layout with elements that are common to all web pages. The individual pages extend the master layout. This saves us time of repeating the same elements in the individual pages.
</p>

<p>
  <strong>Master Layout</strong><br/>
  All blade templates must be saved with the .blade extension. In this section, we are going to create a master template that all pages will extend. The following is the syntax for defining a master layout.<br/>
</p>

<p>
  Create a new file named <code>master.blade.php</code> in <code>/resources/views/layouts</code> folder with the following code below:
</p>

{% codeblock master.blade.php lang:php %}
<html>
  <head>
    <title>@yield('title')</title>
  </head>
  <body>
    @section('sidebar')
      Here is the master sidebar.
    @show

    <div class="container">
      @yield('content')
    </div>
  </body>
</html>
{% endcodeblock %}

<p>
  - <code>@yield('title')</code> is used to display the value of the title.<br/>
  - <code>@section('sidebar')</code> is used to define a section named sidebar.<br/>
  - <code>@show</code> is used to display the contents of a section.<br/>
  - <code>@yield('content')</code> is used to display the contents of content.
</p>

<p>
  <strong>Extending the Master Layout</strong><br/>
  Now we will create a page that extends the master layout. Create a new page named <code>page.blade.php</code> in <code>/resources/views</code> folder with the following code below:
</p>

{% codeblock page.blade.php lang:php %}
@extends('layouts.master')

@section('title', 'Page Title')

@section('sidebar')
  <p>Here is appended to the master sidebar.</p>
@endsection

@section('content')
  <p>Here is my body content.</p>
@endsection
{% endcodeblock %}

<p>
  - <code>@extends('layouts.master')</code> is used to extends the master layout.<br/>
  - <code>@section('title', 'Page Title')</code> is used to sets the value of the title section.<br/>
  - <code>@section('sidebar')</code> is used to defines a sidebar section in the child page of master layout.<br/>
  - <code>@endsection</code> is used to ends the sidebar section.<br/>
  - <code>@section('content')</code> is used to defines the content section.<br/>
</p>

<p>
  And now we will add a route to tests our blade template. Open up <code>/routes/web.php</code> file and add the following route below:
</p>

{% codeblock web.php lang:php %}
Route::get('blade', function () {
  return view('page');
});
{% endcodeblock %}

<p>
  Load the <code>http:://localhost:8000/blade</code> URL in your web browser and you will see the paragraph.
</p>

<p>
  <strong>Displaying Variables in a Blade Template</strong><br/>
  Now we will define a variable and pass it to our blade template view. Open up <code>/routes/web.php</code> file and add the route below:
</p>

{% codeblock web.php lang:php %}
Route::get('blade', function () {
  return view('page',array('name' => 'The Foodie'));
});
{% endcodeblock %}

<p>
  And then update <code>pages.blade.php</code> file to display the variable. Open up <code>page.blade.php</code> file and update the contents to the following:
</p>

{% codeblock page.blade.php lang:php %}
@extends('layouts.master')

@section('title', 'Page Title')

@section('sidebar')
  <p>Here is appended to the master sidebar.</p>
@endsection
@section('content')
  <h2>{{$name}}</h2>   
  <p>Here is my body content.</p>
@endsection
{% endcodeblock %}

<p>
  <code>{% raw %}{{$name}}{% endraw %}</code> double opening curly braces and double closing curly braces are used to display the value of $name variable.
</p>

<p>
  <strong>Blade Condition Statements</strong><br/>
  Blade also supports conditional statements. Conditional statements are used to determine what to display in the browser. We will pass a variable that will determine what to display in the browser.
</p>

<p>
  Open up <code>/routes/web.php</code> file and modify route as follow:
</p>

{% codeblock web.php lang:php %}
Route::get('blade', function () {
  return view('page', array('name' => 'The Foodie', 'day' => 'Sunday'));
});
{% endcodeblock %}

<p>
  We added another variable <code>day</code> with a value of Sunday.
</p>

<p>
  And then open up <code>/resources/views/page.blade.php</code> file and modify the codes to the following:
</p>

{% codeblock page.blade.php lang:php %}
@extends('layouts.master')

@section('title', 'Page Title')

@section('sidebar')
  <p>Here is appended to the master sidebar.</p>
@endsection

@section('content')
  <h2>{{$name}}</h2>    
  <p>Here is my body content.</p>
  <h2>If Statement</h2>
  @if ($day == 'Sunday')
    <p>Time to party</p>
  @else
    <p>Time to make money</p>
  @endif
@endsection
{% endcodeblock %}

<p>
  - <code>@if ($day == 'Sunday')</code> starts the if statement and evaluates the condition $day == 'Sunday'.<br/>
  - <code>@else</code> is the else part of the if statement.<br/>
  - <code>@endif</code> ends the if statement.
</p>

<p>
  <strong>Blade Loop</strong><br/>
  Blade template supports all of the loops that PHP supports. We will look at how we can use the foreach loop in blade to loop through an array of items.
</p>

<p>
  Open up <code>/routes/web.php</code> file and modify the codes for the blade route to the following:
</p>

{% codeblock web.php lang:php %}
Route::get('blade', function () {
  $drinks = array('Vodka', 'Gin', 'Brandy');
  return view('page', array('name' => 'The Foodie','day' => 'Sunday', 'drinks' => $drinks));
});
{% endcodeblock %}

<p>
  <code>$drinks = array('Vodka', 'Gin', 'Brandy');</code> defines an array variable that we are passing to the blade template.
</p>

<p>
  And then open up <code>/resources/views/page.blade.php</code> file and modify the contents to the following:
</p>

{% codeblock page.blade.php lang:php %}
@extends('layouts.master')

@section('title', 'Page Title')

@section('sidebar')
  <p>Here is appended to the master sidebar.</p>
@endsection

@section('content')
  <h2>{{$name}}</h2>    
  <p>Here is my body content.</p>
  <h2>If Statement</h2>
  @if ($day == 'Sunday')
    <p>Time to party</p>
  @else
    <p>Time to make money</p>
  @endif
  <h2>Foreach Loop</h2>
  @foreach ($drinks as $drink)
    <p>{{$drink}}</p>
  @endforeach
@endsection
{% endcodeblock %}

<p>
  <strong>Executing php functions in Blade</strong><br/>
  We will call the php date function in the blade template. Open up <code>/resources/views/page.blade.php</code> file and modify the contents to the following:
</p>

{% codeblock page.blade.php lang:php %}
@extends('layouts.master')

@section('title', 'Page Title')

@section('sidebar')
  <p>Here is appended to the master sidebar.</p>
@endsection

@section('content')
  <h2>{{$name}}</h2>    
  <p>Here is my body content.</p>
  <h2>If Statement</h2>
  @if ($day == 'Sunday')
    <p>Time to party</p>
  @else
    <p>Time to make money</p>
  @endif
  <h2>Foreach Loop</h2>
  @foreach ($drinks as $drink)
    <p>{{$drink}}</p>
  @endforeach

  <h2>Execute PHP Function</h2>
  <p>The date is {{date(' D M, Y')}}</p>
@endsection
{% endcodeblock %}

<p>
  <code>{% raw %}{{date(' D M, Y')}}{% endraw %}</code> double opening and closing curly braces are used to execute the php date function.
</p>
