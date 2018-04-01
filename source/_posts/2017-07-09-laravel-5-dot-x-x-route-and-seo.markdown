---
layout: post
title: "Laravel 5.x.x Route &amp; SEO"
date: 2017-07-09 10:49
categories: [Laravel]
keywords: Laravel 5.x.x Route and SEO, Laravel 5 Route and SEO, Laravel Route and SEO, Route and SEO, Route, SEO
---

<p>
  <img src="/images/laravel_news_letter.png" width="600" alt="Laravel 5.x.x Route and SEO" />
</p>

<p>
  SEO stands for "search engine optimization". URLs is an important thing in getting found on the web. In this article I will implement routes and SEO friendly URLs for Laravel project.
</p>

<p>
  <strong>Things that Affect SEO</strong><br/>
  The following are some of the things that search engines such as Google Search consider when evaluating web sites:<br/>
  1. Website speed<br/>
  - No one waiting to visit a websites that take forever to load. We all love fast websites. The goal should be to keep the load time under 2 seconds. If you can get it under a second that is even much better. You need to test your web application for speed and optimize if necessary.<br/>
  2. Responsive designs<br/>
  - Mobile devices have a market share of internet usage. Since user experience matters to search engines, you need to ensure that the web site displays properly in mobile devices, tablets and desktops as well.<br/>
  3. Keywords<br/>
  - Search engines look at keywords when querying billions of indexed websites. As a developer you have to ensure that you provide title tags, meta description and HTML H2 heading that the content writers can use to place keywords.<br/>
  4. Social media statistics<br/>
  - If you read something cool on the web, you naturally share it on social media. This is a stamp of approval to search engines. Your have to include tools on the web site that will make it easy for the visitors to share the contents.<br/>
  5. Website URLs<br/>
  - The URLs should be keyword rich and words should be separated by dashes and not underscores.
</p>

<p>
  <strong>How to Implement SEO Friendly URLS in Laravel</strong><br/>
  Now we have to cover the basics SEO and we will map routes to controllers and create a single controller for all routes. The following table shows the URLs that will be implemented:
</p>

<table>
  <thead>
    <tr>
      <th>#</th>
      <th>URLs</th>
      <th>Method</th>
      <th>Description</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>1</td>
      <td>/</td>
      <td>index</td>
      <td>Home page</td>
    </tr>
    <tr>
      <td>2</td>
      <td>/products</td>
      <td>products</td>
      <td>Products page</td>
    </tr>
    <tr>
      <td>3</td>
      <td>/products/details/{id}</td>
      <td>product_details(id)</td>
      <td>Product detailed based on product id</td>
    </tr>
    <tr>
      <td>4</td>
      <td>/products/category</td>
      <td>product_categories</td>
      <td>Product categories</td>
    </tr>
    <tr>
      <td>5</td>
      <td>/products/brands</td>
      <td>product_brands</td>
      <td>Product brands</td>
    </tr>
    <tr>
      <td>6</td>
      <td>/blog</td>
      <td>blog</td>
      <td>Blog postings list</td>
    </tr>
    <tr>
      <td>7</td>
      <td>/blog/post/{id}</td>
      <td>blog_post{id}</td>
      <td>Blog post content</td>
    </tr>
    <tr>
      <td>8</td>
      <td>/contact-us</td>
      <td>contact_us</td>
      <td>Contact us page</td>
    </tr>
    <tr>
      <td>9</td>
      <td>/login</td>
      <td>login</td>
      <td>Login user</td>
    </tr>
    <tr>
      <td>10</td>
      <td>/logout</td>
      <td>logout</td>
      <td>Logout user</td>
    </tr>
    <tr>
      <td>11</td>
      <td>/cart</td>
      <td>cart</td>
      <td>Cart contents</td>
    </tr>
    <tr>
      <td>12</td>
      <td>/checkout</td>
      <td>checkout</td>
      <td>Checkout shopper</td>
    </tr>
    <tr>
      <td>13</td>
      <td>/search/{query}</td>
      <td>search</td>
      <td>Search results</td>
    </tr>
  </tbody>
</table>

<br/>

<p>
  For this section assumes you have created the tutorial project. If you haven’t done so yet then read this <a href="http://geekhmer.github.io/blog/2017/06/30/hello-laravel-5-dot-x-x/">Laravel Hello World</a>. We use the artisan command line tool to generate the codes for ShopController.php controller.
</p>

<p>
  Then open up your terminator and run the following command to browse to the project. Assumed that you are using Laravel plugin web server. 
</p>

{% codeblock lang:php %}
php artisan serve
{% endcodeblock %}

<p>
  Then run the following command to generate the Shop controller:
</p>

{% codeblock lang:php %}
php artisan make:controller ShopController
{% endcodeblock %}

<p>
  Open up /app/Http/Controllers/ShopController.php and replace the generated codes with the following codes below:
</p>

{% codeblock ShopController.php lang:php %}
<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use App\Http\Controllers\Controller;

class ShopController extends Controller {

  public function index() {
    return 'index page';
  }

  public function products() {
    return 'products page';
  }

  public function product_details($id) {
    return 'product details page';
  }

  public function product_categories() {
    return 'product categories page';
  }

  public function product_brands() {
    return 'product brands page';
  }

  public function blog() {
    return 'blog page';
  }

  public function blog_post($id) {
    return 'blog post page';
  }

  public function contact_us() {
    return 'contact us page';
  }

  public function login() {
    return 'login page';
  }

  public function logout() {
    return 'logout page';
  }

  public function cart() {
    return 'cart page';
  }

  public function checkout() {
    return 'checkout page';
  }

  public function search($query) {
    return "$query search page";
  }
}
{% endcodeblock %}

<p>
  The above code defines functions that will responds to the routes.
</p>

<p>
  And then we will add routes that will call the methods in the controllers.
</p>

<p>
  Open up <code>web.php</code> in /routes folder and replace the code with the following:
</p>

{% codeblock web.php lang:php %}
<?php

/*
|--------------------------------------------------------------------------
| Web Routes
|--------------------------------------------------------------------------
|
| Here is where you can register web routes for your application. These
| routes are loaded by the RouteServiceProvider within a group which
| contains the "web" middleware group. Now create something great!
|
*/

Route::get('/','ShopController@index');
Route::get('/products','ShopController@products');
Route::get('/products/details/{id}','ShopController@product_details');
Route::get('/products/categories','ShopController@product_categories');
Route::get('/products/brands','ShopController@product_brands');
Route::get('/blog','ShopController@blog');
Route::get('/blog/post/{id}','ShopController@blog_post');
Route::get('/contact-us','ShopController@contact_us');
Route::get('/login','ShopController@login');
Route::get('/logout','ShopController@logout');
Route::get('/cart','ShopController@cart');
Route::get('/checkout','ShopController@checkout');
Route::get('/search/{query}','ShopController@search');
{% endcodeblock %}

<p>
  So far so good, That's it!!! See ya!!! :)
</p>
