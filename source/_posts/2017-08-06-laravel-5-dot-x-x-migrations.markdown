---
layout: post
title: "Laravel 5.x.x Migrations"
date: 2017-08-06 09:59
comments: true
categories: [Laravel]
keywords: Laravel 5.x.x Migrations
---

<p>
  <img src="/images/laravel_news_letter.png" width="600" alt="Laravel 5.x.x Migrations" />
</p>

<p>
  Laravel migrations provide mechanisms for creating and modifying database tables. Migrations are database agnostic, this means you don't have to worry about the specific SQL syntax for the database engine that you are creating tables for.
</p>

<p>
  Well, in this articles I will cover the following sections: Requirements for running migrations, Artisan migration command, Migration structure, How to create a table using a migration, Laravel migration rollback, Laravel migration how-tos, Database seeding.
</p>

<h3>Requirements for Running Migrations</h3>

<p>
  1. Create the database for Laravel project<br/>
  2. Set the database connection parameters for Laravel project<br/>
  3. Set the database connection parameters for artisan command line
</p>

<p>
  <strong>1. Create the Database for Laravel Project</strong><br/>
  Open up terminator or what ever MySQL database management tool that you are using and run the command below:
</p>

{% codeblock lang:php %}
CREATE DATABASE foodie;
{% endcodeblock %}

<p>
  <code>CREATE DATABASE foodie;</code> creates a database called foodie in MySQL.
</p>

<p>
  <strong>2. Set the Database Connection Parameters for Laravel Project</strong><br/>
  Open up <code>/config/database.php</code> file and modify to the following:
</p>

{% codeblock database.php lang:php %}
'mysql' => [
  'driver' => 'mysql',
  'host' => env('DB_HOST', '127.0.0.1'),
  'port' => env('DB_PORT', '3306'),
  'database' => env('DB_DATABASE', 'foodie'),
  'username' => env('DB_USERNAME', 'root'),
  'password' => env('DB_PASSWORD', ''),
  'unix_socket' => env('DB_SOCKET', ''),
  'charset' => 'utf8mb4',
  'collation' => 'utf8mb4_unicode_ci',
  'prefix' => '',
  'strict' => true,
  'engine' => null,
]
{% endcodeblock %}

<p>
  <strong>3. Set the Database Connection Parameters for Artisan Command Line</strong><br/>
  One of the challenges that most developers face when working with migrations in Laravel 5.x.x from the artisan command line is the following message:
</p>

{% codeblock lang:php %}
Access denied for user 'homestead'@' localhost' (using password: YES)
{% endcodeblock %}

<p>
  You will get the above message even you have set the correct parameters in <code>/config/database.php</code> file, because the artisan command line uses the database connection parameters specified in <code>.env</code> file.
</p>

<p>
  The solutions is go to the project open up <code>/.env</code> file and modify to the following:
</p>

{% codeblock .env lang:php %}
APP_NAME=Laravel
APP_ENV=local
APP_KEY=base64:n8KivGzDCuNX1SljFb8xxQxBOPquewnAQIBa0H81nR8=
APP_DEBUG=true
APP_LOG_LEVEL=debug
APP_URL=http://localhost

DB_CONNECTION=mysql
DB_HOST=127.0.0.1
DB_PORT=3306
DB_DATABASE=foodie
DB_USERNAME=root
DB_PASSWORD=

BROADCAST_DRIVER=log
CACHE_DRIVER=file
SESSION_DRIVER=file
QUEUE_DRIVER=sync

REDIS_HOST=127.0.0.1
REDIS_PASSWORD=null
REDIS_PORT=6379

MAIL_DRIVER=smtp
MAIL_HOST=smtp.mailtrap.io
MAIL_PORT=2525
MAIL_USERNAME=null
MAIL_PASSWORD=null
MAIL_ENCRYPTION=null

PUSHER_APP_ID=
PUSHER_APP_KEY=
PUSHER_APP_SECRET=
{% endcodeblock %}

<p>
  The database, username and password must match the ones on your system.
</p>

<h3>Artisan Migration Command</h3>

<p>
  We will create:<br/>
  1. The migration table in our database.<br/>
  2. A migration file that we will use to create a table for hard drinks.
</p>

<p>
  When you create a migration file, Laravel will stores it in <code>/database/migrations</code> folder. You can specify a different path if you would like to but we won’t cover that in this articles. We will work with the default path.
</p>

<p>
  <strong>Create Migration Table</strong><br/>
  Open up the terminator and run the following artisan command to create a migration table:
</p>

{% codeblock lang:php %}
php artisan make:migration create_drinks_table
{% endcodeblock %}

<p>
  <code>php artisan make:migration</code> executes the make migration method via the artisan command.<br/>
  <code>create_drinks_table</code> specifies the name of the migration file that will be created.
</p>

<p>
  You will get the following results:
</p>

{% codeblock lang:php %}
Created Migration: 2017_08_08_072434_create_drinks_table
{% endcodeblock %}

<h3>Migration Structure</h3>

<p>
  You will get the following file with the contents below:
</p>

{% codeblock 20170808072434createdrinkstable.php lang:php %}
<?php

use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class CreateDrinksTable extends Migration {
  public function up() {
    //
  }

  public function down() {
    //
  }
}
{% endcodeblock %}

<p>
  - <code>class CreateDrinksTable extends Migration</code> defines the CreateDrinksTable class that extends Migration class.
  - <code>public function up()</code> defines the function that is executed when the migration is run.<br/>
  - <code>public function down()</code> defines the function that is executed when you run migration rollback.
</p>

<h3>How to Create a Table Using a Migration</h3>

<p>
  Now that we have successfully created a migration file, we will add the table definition fields in the migration modify the contents of <code>/database/migrations/20170808072434createdrinkstable.php file.</code>
</p>

{% codeblock 20170808072434createdrinkstable.php lang:php %}
<?php
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class CreateDrinksTable extends Migration {
  /**
  * Run the migrations.
  *
  * @return void
  */
  public function up() {
    Schema::create('drinks', function (Blueprint $table) {
      $table->increments('id');
      $table->string('name', 75)->unique();
      $table->text('comments')->nullable();
      $table->integer('rating');
      $table->date('brew_date');

      $table->timestamps();
    });
  }

  /**
  * Reverse the migrations.
  *
  * @return void
  */
  public function down() {
    Schema::drop('drinks');
  }
}
{% endcodeblock %}

<p>
  - <code>Schema::create('drinks', function (Blueprint $table) {...}</code> calls the create function of the Schema class. The create function is responsible for creating the database table.<br/>
  - <code>(Blueprint $table)</code> is a closure function with a $table parameter.<br/>
  - <code>$table</code> parameter is used to define the structure of the database.<br/>
  - <code>$table->increments('id');</code> increments is used to define an auto increment field.<br/>
  - <code>$table->string('name', 75)->unique();</code> string is used to define varchar fields. The second parameter is the length of the field. <code>->unique()</code> is used to mark the column as unique.<br/>
  - <code>$table->text('comments')->nullable();</code> is used to define text fields. <code>->nullable()</code> is used to allow the column to accept null values.<br/>
  - <code>$table->integer('rating');</code> integer is used to define int fields.<br/>
  - <code>$table->date('brew_date');</code> is used to define date fields.<br/>
  - <code>$table->timestamps();</code>  is used to automatically create two time stamp fields namely created_at and updated_at.
</p>

<p>
  Go back to the terminator and run the command below:
</p>

{% codeblock lang:php %}
php artisan migrate
{% endcodeblock %}

<p>
  And then you will get many tables drinks and users, password_resets which Laravel has migrated those two tables by defaults.
</p>

<h3>Laravel Migration Rollback</h3>

<p>
  One of the advantages of migrations is that it allow you to roll back to the previous state before you run the migrations. In this section, we will roll back the creation of the tables.
</p>

<p>
  Go back to the terminator and run the command below:
</p>

{% codeblock lang:php %}
php artisan migrate:rollback
{% endcodeblock %}

<p>
  And then you will get the following output:
</p>

{% codeblock lang:php %}
Rolled back: 2017_08_08_000000_create_users_table.php
Rolled back: 2017_08_08_100000_create_password_resets_table.php
Rolled back: 2017_08_08_090421_create_drinks_table.php
{% endcodeblock %}

<h3>Laravel Migration How-tos</h3>

<p>
  This section I will show how to perform various Laravel migration tasks.
</p>

<p>
  <strong>Laravel Migration Insert Data</strong><br/>
  This "how-to" shows you how to create a migration file that inserts data into the newly created table. We will create an employees table and add 33 seed records using Faker Library.
</p>

<p>
  Open up the terminator and run the command below:
</p>

{% codeblock lang:php %}
php artisan make:migration employees
{% endcodeblock %}

<p>
  Open up <code>/database/migrations/xxxxxxxxx_employees.php</code> file and add the following codes:
</p>

{% codeblock xxxxxxxxx_employees.php lang:php %}
<?php

use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class Employees extends Migration
{
  /**
   * Run the migrations.
   *
   * @return void
   */
  public function up() {
    Schema::create('employees', function (Blueprint $table) {
      $table->increments('id');
      $table->string('name');
      $table->string('email')->unique();
      $table->string('contact_number');
      $table->timestamps();       
    });

    $faker = Faker\Factory::create();

    $limit = 33;

    for($i = 0; $i < $limit; $i++) {
      DB::table('employees')->insert([ //,
        'name' => $faker->name,
        'email' => $faker->unique()->email,
        'contact_number' => $faker->phoneNumber,
      ]);
    }
  }

  /**
   * Reverse the migrations.
   *
   * @return void
   */
  public function down() {
    Schema::drop('employees');
  }
}
{% endcodeblock %}

<p>
  <code>$faker = Faker\Factory::create();</code> creates an instance of Faker factory.<br/>
  <code>$limit = 33;</code> sets the number of records that we want to add to the database.<br/>
  <code>for($i = 0; $i < $limit; $i++) { DB::table('employees')->insert(...); }</code> uses a for loop to add records to the database 33 times. <code>$faker->name</code> generates a faker name. <code>$faker->unique()->email</code> generates a fake unique email address. <code>$faker->phoneNumber</code> generates a fake phone number.
</p>

<p>
  Open up the terminator and run the following command to run the migration:
</p>

{% codeblock lang:php %}
php artisan migration
{% endcodeblock %}

<p>
  <strong>Laravel Migration Add Column/Drop Colum</strong><br/>
  We will add a new gender column to employees table. 
</p>

<p>
  Open up the terminator and run the following command:
</p>

{% codeblock lang:php %}
php artisan make:migration add_gender_to_employees --table=employees
{% endcodeblock %}

<p>
  <code>--table=employees</code> tells Laravel we want to work with an existing table called employees.
</p>

<p>
  Open up <code>/database/migration/xxxxxxx_add_gender_to_employees.php</code> and modify to the following:
</p>

{% codeblock xxxxxxx_add_gender_to_employees.php lang:php %}
<?php

use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class AddGenderToEmployees extends Migration
{
  /**
   * Run the migrations.
   *
   * @return void
   */
  public function up() {
    Schema::table('employees', function (Blueprint $table) {
      $table->string('gender')->after('contact_number');
    });
  }

  /**
   * Reverse the migrations.
   *
   * @return void
   */
  public function down() {
    Schema::table('employees', function (Blueprint $table) {
      $table->dropColumn('gender');
    });
  }
}
{% endcodeblock %}

<p>
  <code>public function up() {...}</code> uses <code>Schema::table('employees' ...)</code> to add a new column gender.<br/>
  <code>public function down() {...}</code> drops the new column from the table when we reverse the command. <code>$table->dropColumn('gender');</code> is the command that drops the table.
</p>

<p>
  <strong>Laravel Migration Change Column Type</strong><br/>
</p>

<p>
  We have created the gender column with the default size of 255. We want to change it to 5 as the maximum size.
</p>

<p>
  Open up the terminator and run the following command:
</p>

{% codeblock lang:php %}
php artisan make:migration modify_gender_in_employees --table=employees
{% endcodeblock %}

<p>
  Open up <code>/database/migrations/xxxxxxx_modify_gender_in_employees.php</code> file and modify to the following:
</p>

{% codeblock xxxxxxx_modify_gender_in_employees.php lang:php %}
<?php

use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class ModifyGenderInEmployees extends Migration {
  /**
   * Run the migrations.
   *
   * @return void
   */
  public function up()
  {
    Schema::table('employees', function(Blueprint $table) {
      $table->string('gender', 5)->change();
    });
  }

  /**
   * Reverse the migrations.
   *
   * @return void
   */
  public function down() {
    Schema::table('employees', function(Blueprint $table) {
      $table->string('gender', 255)->change();
    });
  }
}
{% endcodeblock %}

<p>
  <code>$table->string('gender', 5)->change();</code> maintains the varchar data type and sets the character limit to 5. If we wanted to change the data type too, we would have specified a different data type.<br/>
  <code>$table->string('gender', 255)->change();</code> rollback the migration to the previous state.
</p>

<p>
  Open up the terminator and run the following command to run the migration:
</p>

{% codeblock lang:php %}
php artisan migrate
{% endcodeblock %}

<p>
  <strong>Laravel Migration Nullable</strong><br/>
  By default, Laravel assumes all columns are required unless you tell it so let’s assume the gender field is optional.
</p>

<p>
  Open up the terminator and run the following command to create a migration file:
</p>

{% codeblock lang:php %}
php artisan make:migration make_gender_null_in_employees –table-employees
{% endcodeblock %}

<p>
  Open up <code>/database/migrations/xxxxxxx_make_gender_null_in_employees.php</code> file and modify to the following:
</p>

{% codeblock xxxxxxx_make_gender_null_in_employees.php lang:php %}
<?php

use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class MakeGenderNullInEmployees extends Migration {
  /**
   * Run the migrations.
   *
   * @return void
   */
  public function up() {
    Schema::table('employees', function(Blueprint $table) {
      $table->string('gender', 5)->nullable()->change();
    });
  }

  /**
   * Reverse the migrations.
   *
   * @return void
   */
  public function down() {
    Schema::table('employees', function(Blueprint $table) {
      $table->string('gender', 5)->change();
    });
  }
}
{% endcodeblock %}

<p>
  <strong>Laravel Migration Foreign Key</strong><br/>
  Let’s say we want to group our employees by their departments, we can add a foreign key for the dept_id.
</p>

<p>
  Open up the terminator and run the following command to create a migration file for depts table:
</p>

{% codeblock lang:php %}
php artisan make:migration depts
{% endcodeblock %}

<p>
  Open up <code>/database/migrations/xxxxxxxxx_depts.php</code> file and add the following codes:
</p>

{% codeblock xxxxxxxxx_depts.php lang:php %}
<?php

use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class Depts extends Migration
{
  /**
   * Run the migrations.
   *
   * @return void
   */
  public function up() {
    Schema::create('depts', function(Blueprint $table) {
      $table->increments('id');
      $table->string('name');
      $table->timestamps();
    });
  }

  /**
   * Reverse the migrations.
   *
   * @return void
   */
  public function down() {
    Schema::drop('depts');
  }
}
{% endcodeblock %}

<p>
  Open up the terminator and run the following command to create the depts table:
</p>

{% codeblock lang:php %}
php artisan migrate
{% endcodeblock %}

<p>
  The primary and foreign key relationship requires both tables to have the same data type and length. We used Schema’s increments to define the primary key for depts id. Schema’s increments creates an unsigned integer INT(10), Schema’s integer creates signed integer INT(11).
</p>

<p>
  We need to use Schema’s unsignedInteger when creating dept_id so that both the primary and foreign keys will be INT(10).
</p>

<p>
  Open up the terminator and run the following command to create the migration for adding the dept_id to the employees table:
</p>

{% codeblock lang:php %}
php artisan make:migration add_dept_id_in_employees --table=employees
{% endcodeblock %}

<p>
  Open up <code>/database/migrations/xxxxxxxxx_add_dept_id_in_employees.php</code> file and add the following codes:
</p>

{% codeblock xxxxxxxxx_add_dept_id_in_employees.php lang:php %}
<?php

use Illuminate\Database\Schema\Blueprint;
use Illuminate\Database\Migrations\Migration;

class AddDeptIdInEmployees extends Migration {
  /**
   * Run the migrations.
   *
   * @return void
   */
  public function up() {
    Schema::table('employees', function (Blueprint $table) {
      $table-> unsignedInteger ('dept_id')->after('gender');
      $table->foreign('dept_id')
              ->references('id')->on('depts')
              ->onDelete('cascade');
    });
  }

  /**
   * Reverse the migrations.
   *
   * @return void
   */
  public function down() {
    Schema::table('employees', function (Blueprint $table) {
      $table->dropColumn('dept_id');
    });
  }
}
{% endcodeblock %}

<p>
  Open up the terminator and run the following command to execute the migration:
</p>

{% codeblock lang:php %}
php artisan migrate
{% endcodeblock %}

<h3>Database Seeding</h3>

<p>
  In this section, we will add dummy data to our database. Seeding is a term that is used to describe the process of adding data to the database.
</p>

<p>
  Open up the terminator and run the following command:
</p>

{% codeblock lang:php %}
php artisan make:seeder DrinksTableSeeder
{% endcodeblock %}

<p>
  Open up <code>/database/seeds/DrinksTableSeeder.php</code> file and add the following codes:
</p>

{% codeblock DrinksTableSeeder.php lang:php %}
<?php

use Illuminate\Database\Seeder;

class DrinksTableSeeder extends Seeder {

  /**
   * Run the database seeds.
   *
   * @return void
   */
  public function run() {
    DB::table('drinks')->insert([
      'name' => 'Vodka',
      'comments' => 'Blood of creativity',
      'rating' => 9,
      'brew_date' => '1973-09-03',
    ]);
  }
}
{% endcodeblock %}

<p>
  <code>class DrinksTableSeeder extends Seeder</code> defines the table DrinksTableSeeder that extends the Seeder class.<br/>
  <code>public function run()</code> defines the function that is executed when you run the seed command from artisan.
</p>

<p>
  The above table uses an array that matches database field name to values and inserts the record into the specified table drinks. Now let's run the seed and add our dummy record to the database.
</p>

<p>
  Open up the terminator and run the following command:
</p>

{% codeblock lang:php %}
php artisan db:seed --class=DrinksTableSeeder
{% endcodeblock %}
