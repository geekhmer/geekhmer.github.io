---
layout: post
title: "Ajax on Rails"
date: 2015-06-19 11:37
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Ajax on Rails
---

<p>
  <img src="/images/ajax_on_rails.png" width="400" alt="Ajax on Rails" />
</p>

<p>
  This is a tutorial for ajax use with Rails conventions. For illustrative purposes, we’ll build a single-page Product list app.
</p>

<p>
  <strong>About Ajax</strong><br/>
  Ajax (Asynchronous JavaScript and XML) is used as a mechanism for sending and retrieving data asynchronously (in the background). While XML can certainly be used with ajax, it is not limited to this format. The JSON format, for example, is more commonly used today, especially in the Rails community. There are significant advantages in using Ajax, which include better user interactivity. Ajax allows content on a page to be updated without having to re-render the entire page, making it a "seamless" experience.
</p>

<p>
  <strong>Create a New Product on the Index Page</strong><br/>
  Before we start, let’s take a quick look at our schema so that we know what we’re working with:
</p>

{% codeblock lang:ruby %}
ActiveRecord::Schema.define(version: 20140620130316) do
  create_table "products", force: true do |t|
    t.datetime "created_at",  null: false
    t.datetime "updated_at",  null: false
    t.string   "name",        null: false
    t.string   "description", null: false
  end
end
{% endcodeblock %}

<p>
  After creating a Product model and then create some products to play with, our Product Controller should look like this:
</p>

{% codeblock products_controller.rb lang:ruby %}
class ProductsController < ApplicationController
  def index
    @products = Product.all
  end
end
{% endcodeblock %}

<p>
  Instead of creating <code>new.html.erb</code>, let’s add a button somewhere on our <code>index.html.erb</code> that users can use to display a hidden form:
</p>

{% codeblock index.html.erb lang:ruby %}
...

<%= link_to 'New Product', new_product_path, remote: true %>

...
{% endcodeblock %}

<p>
  Here we pass the <code>remote: true</code> option to disable the default Rails mechanism that would have otherwise navigated us to <code>/products/new</code>.
</p>

<p>
  Before moving on, let’s quickly revisit our Product Controller and set it up to create new products with ajax:
</p>

{% codeblock products_controller.rb lang:ruby %}
class ProductsController < ApplicationController
  before_action :all_product, only: [:index, :create]

  # index action has been removed

  def new
    @product = Product.new
  end

  def create
    @product  = Product.create(product_params)
  end

  private
  def all_products
    @products = Product.all
  end

  def product_params
    params.require(:product).permit(:name, :description)
  end
end
{% endcodeblock %}

<p>
  I have removed the index action because I created a <code>before_action</code> filter that creates the <code>@products</code> instance variable for us automatically. Because we no longer have any logic in our index action, it is not necessary. Rails will automatically render the correct template, even without the presence of the action.
</p>

<p>
  Noteworthy here is the <code>respond_to</code> method invoked near the top that will allow us to render both html and javascript responses with all of our controller actions. The respond_to method provides the ability to respond to the requests with many formats(i.e. csv, xml, etc…). This can be done for one or all actions in a controller. If we wanted to provide json only in our index action, we would write something like this:
</p>

{% codeblock lang:ruby %}
def index
  @products = Product.all

  respond_to do |format|
    format.html
    format.json
  end
end
{% endcodeblock %}

<p>
  Now, choose a place on the index page to hide your form by passing a style attribute with the following:
</p>

{% codeblock index.html.erb lang:ruby %}
...

<div id="product-form" style="display:none;"></div>

...
{% endcodeblock %}

<p>
  which will hide our form when the page is initially visited.
</p>

<p>
  Next, create <code>new.js.erb</code>:
</p>

{% codeblock new.js.erb lang:ruby %}
$('#product-form').html("<%= j (render 'form') %>");
$('#product-form').slideDown(350);
{% endcodeblock %}

<p>
  This is just an ERB template that generates Javascript instead of the HTML we’re used to. It basically translates to: "Find the element with an id attribute of product-form and at that point render the html in the form partial." We typically do this in <code>new.html.erb</code> with:
</p>

{% codeblock new.html.erb lang:ruby %}
<%= render 'form' %>
{% endcodeblock %}

<p>
  Since render is a Rails method, JavaScript doesn’t understand it and it has to be interpreted with ERB. The 'j' is syntactic sugar for <code><%= escape_javascript (render 'form') %></code>
</p>

{% codeblock _form.html.erb lang:ruby %}
<%= simple_form_for @product, remote: true do |f| %>
  <%= f.input  :description %>
  <%= f.input  :deadline %>
  <%= f.button :submit %>
<% end %>
{% endcodeblock %}

<p>
  This is the 'form' being rendered in <code>new.js.erb</code> with a <code>remote: true</code> option being passed in. In our form partial, we also pass the <code>remote: true</code> option that will execute an ajax POST request.
</p>

<p>
  Finally, we can wrap things up by rendering our new Product list and hiding our form. This final step includes identifying where to render our list. Using the rai-jax app as an example, let’s look at what our final <code>index.html.erb</code> should look like at this stage:
</p>

{% codeblock index.html.erb lang:ruby %}
<div class="row">
  <div class="col-md-5 col-md-offset-1">
    <h2>Products</h2>
  </div>

  <div class="col-md-2 col-md-offset-4">
    <%= link_to new_product_path, remote: true do %>
      <button class="btn btn-default">New</button>
    <% end %>
  </div>
</div>

<div class="row">
  <div class="col-md-6 col-md-offset-2" id="product-form" style="display:none;"></div>
</div>

<div class="row">
  <div class="col-md-7 col-md-offset-1" id="products"><%= render @products %></div>
</div>
{% endcodeblock %}

<p>
  And we update our product list and hide our form with <code>create.js.erb</code>:
</p>

{% codeblock create.js.erb lang:ruby %}
$('#products').html("<%= j (render @products) %>");
$('#product-form').slideUp(350);
{% endcodeblock %}

<p>
  <strong>Update a Product on the Index Page</strong><br/>
  As in Part One, let’s start by visiting our Product Controller and setting it up for updates:
</p>

{% codeblock products_controller.rb lang:ruby %}
class ProductsController < ApplicationController
  before_action :all_products, only: [:index, :create, :update]
  before_action :set_products, only: [:edit, :update]

  ...

  def update
    @product.update_attributes(product_params)
  end

  ...

  private
  ...

  def set_products
    @product = Product.find(params[:id])
  end

  def product_params
    params.require(:product).permit(:name, :description)
  end
end
{% endcodeblock %}

<p>
  Similar to Part One, we add an edit button with a <code>remote:</code> true option:
</p>

{% codeblock _product.html.erb lang:ruby %}
...

  <%= link_to edit_product_path(product), remote: true do %>
    <button>Edit</button>
  <% end %>

...
{% endcodeblock %}

<p>
  And, finally, our <code>edit.js.erb</code> and <code>update.js.erb</code> are the same as our new and update templates: <code>edit.js.erb</code> corresponds to <code>new.js.erb</code> and <code>create.js.erb</code> corresponds to <code>update.js.erb</code>.
</p>

<p>
  <strong>Delete a Product on the Index Page</strong><br/>
  Our final updates to our Product Controller involves us providing the destroy action:
</p>

{% codeblock product_controller.rb lang:ruby %}
class ProductsController < ApplicationController
  before_action :all_products, only: [:index, :create, :update, :destroy]
  before_action :set_products, only: [:edit, :update, :destroy]

  ...

  def destroy
    @product.destroy
  end
end
{% endcodeblock %}

<p>
  When adding our delete button, two additional steps are required:
</p>

{% codeblock _product.html.erb lang:ruby %}
...

<%= link_to product, remote: true, method: :delete,  data: { confirm: 'Are you sure?' } do %>
  <button>Delete!</button>
<% end %>

...
{% endcodeblock %}

<p>
  First, we pass in a <code>method: :delete</code> option; Second, we provide a courtesy confirmation to the user making sure they don’t delete anything by accident.
</p>

<p>
  The last file we’ll create is <code>destroy.js.erb</code> and it will contain one line:
</p>

{% codeblock destroy.js.erb lang:ruby %}
$('#products').html("<%= j (render @products) %>");
{% endcodeblock %}

<p>
  Seriously, Rails makes ajax easy. As I mentioned above.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>