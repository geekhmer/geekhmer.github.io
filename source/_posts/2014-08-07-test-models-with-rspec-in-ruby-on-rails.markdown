---
layout: post
title: "Test Models With RSpec in Ruby on Rails"
date: 2014-08-07 09:40
comments: true
categories: [Ruby on Rails, Ruby, testing]

keywords: models test, models testing, model test, model testing, test models with rspec in ruby on rails, test model with rspec in ruby on rails, test models with rspec in rails, test model with rspec in rails
description: Test Models With RSpec in Ruby on Rails
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" />
</p>

<p>
  Testing is a good pratice. You should be doing it. It will make you a better programmer and save you a great deal of headache as your web app grows up. It is especially important when working alongside other programmers. Testing is not perfect though so don't try to be perfect. Just get started, and you will improve as time goes on.
</p>

<p>
  <strong>How should I be testing?</strong><br/>
  - Using RSpec & factorygirl.<br/>
  - Testing the Model.
</p>

<p>
  <strong>Installation</strong><br/>
  Add rspec-rails and factorygirl to both the :development and :test groups in the Gemfile:
</p>

{% codeblock Gemfile lang:ruby %}
group :development, :test do
  gem 'factory_girl_rails', '4.2.1'
  gem 'rspec-rails', '~> 3.0.0'
end
{% endcodeblock %}

<p>
  Download and install by running command:<br>
</p>

{% codeblock lang:ruby %}
bundle install
{% endcodeblock %}

<p>
  Initialize the spec/ directory (where specs will reside) with:
</p>

{% codeblock lang:ruby %}
rails generate rspec:install
{% endcodeblock %}

<p>
  This adds the following files which are used for configuration:<br/>
  - .rspec<br/>
  - spec/spec_helper.rb<br/>
  - spec/rails_helper.rb
</p>

<p>
  <strong>Generators</strong><br/>
  Once installed, RSpec and factorygirl will generate spec files instead of Test::Unit test files when run commands like: <code>rails generate model</code> and <code>rails generate controller</code> are used.
</p>

<p>
  Example:
</p>

{% codeblock lang:ruby %}
rails generate model Post
{% endcodeblock %}

<p>
  After you run the command above this adds the following directory and file:<br/>
  - spec/models/posts.rb<br/>
  - spec/factories/posts.rb
</p>

<p>
  <strong>Let's get started the Model testing</strong>
</p>

<p>
  Assume we have three Models such as post.rb, category.rb, categorization.rb:
</p>

{% codeblock post.rb lang:ruby %}
class Post < ActiveRecord::Base
  validates :title, length: { minimum: 10, maximum: 100 }, presence: true, uniqueness: true
  validates :body, length: { minimum: 20, maximum: 200 }
  validates :status, length: { minimum: 2, maximum: 20 }, presence: true
  validates :category_id, presence: true

  has_many :categorizations
  has_many :categories, through: :categorizations

  scope :search_by_title, -> (title) { where("(title like ?) OR title in (?)", "%#{title}%", title.split) }
end
{% endcodeblock %}

{% codeblock category.rb lang:ruby %}
class Category < ActiveRecord::Base
  validates :name, length: { minimum: 10, maximum: 50 }, presence: true, uniqueness: true
  validates :short_name, length: { minimum: 10, maximum: 50 }, presence: true, uniqueness: true
  validates :description, length: { maximum: 200 }
  
  has_many :categorizations
  has_many :posts, through: :categorizations
end
{% endcodeblock %}

{% codeblock categorization.rb lang:ruby %}
class Categorization < ActiveRecord::Base
  validates :category_id, presence: true
  validates :post_id, presence: true

  belongs_to :category
  belongs_to :post
end
{% endcodeblock %}

<p>
  Next, we define default factorygirl object for each Models in spec/factories/:
</p>

{% codeblock posts.rb lang:ruby %}
FactoryGirl.define do
  factory :post do
    title 'Ruby on Rails'
    body 'Ruby on Rails is good'
    status 'open'
    category_id 1
  end
end
{% endcodeblock %}

{% codeblock categories.rb lang:ruby %}
FactoryGirl.define do
  factory :category do
    id 1
    name 'programming'
    short_name 'programming'
    description 'computer programming'
  end
end
{% endcodeblock %}

{% codeblock categorizations.rb lang:ruby %}
FactoryGirl.define do
  factory :categorization do
    category_id '1'
    post_id '1'
  end
end
{% endcodeblock %}

<p>
  Here, how we test each Models in spec/models/:
</p>

{% codeblock post.rb lang:ruby %}
require 'spec_helper'

describe Post, 'validation' do
  it { should ensure_length_of(:title).is_at_least(10) }
  it { should ensure_length_of(:title).is_at_most(100) }
  it { should validate_presence_of(:title) }
  it { should validate_uniqueness_of(:title) }

  it { should ensure_length_of(:body).is_at_least(20) }
  it { should ensure_length_of(:body).is_at_most(200) }

  it { should ensure_length_of(:status).is_at_least(2) }
  it { should ensure_length_of(:status).is_at_most(20) }
  it { should validate_presence_of(:status) }

  it { should validate_presence_of(:category_id) }
end

describe Post, 'association' do
  it { should have_many(:categorizations) }
  it { should have_many(:categories).through(:categorizations) }
end

describe Post, 'column_specification' do
  it { should have_db_column(:title).of_type(:string).with_options(length: { minimum: 10, maximum: 100 }, presence: true, uniqueness: true) }
  it { should have_db_column(:body).of_type(:text).with_options(length: { minimum: 20, maximum: 200 }) }
  it { should have_db_column(:status).of_type(:string).with_options(length: { minimum: 2, maximum: 20, presence: true }) }
  it { should have_db_column(:category_id).of_type(:integer) }

  it { should have_db_index(:title).unique(true) }
end

describe Post, '.search_by_name' do
  before(:each) do
    FactoryGirl.create(:post, title: 'Ruby on Rails')
  end

  it 'returns post that match with title' do
    Post.search_by_title('Ruby on Rails').count.should eql 1
  end

  it 'returns post that like title' do
    Post.search_by_title('ruby on rails').count.should eql 1
  end

  it 'returns post when title is blank' do
    Post.search_by_title('').count.should eql 1
  end

  it 'returns empty when title is not match' do
    Post.search_by_title('not match').count.should eql 0
  end
end
{% endcodeblock %}

{% codeblock category.rb lang:ruby %}
require 'spec_helper'

describe Category, 'validation' do
  it { should ensure_length_of(:name).is_at_least(10) }
  it { should ensure_length_of(:name).is_at_most(50) }
  it { should validate_presence_of(:name) }
  it { should validate_uniqueness_of(:name) }

  it { should ensure_length_of(:short_name).is_at_least(10) }
  it { should ensure_length_of(:short_name).is_at_most(50) }
  it { should validate_presence_of(:short_name) }
  it { should validate_uniqueness_of(:short_name) }
end

describe Category, 'association' do
  it { should have_many(:categorizations) }
  it { should have_many(:posts).through(:categorizations) }
end

describe Category, 'column_specification' do
  it { should have_db_column(:name).of_type(:string).with_options(length: { minimum: 10, maximum: 50 }, presence: true, uniqueness: true) }
  it { should have_db_column(:short_name).of_type(:string).with_options(length: { minimum: 10, maximum: 50 }, presence: true, uniqueness: true) }
  it { should have_db_column(:description).of_type(:text).with_options(length: { maximum: 200 }) }
end
{% endcodeblock %}

{% codeblock categorization.rb lang:ruby %}
require 'spec_helper'

describe Categorization, 'validation' do
  it { should validate_presence_of(:category_id) }
  it { should validate_presence_of(:post_id) }
end

describe Categorization, 'association' do
  it { should belong_to(:category) }
  it { should belong_to(:post) }
end

describe Categorization, 'column_specification' do
  it { should have_db_column(:category_id).of_type(:integer).with_options(presence: true) }
  it { should have_db_column(:post_id).of_type(:integer).with_options(presence: true) }
end
{% endcodeblock %}

<p>
  To run Models specs use the following command:
</p>

{% codeblock lang:ruby %}
rspec spec/models
{% endcodeblock %}

<p>
  You should get back the response something like:
</p>

<p>
  <img src="/images/model_testing.png" width="600"/>
</p>

<p>
  For more detail about <a href="https://github.com/rspec/rspec-rails" target="_blank">RSpec</a>, <a href="https://github.com/thoughtbot/factory_girl" target="_blank">FactoryGirl</a>.<br/>
  So far so good, We've already created 40 models specs to test model. :)
</p>