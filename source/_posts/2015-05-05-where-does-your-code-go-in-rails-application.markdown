---
layout: post
title: "Where Does Your Code Go in Rails Application?"
date: 2015-05-05 08:50
comments: true
categories: [Ruby, Ruby on Rails]
keywords: Where Does Your Code Go in Rails Application?
---

<p>
  <img src="/images/ruby_on_rails.png" width="400" alt="Where Does Your Code Go in Rails Application?" />
</p>

<p>
  After you finish the Rails tutorials and start your own app, things get confusing something like where does your non-CRUD and general logic go?
</p>

<p>
  You can't build the app you've been dreaming of without some general, non-Rails logic. So where do you put your code, and still keep things simple?
</p>

<p>
  <strong>The Easy Place to Start</strong><br/>
  when I have logic that feels related to an existing ActiveRecord model, I'll start by putting it into that model. For example if I had a Game model and I want to import a bunch of games from CSV files, I would put that method right onto the Game class.
</p>

{% codeblock app/models/game.rb lang:ruby %}
class Game < ActiveRecord::Base
  def self.parse_from_csv(csv_string)
    games = []
    CSV.parse(csv_string, quote_char: "'") do |row|
      games << Game.from_csv_row(row) if (row[0] == 'G')
    end
    games
  end

  def self.from_csv_row(row)
    Game.new({
      dgs_game_id: row[1],
      opponent_name: row[2],
      created_at: row[4],
      updated_at: row[4],
    })
  end
end
{% endcodeblock %}

<p>
  You have all the information your methods need right at hand. it's easily testable. And it's probably where a new contributor world look for that logic first.
</p>

<p>
  But if you keep adding and changing that model, it'll get big and complicated. Different parts of the model will interact in strange ways. The more you change it, the harder it will be to change.
</p>

<p>
  In that case, you would probably want to refactor that code out to a non-ActiveRecord model.
</p>

<p>
  <strong>Non-ActiveRecord Models</strong><br/>
  You can write your own Ruby code, in plain Ruby objects, and use them in your Rails app. These objects can still be called models, because they're modeling part of your problem. They just don't have an ActiveRecord database storing their data.
</p>

<p>
  The next time I worked on that game CSV parser, the Game class was getting a little too big. So I moved the parser logic into its own GameCSVParser class.
</p>

<p>
  Non-ActiveRecord class looks like:
</p>

{% codeblock app/models/game_csv_parser.rb lang:ruby %}
class GameCSVParser
  def initialize(csv_string)
    @rows = CSV.parse(csv_string, quote_char: "'")
  end

  def games
    game_rows.map { |row| Game.new(game_attributes(row)) }
  end

  private

  def game_rows
    @rows.select { |row| is_game_row?(row) }
  end

  def game_attributes(row)
    {
      dgs_game_id: row[1],
      opponent_name: row[2],
      created_at: row[4],
      updated_at: row[4],
    }
  end

  def is_game_row?(row)
    row[0] == 'G'
  end
end
{% endcodeblock %}

<p>
  I will go right to creating a new plain Ruby object if the logic I'm adding doesn't feel related to any specific ActiveRecord model. Of if the code seems like it should be a part of a thing that doesn't exist yet in the app. Otherwise, they mostly pop up through refactoring.
</p>

<p>
  So far so good, That's it!!! See ya!!! :)
</p>