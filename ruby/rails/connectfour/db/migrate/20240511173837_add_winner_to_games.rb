class AddWinnerToGames < ActiveRecord::Migration[7.1]
  def change
    add_column :games, :winner, :integer
  end
end
