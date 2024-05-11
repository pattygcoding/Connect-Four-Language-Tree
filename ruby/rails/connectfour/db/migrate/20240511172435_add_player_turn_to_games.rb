class AddPlayerTurnToGames < ActiveRecord::Migration[7.1]
  def change
    add_column :games, :player_turn, :integer
  end
end
