class CreateGames < ActiveRecord::Migration[7.1]
  def change
    create_table :games do |t|
      t.text :board
      t.integer :current_player
      t.string :status

      t.timestamps
    end
  end
end
