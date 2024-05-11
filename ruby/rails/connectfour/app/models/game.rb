# app/models/game.rb
class Game < ApplicationRecord
    serialize :state, JSON
  
    after_initialize :set_default_state
  
    def set_default_state
      self.state ||= Array.new(6) { Array.new(7, nil) }
      self.current_player ||= 1
    end
  
    def drop_piece(column)
      column = column.to_i
      row = state.reverse.find_index { |r| r[column].nil? }
      return false unless row
  
      state[5 - row][column] = current_player
      switch_player
      save
      true
    end
  
    private
  
    def switch_player
      self.current_player = current_player == 1 ? 2 : 1
    end
  end
  