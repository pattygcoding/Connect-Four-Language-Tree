class Game < ApplicationRecord
    serialize :state, JSON
  
    after_initialize :set_default_state
  
    def set_default_state
      self.state = (self.state || Array.new(6) { Array.new(7, nil) }).map(&:dup)
      self.current_player ||= 1
      self.winner ||= nil
    end
  
    def drop_piece(column)
        row = state.reverse.find_index { |r| r[column].nil? }
        return false unless row  # Returns false if the column is full or invalid
        state[5 - row][column] = current_player
        if check_winner(5 - row, column)
          self.winner = current_player
        end
        switch_player
        save
        true
      end
  
    def switch_player
      self.current_player = current_player == 1 ? 2 : 1
    end
  
    def check_winner(row, column)
      directions = [[1, 0], [0, 1], [1, 1], [1, -1]]
      directions.any? do |dx, dy|
        count_consecutive_pieces(row, column, dx, dy) + count_consecutive_pieces(row, column, -dx, -dy) - 1 >= 4
      end
    end
  
    private
  
    def count_consecutive_pieces(row, column, dx, dy)
      consecutive_count = 0
      loop do
        row += dx
        column += dy
        break unless row.between?(0, 5) && column.between?(0, 6) && state[row][column] == current_player
        consecutive_count += 1
      end
      consecutive_count
    end
  end
  