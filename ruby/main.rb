class ConnectFour
  ROWS = 6
  COLS = 7
  EMPTY = '.'
  PLAYERS = ['X', 'O']

  def initialize
    @board = Array.new(ROWS) { Array.new(COLS, EMPTY) }
    @current_player = 0
  end

  def play
    until game_over?
      display_board
      col = get_column
      row = place_piece(col)
      if winning_move?(row, col)
        display_board
        puts "Player #{PLAYERS[@current_player]} wins!"
        return
      end
      switch_player
    end
    puts "The game is a draw!"
  end

  private

  def display_board
    puts '1 2 3 4 5 6 7'
    @board.each { |row| puts row.join(' ') }
  end

  def get_column
    loop do
      print "Player #{PLAYERS[@current_player]}, enter a column (1-7): "
      col = gets.to_i - 1
      return col if col.between?(0, COLS - 1) && @board[0][col] == EMPTY
      puts "Invalid input, please try again."
    end
  end

  def place_piece(col)
    (ROWS - 1).downto(0) do |row|
      if @board[row][col] == EMPTY
        @board[row][col] = PLAYERS[@current_player]
        return row
      end
    end
  end

  def winning_move?(row, col)
    piece = PLAYERS[@current_player]
    directions = [[1, 0], [0, 1], [1, 1], [1, -1]]
    directions.any? do |dx, dy|
      line_length(row, col, dx, dy) + line_length(row, col, -dx, -dy) >= 3
    end
  end

  def line_length(row, col, dx, dy)
    length = 0
    loop do
      row += dx
      col += dy
      break unless row.between?(0, ROWS - 1) && col.between?(0, COLS - 1) && @board[row][col] == PLAYERS[@current_player]
      length += 1
    end
    length
  end

  def switch_player
    @current_player = 1 - @current_player
  end

  def game_over?
    @board.flatten.none?(EMPTY) || winning_move?(-1, -1)
  end
end

game = ConnectFour.new
game.play
