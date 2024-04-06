class ConnectFour
  ROWS = 6
  COLS = 7
  EMPTY = ' '
  PLAYER1 = 'X'
  PLAYER2 = 'O'

  def initialize
    @board = Array.new(ROWS) { Array.new(COLS, EMPTY) }
    @player1_turn = true
  end

  def display_board
    puts " 1 2 3 4 5 6 7"
    @board.each do |row|
      puts "|" + row.join("|") + "|"
    end
    puts "---------------"
  end

  def drop_piece(col)
    (ROWS - 1).downto(0) do |row|
      if @board[row][col] == EMPTY
        @board[row][col] = @player1_turn ? PLAYER1 : PLAYER2
        return true
      end
    end
    false # Column is full
  end

  def check_win(row, col)
    player = @board[row][col]

    # Check vertical
    count = 0
    (row...ROWS).each do |r|
      if @board[r][col] == player
        count += 1
      else
        break
      end
    end
    return true if count >= 4

    # Check horizontal
    count = 0
    (0...COLS).each do |c|
      if @board[row][c] == player
        count += 1
      else
        count = 0
      end
      return true if count >= 4
    end

    # Check diagonal (bottom-left to top-right)
    count = 0
    (row...ROWS).each_with_index do |r, index|
      c = col + index
      break if c >= COLS
      if @board[r][c] == player
        count += 1
      else
        count = 0
      end
      return true if count >= 4
    end

    # Check diagonal (top-left to bottom-right)
    count = 0
    (row).downto(0).each_with_index do |r, index|
      c = col + index
      break if c >= COLS
      if @board[r][c] == player
        count += 1
      else
        count = 0
      end
      return true if count >= 4
    end

    false
  end

  def play
    loop do
      display_board

      current_player = @player1_turn ? PLAYER1 : PLAYER2
      puts "Player #{current_player}'s turn."

      loop do
        print "Enter column (1-7): "
        col = gets.chomp.to_i - 1
        if col >= 0 && col < COLS && drop_piece(col)
          break
        else
          puts "Invalid move. Please try again."
        end
      end

      row = @board.find_index { |row| row[col] != EMPTY }
      if check_win(row, col)
        display_board
        puts "Player #{current_player} wins!"
        break
      end

      @player1_turn = !@player1_turn
    end
  end
end

game = ConnectFour.new
game.play
