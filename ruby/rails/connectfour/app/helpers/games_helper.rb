module GamesHelper
    def cell_text(cell)
      return ' ' if cell.nil?
      cell == 1 ? 'X' : 'O'
    end
  
    def cell_color(cell)
      return 'white' if cell.nil?
      cell == 1 ? 'red' : 'yellow'
    end
  end
  