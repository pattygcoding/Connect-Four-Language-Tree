use strict;
use warnings;

# Constants
use constant ROWS => 6;
use constant COLS => 7;
use constant EMPTY => ' ';
use constant PLAYER1 => 'X';
use constant PLAYER2 => 'O';

# Initialize the game board
my @board = ();
for my $row (0 .. ROWS - 1) {
    push @board, [ (EMPTY) x COLS ];
}

# Display the game board
sub display_board {
    print " 1 2 3 4 5 6 7\n";
    for my $row (@board) {
        print "|", join("|", @$row), "|\n";
    }
    print "---------------\n";
}

# Drop a piece into a column
sub drop_piece {
    my ($col, $player) = @_;

    for my $row (reverse 0 .. ROWS - 1) {
        if ($board[$row][$col] eq EMPTY) {
            $board[$row][$col] = $player;
            return 1;
        }
    }

    return 0; # Column is full
}

# Check for a win condition
sub check_win {
    my ($player) = @_;

    # Check horizontal
    for my $row (0 .. ROWS - 1) {
        for my $col (0 .. COLS - 4) {
            if (   $board[$row][$col] eq $player
                && $board[$row][$col + 1] eq $player
                && $board[$row][$col + 2] eq $player
                && $board[$row][$col + 3] eq $player)
            {
                return 1;
            }
        }
    }

    # Check vertical
    for my $row (0 .. ROWS - 4) {
        for my $col (0 .. COLS - 1) {
            if (   $board[$row][$col] eq $player
                && $board[$row + 1][$col] eq $player
                && $board[$row + 2][$col] eq $player
                && $board[$row + 3][$col] eq $player)
            {
                return 1;
            }
        }
    }

    # Check diagonal (top-left to bottom-right)
    for my $row (0 .. ROWS - 4) {
        for my $col (0 .. COLS - 4) {
            if (   $board[$row][$col] eq $player
                && $board[$row + 1][$col + 1] eq $player
                && $board[$row + 2][$col + 2] eq $player
                && $board[$row + 3][$col + 3] eq $player)
            {
                return 1;
            }
        }
    }

    # Check diagonal (bottom-left to top-right)
    for my $row (3 .. ROWS - 1) {
        for my $col (0 .. COLS - 4) {
            if (   $board[$row][$col] eq $player
                && $board[$row - 1][$col + 1] eq $player
                && $board[$row - 2][$col + 2] eq $player
                && $board[$row - 3][$col + 3] eq $player)
            {
                return 1;
            }
        }
    }

    return 0;
}

# Main game loop
my $currentPlayer = PLAYER1;
my $gameOver = 0;

while (!$gameOver) {
    display_board();

    # Prompt current player for column input
    print "Player $currentPlayer's turn. Enter column (1-7): ";
    my $input = <STDIN>;
    chomp $input;
    my $col = $input - 1;

    # Validate column input
    while ($col < 0 || $col >= COLS || $board[0][$col] ne EMPTY) {
        print "Invalid column. Please enter a valid column (1-7): ";
        $input = <STDIN>;
        chomp $input;
        $col = $input - 1;
    }

    # Drop the piece into the board
    if (drop_piece($col, $currentPlayer)) {
        # Check for win condition
        if (check_win($currentPlayer)) {
            display_board();
            print "Player $currentPlayer wins!\n";
            $gameOver = 1;
        } elsif (grep { $_ eq EMPTY } @{$board[0]}) {
            $currentPlayer = ($currentPlayer eq PLAYER1) ? PLAYER2 : PLAYER1; # Switch player
        } else {
            display_board();
            print "It's a draw! The board is full.\n";
            $gameOver = 1;
        }
    }
}

# Display the final game board
display_board();

