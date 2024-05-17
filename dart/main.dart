import 'dart:io';

const int rows = 6;
const int columns = 7;

List<List<String>> board = List.generate(rows, (i) => List.filled(columns, ' '));

void printBoard() {
  for (int row = 0; row < rows; row++) {
    for (int col = 0; col < columns; col++) {
      stdout.write('| ${board[row][col]} ');
    }
    stdout.writeln('|');
  }
  for (int col = 0; col < columns; col++) {
    stdout.write('----');
  }
  stdout.writeln('-');
  for (int col = 0; col < columns; col++) {
    stdout.write('  ${col + 1} ');
  }
  stdout.writeln();
}

bool dropDisc(int column, String disc) {
  for (int row = rows - 1; row >= 0; row--) {
    if (board[row][column] == ' ') {
      board[row][column] = disc;
      return true;
    }
  }
  return false; // Column is full
}

bool checkWin(String disc) {
  // Directions to check: right, down, down-right, down-left
  List<List<int>> directions = [
    [0, 1],  // Horizontal
    [1, 0],  // Vertical
    [1, 1],  // Diagonal /
    [1, -1]  // Diagonal \
  ];

  for (int row = 0; row < rows; row++) {
    for (int col = 0; col < columns; col++) {
      if (board[row][col] == disc) {
        for (var direction in directions) {
          int count = 0;
          for (int i = 0; i < 4; i++) {
            int r = row + direction[0] * i;
            int c = col + direction[1] * i;
            if (r >= 0 && r < rows && c >= 0 && c < columns && board[r][c] == disc) {
              count++;
            } else {
              break;
            }
          }
          if (count == 4) {
            return true;
          }
        }
      }
    }
  }
  return false;
}

void playGame() {
  String currentPlayer = 'X';
  while (true) {
    printBoard();
    print('Player $currentPlayer, choose a column (1-$columns):');
    int? column = int.tryParse(stdin.readLineSync() ?? '');
    if (column != null && column >= 1 && column <= columns) {
      if (dropDisc(column - 1, currentPlayer)) {
        if (checkWin(currentPlayer)) {
          printBoard();
          print('Player $currentPlayer wins!');
          break;
        }
        currentPlayer = (currentPlayer == 'X') ? 'O' : 'X';
      } else {
        print('Column is full! Choose another column.');
      }
    } else {
      print('Invalid input! Please enter a column number between 1 and $columns.');
    }
  }
}

void main() {
  playGame();
}
