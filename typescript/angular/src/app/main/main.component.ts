import { Component } from '@angular/core';

enum Player {
  None = 0,
  One = 1,
  Two = 2
}

@Component({
  selector: 'app-main',
  templateUrl: './main.component.html',
  styleUrls: ['./main.component.css']
})
export class MainComponent {

  rows = 6;
  cols = 7;
  currentPlayer: Player = Player.One;
  board: Player[][] = [];

  constructor() {
    this.initializeBoard();
  }

  initializeBoard() {
    this.board = [];
    for (let row = 0; row < this.rows; row++) {
      this.board[row] = [];
      for (let col = 0; col < this.cols; col++) {
        this.board[row][col] = Player.None;
      }
    }
  }

  dropChip(col: number) {
    for (let row = this.rows - 1; row >= 0; row--) {
      if (this.board[row][col] === Player.None) {
        this.board[row][col] = this.currentPlayer;
        this.checkWin(row, col);
        this.currentPlayer = this.currentPlayer === Player.One ? Player.Two : Player.One;
        break;
      }
    }
  }

  checkWin(row: number, col: number) {
    // Implement win checking logic (left as an exercise)
  }

}
