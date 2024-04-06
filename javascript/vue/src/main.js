// src/main.js

import Vue from 'vue';

const Player = {
  None: 0,
  One: 1,
  Two: 2
};

new Vue({
  el: '#app',
  data() {
    return {
      rows: 6,
      cols: 7,
      currentPlayer: Player.One,
      board: [],
      gameOver: false
    };
  },
  created() {
    this.initializeBoard();
  },
  methods: {
    initializeBoard() {
      this.board = [];
      for (let row = 0; row < this.rows; row++) {
        this.board[row] = [];
        for (let col = 0; col < this.cols; col++) {
          this.board[row][col] = Player.None;
        }
      }
    },
    dropChip(col) {
      if (this.gameOver) return;
      for (let row = this.rows - 1; row >= 0; row--) {
        if (this.board[row][col] === Player.None) {
          this.board[row][col] = this.currentPlayer;
          if (this.checkWin(row, col)) {
            this.gameOver = true;
            alert(`Player ${this.currentPlayer} wins!`);
          } else {
            this.currentPlayer = this.currentPlayer === Player.One ? Player.Two : Player.One;
          }
          break;
        }
      }
    },
    checkWin(row, col) {
      // Implement win checking logic
      // Left as an exercise (check for horizontal, vertical, and diagonal wins)
      return false;
    },
    restartGame() {
      this.initializeBoard();
      this.currentPlayer = Player.One;
      this.gameOver = false;
    }
  }
});
