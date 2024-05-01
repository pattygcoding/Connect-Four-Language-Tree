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
      const directions = [
        [0, 1],
        [1, 0],
        [1, 1],
        [-1, 1]
      ];

      for (const [dx, dy] of directions) {
        let consecutiveChips = 1;

        // Check in one direction
        let newRow = row + dx;
        let newCol = col + dy;
        while (newRow >= 0 && newRow < this.rows && newCol >= 0 && newCol < this.cols && this.board[newRow][newCol] === this.currentPlayer) {
          consecutiveChips++;
          newRow += dx;
          newCol += dy;
        }

        // Check in the opposite direction
        newRow = row - dx;
        newCol = col - dy;
        while (newRow >= 0 && newRow < this.rows && newCol >= 0 && newCol < this.cols && this.board[newRow][newCol] === this.currentPlayer) {
          consecutiveChips++;
          newRow -= dx;
          newCol -= dy;
        }

        // Check if there are four consecutive chips
        if (consecutiveChips >= 4) {
          return true;
        }
      }

      return false;
    },
    restartGame() {
      this.initializeBoard();
      this.currentPlayer = Player.One;
      this.gameOver = false;
    }
  }
});
