<template>
    <div class="connect-four">
        <h1>Connect Four</h1>
        <div v-for="(row, rowIndex) in board" :key="rowIndex" class="row">
            <div v-for="(cell, colIndex) in row" :key="colIndex" class="cell"
                :class="{ 'cell-player1': cell === 1, 'cell-player2': cell === 2 }" @click="dropPiece(colIndex)">
                {{ cell === 1 ? 'X' : cell === 2 ? 'O' : '' }}
            </div>
        </div>
        <button @click="resetBoard">Reset Game</button>
        <p v-if="winner">Player {{ winner }} wins!</p>
        <p v-if="checkForTie && !winner">It's a tie!</p>
    </div>
</template>

<script>
export default {
    name: 'ConnectFour',
    data() {
        return {
            board: Array.from({ length: 6 }, () => Array(7).fill(0)),
            currentPlayer: 1,  // 1 for human, 2 for AI
            isPlayerTurn: true,
            winner: null
        };
    },
    methods: {
        dropPiece(colIndex) {
            if (this.winner || !this.isPlayerTurn) return;  // Only proceed if it's the player's turn and no winner yet
            if (this.placePiece(colIndex, this.currentPlayer)) {
                if (!this.winner) {
                    this.isPlayerTurn = false;  // Hand over the turn to AI
                    this.$nextTick(() => {
                        this.aiMove();
                    });
                }
            }
        },
        aiMove() {
            setTimeout(() => {
                const bestMove = this.getBestMove(this.currentPlayer);
                this.placePiece(bestMove, this.currentPlayer);
                this.isPlayerTurn = true;  // Hand back the turn to the player
            }, 1000);  // AI move delay of 1 second
        },

        getBestMove(player) {
            let bestScore = -Infinity;
            let move = 0;
            for (let col = 0; col < 7; col++) {
                if (this.canPlacePiece(col)) {
                    this.placePiece(col, player);
                    let score = this.minimax(0, false);
                    this.undoMove(col);  // Remove the piece after calculation
                    if (score > bestScore) {
                        bestScore = score;
                        move = col;
                    }
                }
            }
            return move;
        },

        minimax(depth, isMaximizingPlayer) {
            const winner = this.checkForWinner();
            if (winner !== null) {
                return winner === this.currentPlayer ? 10 : -10;
            }

            if (isMaximizingPlayer) {
                let maxEval = -Infinity;
                for (let col = 0; col < 7; col++) {
                    if (this.canPlacePiece(col)) {
                        this.placePiece(col, this.currentPlayer);
                        let evalu = this.minimax(depth + 1, false);
                        this.undoMove(col);
                        maxEval = Math.max(maxEval, evalu);
                    }
                }
                return maxEval;
            } else {
                let minEval = Infinity;
                for (let col = 0; col < 7; col++) {
                    if (this.canPlacePiece(col)) {
                        this.placePiece(col, this.opponent);
                        let evalu = this.minimax(depth + 1, true);
                        this.undoMove(col);
                        minEval = Math.min(minEval, evalu);
                    }
                }
                return minEval;
            }
        },
        canPlacePiece(col) {
            // Check if the topmost cell of the column is empty (assuming null or similar signifies empty)
            return this.board[0][col] == null;
        },

        placePiece(colIndex, player) {
            for (let i = this.board.length - 1; i >= 0; i--) {
                if (this.board[i][colIndex] === 0) {
                    this.board[i][colIndex] = player;
                    if (this.checkWinner(i, colIndex)) {
                        this.winner = player;
                    }
                    this.currentPlayer = this.currentPlayer === 1 ? 2 : 1;
                    return true;  // Indicate successful placement
                }
            }
            return false;  // Column full, unsuccessful placement
        },
        undoMove(col) {
            // Iterate from the top of the column to find the first non-empty cell and clear it
            for (let row = 0; row < this.board.length; row++) {
                if (this.board[row][col] != null) {
                    this.board[row][col] = null;
                    break;  // Only remove the top-most piece
                }
            }
        },

        checkWinner(row, col) {
            // Define directions: [vertical, horizontal, diagonal up, diagonal down]
            const directions = [
                [1, 0], [0, 1], [1, -1], [1, 1]
            ];
            const lines = directions.map(([dx, dy]) => {
                return 1 +
                    this.countDirection(row, col, dx, dy) +
                    this.countDirection(row, col, -dx, -dy);
            });
            return lines.some(count => count >= 4);
        },
        countDirection(row, col, dx, dy) {
            let count = 0;
            let x = row + dx;
            let y = col + dy;
            while (this.validPosition(x, y) && this.board[x][y] === this.currentPlayer) {
                count++;
                x += dx;
                y += dy;
            }
            return count;
        },
        validPosition(row, col) {
            return row >= 0 && row < this.board.length && col >= 0 && col < this.board[0].length;
        },
        resetBoard() {
            this.board = Array.from({ length: 6 }, () => Array(7).fill(0));
            this.currentPlayer = 1;
            this.winner = null;
        },
        checkForTie() {
            return this.board.flat().every(cell => cell !== 0);
        }
    }
};
</script>

<style scoped>
.connect-four .row {
    display: flex;
}

.connect-four .cell {
    width: 50px;
    height: 50px;
    border: 1px solid black;
    display: flex;
    justify-content: center;
    align-items: center;
}

.cell-player1 {
    background-color: red;
}

.cell-player2 {
    background-color: yellow;
}
</style>