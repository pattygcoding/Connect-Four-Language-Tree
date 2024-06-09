<!-- src/components/ConnectFour.vue -->
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
		<button @click="goToTogglePage">Go to Toggle Page</button>
	</div>
</template>

<script>
export default {
	name: 'ConnectFour',
	data() {
		return {
			board: Array.from({ length: 6 }, () => Array(7).fill(0)),
			currentPlayer: 1,
			winner: null
		};
	},
	methods: {
		dropPiece(colIndex) {
			if (this.winner) return;  // Stop game if there's already a winner
			for (let i = this.board.length - 1; i >= 0; i--) {
				if (this.board[i][colIndex] === 0) {
					this.board[i][colIndex] = this.currentPlayer;
					if (this.checkWinner(i, colIndex)) {
						this.winner = this.currentPlayer;
					}
					this.currentPlayer = this.currentPlayer === 1 ? 2 : 1;
					break;
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
		},
		goToTogglePage() {
			this.$router.push('/toggle');
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