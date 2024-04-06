const ROWS = 6;
const COLS = 7;
let board = [];
let currentPlayer = 1;
let gameOver = false;

// Initialize the game board
function initBoard() {
    board = [];
    for (let i = 0; i < ROWS; i++) {
        board.push(Array.from({ length: COLS }, () => 0));
    }
}

// Render the game board
function renderBoard() {
    const boardElement = document.getElementById('board');
    boardElement.innerHTML = '';

    for (let row = 0; row < ROWS; row++) {
        for (let col = 0; col < COLS; col++) {
            const cell = document.createElement('div');
            cell.className = 'cell';
            cell.dataset.row = row;
            cell.dataset.col = col;
            cell.addEventListener('click', () => dropPiece(col));
            if (board[row][col] === 1) {
                cell.style.backgroundColor = 'red';
            } else if (board[row][col] === 2) {
                cell.style.backgroundColor = 'yellow';
            }
            boardElement.appendChild(cell);
        }
    }
}

// Drop a piece into the specified column
function dropPiece(col) {
    if (gameOver) return;

    for (let row = ROWS - 1; row >= 0; row--) {
        if (board[row][col] === 0) {
            board[row][col] = currentPlayer;
            renderBoard();
            if (checkWin(row, col)) {
                gameOver = true;
                alert(`Player ${currentPlayer} wins!`);
                return;
            }
            currentPlayer = currentPlayer === 1 ? 2 : 1;
            return;
        }
    }
}

// Check for a win condition
function checkWin(row, col) {
    const directions = [
        [0, 1], [1, 0], [1, 1], [-1, 1] // right, down, diagonal down-right, diagonal up-right
    ];

    for (const [dr, dc] of directions) {
        let count = 1;
        for (let step = 1; step < 4; step++) {
            const r = row + dr * step;
            const c = col + dc * step;
            if (r >= 0 && r < ROWS && c >= 0 && c < COLS && board[r][c] === currentPlayer) {
                count++;
            } else {
                break;
            }
        }
        for (let step = 1; step < 4; step++) {
            const r = row - dr * step;
            const c = col - dc * step;
            if (r >= 0 && r < ROWS && c >= 0 && c < COLS && board[r][c] === currentPlayer) {
                count++;
            } else {
                break;
            }
        }
        if (count >= 4) {
            return true;
        }
    }
    return false;
}

// Reset the game
function resetGame() {
    initBoard();
    currentPlayer = 1;
    gameOver = false;
    renderBoard();
}

// Initialize the game on page load
document.addEventListener('DOMContentLoaded', () => {
    initBoard();
    renderBoard();
});
