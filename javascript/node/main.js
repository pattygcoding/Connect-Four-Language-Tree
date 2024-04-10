const readline = require('readline');

const ROWS = 6;
const COLS = 7;
const EMPTY = ' ';
const PLAYER1_PIECE = 'X';
const PLAYER2_PIECE = 'O';

let board = [];

// Initialize the game board
function initializeBoard() {
    board = Array.from({ length: ROWS }, () => Array.from({ length: COLS }, () => EMPTY));
}

// Display the game board
function displayBoard() {
    for (let row = 0; row < ROWS; row++) {
        console.log(board[row].join(' | '));
    }
    console.log('1   2   3   4   5   6   7');
    console.log('');
}

// Prompt player for column input
function promptPlayer(player) {
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });

    return new Promise(resolve => {
        rl.question(`Player ${player}, enter column (1-7): `, answer => {
            rl.close();
            resolve(parseInt(answer) - 1); // Convert to zero-based index
        });
    });
}

// Drop a piece into the board
function dropPiece(column, piece) {
    for (let row = ROWS - 1; row >= 0; row--) {
        if (board[row][column] === EMPTY) {
            board[row][column] = piece;
            return true; // Valid move
        }
    }
    return false; // Column is full
}

// Check for win conditions
function checkWin(piece) {
    // Check horizontal
    for (let row = 0; row < ROWS; row++) {
        for (let col = 0; col <= COLS - 4; col++) {
            if (board[row][col] === piece &&
                board[row][col+1] === piece &&
                board[row][col+2] === piece &&
                board[row][col+3] === piece) {
                return true;
            }
        }
    }

    // Check vertical
    for (let col = 0; col < COLS; col++) {
        for (let row = 0; row <= ROWS - 4; row++) {
            if (board[row][col] === piece &&
                board[row+1][col] === piece &&
                board[row+2][col] === piece &&
                board[row+3][col] === piece) {
                return true;
            }
        }
    }

    // Check diagonal
    for (let row = 0; row <= ROWS - 4; row++) {
        for (let col = 0; col <= COLS - 4; col++) {
            if (board[row][col] === piece &&
                board[row+1][col+1] === piece &&
                board[row+2][col+2] === piece &&
                board[row+3][col+3] === piece) {
                return true;
            }
        }
    }

    // Check anti-diagonal
    for (let row = 0; row <= ROWS - 4; row++) {
        for (let col = 3; col < COLS; col++) {
            if (board[row][col] === piece &&
                board[row+1][col-1] === piece &&
                board[row+2][col-2] === piece &&
                board[row+3][col-3] === piece) {
                return true;
            }
        }
    }

    return false;
}

// Main game loop
async function playConnectFour() {
    let currentPlayer = 1;
    let gameOver = false;

    initializeBoard();

    while (!gameOver) {
        displayBoard();

        const column = await promptPlayer(currentPlayer);
        if (column < 0 || column >= COLS || board[0][column] !== EMPTY) {
            console.log('Invalid move. Try again.');
            continue;
        }

        const piece = (currentPlayer === 1) ? PLAYER1_PIECE : PLAYER2_PIECE;
        if (!dropPiece(column, piece)) {
            console.log('Column is full. Try again.');
            continue;
        }

        if (checkWin(piece)) {
            displayBoard();
            console.log(`Player ${currentPlayer} wins!`);
            gameOver = true;
        } else if (board.every(row => row.every(cell => cell !== EMPTY))) {
            displayBoard();
            console.log('It\'s a draw!');
            gameOver = true;
        }

        currentPlayer = (currentPlayer === 1) ? 2 : 1;
    }
}

// Start the game
playConnectFour();
