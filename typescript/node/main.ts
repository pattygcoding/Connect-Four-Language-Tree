// Constants
const ROWS = 6;
const COLS = 7;
const EMPTY = ' ';
const PLAYER1 = 'X';
const PLAYER2 = 'O';

// Game state
let board: string[][] = [];

// Initialize the game board
function initializeBoard(): void {
    board = Array.from({ length: ROWS }, () => Array.from({ length: COLS }, () => EMPTY));
}

// Display the game board
function displayBoard(): void {
    console.log(" 1 2 3 4 5 6 7");
    board.forEach(row => console.log("|" + row.join("|") + "|"));
    console.log("---------------");
}

// Drop a piece into a column
function dropPiece(col: number, player: string): boolean {
    for (let row = ROWS - 1; row >= 0; row--) {
        if (board[row][col] === EMPTY) {
            board[row][col] = player;
            return true;
        }
    }
    return false; // Column is full
}

// Check for a win condition
function checkWin(player: string): boolean {
    // Check horizontal
    for (let row = 0; row < ROWS; row++) {
        for (let col = 0; col <= COLS - 4; col++) {
            if (board[row][col] === player &&
                board[row][col + 1] === player &&
                board[row][col + 2] === player &&
                board[row][col + 3] === player) {
                return true;
            }
        }
    }

    // Check vertical
    for (let col = 0; col < COLS; col++) {
        for (let row = 0; row <= ROWS - 4; row++) {
            if (board[row][col] === player &&
                board[row + 1][col] === player &&
                board[row + 2][col] === player &&
                board[row + 3][col] === player) {
                return true;
            }
        }
    }

    // Check diagonal (top-left to bottom-right)
    for (let row = 0; row <= ROWS - 4; row++) {
        for (let col = 0; col <= COLS - 4; col++) {
            if (board[row][col] === player &&
                board[row + 1][col + 1] === player &&
                board[row + 2][col + 2] === player &&
                board[row + 3][col + 3] === player) {
                return true;
            }
        }
    }

    // Check diagonal (top-right to bottom-left)
    for (let row = 0; row <= ROWS - 4; row++) {
        for (let col = 3; col < COLS; col++) {
            if (board[row][col] === player &&
                board[row + 1][col - 1] === player &&
                board[row + 2][col - 2] === player &&
                board[row + 3][col - 3] === player) {
                return true;
            }
        }
    }

    return false;
}

// Main game loop
function playConnectFour(): void {
    initializeBoard();
    let currentPlayer = PLAYER1;
    let gameOver = false;

    while (!gameOver) {
        displayBoard();
        const playerPrompt = currentPlayer === PLAYER1 ? "Player 1" : "Player 2";
        const col = getUserInput(`${playerPrompt}'s turn. Enter column (1-7): `);

        if (col >= 1 && col <= COLS) {
            const colIndex = col - 1;
            if (dropPiece(colIndex, currentPlayer)) {
                if (checkWin(currentPlayer)) {
                    displayBoard();
                    console.log(`${playerPrompt} (${currentPlayer}) wins!`);
                    gameOver = true;
                } else if (board.every(row => row.every(cell => cell !== EMPTY))) {
                    displayBoard();
                    console.log("It's a draw! The board is full.");
                    gameOver = true;
                } else {
                    currentPlayer = currentPlayer === PLAYER1 ? PLAYER2 : PLAYER1;
                }
            } else {
                console.log("Column is full. Please choose another column.");
            }
        } else {
            console.log("Invalid input. Please enter a number between 1 and 7.");
        }
    }
}

// Helper function to get user input
function getUserInput(prompt: string): number {
    const input = promptSync(prompt);
    const col = parseInt(input.trim());
    return isNaN(col) ? -1 : col;
}

// Run the game
playConnectFour();
