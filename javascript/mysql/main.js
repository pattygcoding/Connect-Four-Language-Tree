const mysql = require('mysql');
const readline = require('readline');

const ROWS = 6;
const COLS = 7;
const EMPTY = ' ';
const PLAYER1 = 'X';
const PLAYER2 = 'O';

// Connect to MySQL database
const connection = mysql.createConnection({
    host: 'localhost',
    user: 'your_username',
    password: 'your_password',
    database: 'connect_four'
});

// Initialize the game board in the database
function initializeGame(callback) {
    const createTableQuery = `
        CREATE TABLE IF NOT EXISTS games (
            id INT AUTO_INCREMENT PRIMARY KEY,
            board JSON NOT NULL,
            player1Turn BOOLEAN NOT NULL,
            winner CHAR(1) DEFAULT NULL
        )
    `;
    connection.query(createTableQuery, (err) => {
        if (err) {
            callback(err);
        } else {
            const insertGameQuery = `
                INSERT INTO games (board, player1Turn)
                VALUES ('${JSON.stringify(Array.from({ length: ROWS }, () => Array.from({ length: COLS }, () => EMPTY)))}', true)
            `;
            connection.query(insertGameQuery, (err, result) => {
                if (err) {
                    callback(err);
                } else {
                    callback(null, result.insertId);
                }
            });
        }
    });
}

// Display the game board from the database
function displayBoard(board) {
    console.log(' 1 2 3 4 5 6 7');
    for (const row of board) {
        console.log('|' + row.join('|') + '|');
    }
    console.log('---------------');
}

// Drop a piece into a column in the database
function dropPiece(gameId, col, player, callback) {
    const selectGameQuery = `SELECT * FROM games WHERE id = ${gameId}`;
    connection.query(selectGameQuery, (err, results) => {
        if (err) {
            callback(err);
        } else {
            const game = results[0];
            const board = JSON.parse(game.board);

            for (let row = ROWS - 1; row >= 0; row--) {
                if (board[row][col] === EMPTY) {
                    board[row][col] = player;
                    const nextPlayerTurn = !game.player1Turn;

                    const updateGameQuery = `
                        UPDATE games
                        SET board = '${JSON.stringify(board)}', player1Turn = ${nextPlayerTurn}
                        WHERE id = ${gameId}
                    `;
                    connection.query(updateGameQuery, (err) => {
                        if (err) {
                            callback(err);
                        } else {
                            callback(null);
                        }
                    });

                    return;
                }
            }

            callback('Column is full');
        }
    });
}

// Check for a win condition in the database
function checkWin(board, row, col, player) {
    // Implement win checking logic here
    // (This function is left as an exercise)
    return false;
}

// Play the Connect Four game
function playConnectFour(gameId) {
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });

    let col;
    let currentPlayer = PLAYER1;
    displayBoard(Array.from({ length: ROWS }, () => Array.from({ length: COLS }, () => EMPTY)));

    rl.question('Enter column (1-7): ', (answer) => {
        col = parseInt(answer) - 1;
        dropPiece(gameId, col, currentPlayer, (err) => {
            if (err) {
                console.error(err);
            } else {
                displayBoard(Array.from({ length: ROWS }, () => Array.from({ length: COLS }, () => EMPTY))); // Replace with logic to fetch updated board from database

                // Check for win condition
                if (checkWin(Array.from({ length: ROWS }, () => Array.from({ length: COLS }, () => EMPTY)), 0, col, currentPlayer)) {
                    console.log(`Player ${currentPlayer} wins!`);
                } else {
                    // Next player's turn
                    currentPlayer = currentPlayer === PLAYER1 ? PLAYER2 : PLAYER1;
                    playConnectFour(gameId); // Recursive call for next player's turn
                }
            }
            rl.close();
        });
    });
}

// Main function
function main() {
    connection.connect((err) => {
        if (err) {
            console.error('Error connecting to MySQL:', err);
            return;
        }
        console.log('Connected to MySQL');

        initializeGame((err, gameId) => {
            if (err) {
                console.error('Error initializing game:', err);
            } else {
                console.log(`Game initialized with ID: ${gameId}`);
                playConnectFour(gameId);
            }
        });
    });
}

// Run the main function
main();
