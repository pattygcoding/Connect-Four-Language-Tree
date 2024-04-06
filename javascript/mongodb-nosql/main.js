const { MongoClient } = require('mongodb');
const readline = require('readline');

const ROWS = 6;
const COLS = 7;
const EMPTY = ' ';
const PLAYER1 = 'X';
const PLAYER2 = 'O';

let dbClient;
let gameCollection;

async function connectToDatabase() {
    const uri = 'mongodb://localhost:27017';
    const client = new MongoClient(uri);

    try {
        await client.connect();
        console.log('Connected to MongoDB');

        dbClient = client;
        const database = client.db('connect_four');
        gameCollection = database.collection('games');
    } catch (error) {
        console.error('Error connecting to MongoDB:', error);
        process.exit(1);
    }
}

async function initializeGame() {
    const game = {
        board: Array.from({ length: ROWS }, () => Array.from({ length: COLS }, () => EMPTY)),
        player1Turn: true,
        winner: null
    };

    try {
        const result = await gameCollection.insertOne(game);
        return result.insertedId;
    } catch (error) {
        console.error('Error initializing game:', error);
        return null;
    }
}

async function displayBoard(board) {
    console.log(' 1 2 3 4 5 6 7');
    for (const row of board) {
        console.log('|' + row.join('|') + '|');
    }
    console.log('---------------');
}

async function dropPiece(gameId, col) {
    const game = await gameCollection.findOne({ _id: gameId });
    if (!game) {
        console.error('Game not found');
        return false;
    }

    const board = game.board;
    const player = game.player1Turn ? PLAYER1 : PLAYER2;

    for (let row = ROWS - 1; row >= 0; row--) {
        if (board[row][col] === EMPTY) {
            board[row][col] = player;
            const nextPlayerTurn = !game.player1Turn;

            await gameCollection.updateOne(
                { _id: gameId },
                { $set: { board, player1Turn: nextPlayerTurn } }
            );

            return true;
        }
    }

    console.log('Column is full');
    return false;
}

async function checkWin(board, row, col) {
    const player = board[row][col];

    // Check vertical
    let count = 0;
    for (let r = row; r < ROWS; r++) {
        if (board[r][col] === player) {
            count++;
        } else {
            break;
        }
    }
    if (count >= 4) {
        return true;
    }

    // Check horizontal
    count = 0;
    for (let c = 0; c < COLS; c++) {
        if (board[row][c] === player) {
            count++;
        } else {
            count = 0;
        }
        if (count >= 4) {
            return true;
        }
    }

    // Check diagonal (bottom-left to top-right)
    count = 0;
    for (let r = row, c = col; r < ROWS && c < COLS; r++, c++) {
        if (board[r][c] === player) {
            count++;
        } else {
            count = 0;
        }
        if (count >= 4) {
            return true;
        }
    }

    // Check diagonal (top-left to bottom-right)
    count = 0;
    for (let r = row, c = col; r >= 0 && c < COLS; r--, c++) {
        if (board[r][c] === player) {
            count++;
        } else {
            count = 0;
        }
        if (count >= 4) {
            return true;
        }
    }

    return false;
}

async function playConnectFour(gameId) {
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });

    let col;
    while (true) {
        const game = await gameCollection.findOne({ _id: gameId });
        if (!game) {
            console.error('Game not found');
            break;
        }

        await displayBoard(game.board);

        const currentPlayer = game.player1Turn ? PLAYER1 : PLAYER2;
        console.log(`Player ${currentPlayer}'s turn.`);

        await new Promise((resolve) => {
            rl.question('Enter column (1-7): ', (answer) => {
                col = parseInt(answer) - 1;
                resolve();
            });
        });

        if (col < 0 || col >= COLS) {
            console.log('Invalid column. Please enter a number between 1 and 7.');
            continue;
        }

        const success = await dropPiece(gameId, col);
        if (!success) {
            continue;
        }

        const row = game.board.findIndex(row => row[col] !== EMPTY);
        const isWin = await checkWin(game.board, row, col);
        if (isWin) {
            await displayBoard(game.board);
            console.log(`Player ${currentPlayer} wins!`);
            break;
        }
    }

    rl.close();
}

async function main() {
    await connectToDatabase();
    const gameId = await initializeGame();
    if (!gameId) {
        console.error('Failed to initialize game');
        process.exit(1);
    }
    await playConnectFour(gameId);
    dbClient.close();
}

main().catch(console.error);
