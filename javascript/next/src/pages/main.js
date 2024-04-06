// pages/main.js

import { useState } from 'react';
import styles from '../styles/Home.module.css';

const Player = {
  None: 0,
  One: 1,
  Two: 2
};

export default function Main() {
  const [rows, setRows] = useState(6);
  const [cols, setCols] = useState(7);
  const [currentPlayer, setCurrentPlayer] = useState(Player.One);
  const [board, setBoard] = useState([]);
  const [gameOver, setGameOver] = useState(false);

  // Initialize the game board
  const initializeBoard = () => {
    const initialBoard = [];
    for (let row = 0; row < rows; row++) {
      initialBoard[row] = [];
      for (let col = 0; col < cols; col++) {
        initialBoard[row][col] = Player.None;
      }
    }
    setBoard(initialBoard);
    setGameOver(false);
  };

  // Handle player's move
  const dropChip = (col) => {
    if (gameOver) return;
    for (let row = rows - 1; row >= 0; row--) {
      if (board[row][col] === Player.None) {
        const updatedBoard = [...board];
        updatedBoard[row][col] = currentPlayer;
        setBoard(updatedBoard);
        if (checkWin(row, col)) {
          setGameOver(true);
          alert(`Player ${currentPlayer} wins!`);
        } else {
          setCurrentPlayer(currentPlayer === Player.One ? Player.Two : Player.One);
        }
        break;
      }
    }
  };

  // Check for a winning move
  const checkWin = (row, col) => {
    // Implement win checking logic (left as an exercise)
    return false;
  };

  // Restart the game
  const restartGame = () => {
    initializeBoard();
    setCurrentPlayer(Player.One);
  };

  // Render the game board
  const renderBoard = () => {
    return (
      <div className={styles.board}>
        {board.map((row, rowIndex) => (
          <div className={styles.row} key={rowIndex}>
            {row.map((cell, colIndex) => (
              <div
                className={styles.cell}
                key={colIndex}
                onClick={() => dropChip(colIndex)}
              >
                <div className={`${styles.token} ${cell === Player.One ? styles.playerOne : styles.playerTwo}`} />
              </div>
            ))}
          </div>
        ))}
      </div>
    );
  };

  // Initialize the board on component mount
  useState(() => {
    initializeBoard();
  }, []);

  return (
    <div className={styles.container}>
      <h1 className={styles.title}>Connect 4 Game</h1>
      {renderBoard()}
      <button className={styles.button} onClick={restartGame}>Restart Game</button>
    </div>
  );
}
