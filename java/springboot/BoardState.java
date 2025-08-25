package com.yourcompany.connectfour;

public class BoardState {
    private char[][] board;
    private char currentPlayer;
    private char winner;
    private boolean gameOver;
    
    public BoardState() {
        this.board = new char[6][7];
        this.currentPlayer = 'R';
        this.winner = ' ';
        this.gameOver = false;
        
        // Initialize board with empty spaces
        for (int i = 0; i < 6; i++) {
            for (int j = 0; j < 7; j++) {
                board[i][j] = ' ';
            }
        }
    }
    
    // Getters and setters
    public char[][] getBoard() {
        return board;
    }
    
    public void setBoard(char[][] board) {
        this.board = board;
    }
    
    public char getCurrentPlayer() {
        return currentPlayer;
    }
    
    public void setCurrentPlayer(char currentPlayer) {
        this.currentPlayer = currentPlayer;
    }
    
    public char getWinner() {
        return winner;
    }
    
    public void setWinner(char winner) {
        this.winner = winner;
    }
    
    public boolean isGameOver() {
        return gameOver;
    }
    
    public void setGameOver(boolean gameOver) {
        this.gameOver = gameOver;
    }
}