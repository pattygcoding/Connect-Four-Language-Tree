package com.yourcompany.connectfour;

public class MoveResult {
    private boolean success;
    private String message;
    private char winner;
    private boolean gameOver;
    
    public MoveResult(boolean success, String message) {
        this.success = success;
        this.message = message;
        this.winner = ' ';
        this.gameOver = false;
    }
    
    public MoveResult(boolean success, String message, char winner, boolean gameOver) {
        this.success = success;
        this.message = message;
        this.winner = winner;
        this.gameOver = gameOver;
    }
    
    // Getters and setters
    public boolean isSuccess() {
        return success;
    }
    
    public void setSuccess(boolean success) {
        this.success = success;
    }
    
    public String getMessage() {
        return message;
    }
    
    public void setMessage(String message) {
        this.message = message;
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