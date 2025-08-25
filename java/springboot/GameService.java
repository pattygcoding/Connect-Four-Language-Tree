package com.yourcompany.connectfour;

import org.springframework.stereotype.Service;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@Service
public class GameService {
    private Map<String, BoardState> games = new HashMap<>();
    
    public String createNewGame() {
        String gameId = UUID.randomUUID().toString();
        games.put(gameId, new BoardState());
        return gameId;
    }
    
    public MoveResult makeMove(String gameId, int column) {
        BoardState game = games.get(gameId);
        if (game == null) {
            return new MoveResult(false, "Game not found");
        }
        
        if (game.isGameOver()) {
            return new MoveResult(false, "Game is already over");
        }
        
        if (column < 0 || column >= 7) {
            return new MoveResult(false, "Invalid column");
        }
        
        char[][] board = game.getBoard();
        
        // Check if column is full
        if (board[0][column] != ' ') {
            return new MoveResult(false, "Column is full");
        }
        
        // Drop piece
        int row = -1;
        for (int i = 5; i >= 0; i--) {
            if (board[i][column] == ' ') {
                board[i][column] = game.getCurrentPlayer();
                row = i;
                break;
            }
        }
        
        // Check for winner
        char winner = checkWinner(board, row, column);
        if (winner != ' ') {
            game.setWinner(winner);
            game.setGameOver(true);
            return new MoveResult(true, "Player " + winner + " wins!", winner, true);
        }
        
        // Check for draw
        if (isBoardFull(board)) {
            game.setGameOver(true);
            return new MoveResult(true, "It's a draw!", ' ', true);
        }
        
        // Switch player
        game.setCurrentPlayer(game.getCurrentPlayer() == 'R' ? 'Y' : 'R');
        
        return new MoveResult(true, "Move successful");
    }
    
    public BoardState getBoardState(String gameId) {
        return games.get(gameId);
    }
    
    private char checkWinner(char[][] board, int lastRow, int lastCol) {
        char player = board[lastRow][lastCol];
        
        // Check horizontal
        int count = 1;
        // Check left
        for (int i = lastCol - 1; i >= 0 && board[lastRow][i] == player; i--) {
            count++;
        }
        // Check right
        for (int i = lastCol + 1; i < 7 && board[lastRow][i] == player; i++) {
            count++;
        }
        if (count >= 4) return player;
        
        // Check vertical
        count = 1;
        for (int i = lastRow + 1; i < 6 && board[i][lastCol] == player; i++) {
            count++;
        }
        if (count >= 4) return player;
        
        // Check diagonal (top-left to bottom-right)
        count = 1;
        // Check up-left
        for (int i = lastRow - 1, j = lastCol - 1; i >= 0 && j >= 0 && board[i][j] == player; i--, j--) {
            count++;
        }
        // Check down-right
        for (int i = lastRow + 1, j = lastCol + 1; i < 6 && j < 7 && board[i][j] == player; i++, j++) {
            count++;
        }
        if (count >= 4) return player;
        
        // Check diagonal (top-right to bottom-left)
        count = 1;
        // Check up-right
        for (int i = lastRow - 1, j = lastCol + 1; i >= 0 && j < 7 && board[i][j] == player; i--, j++) {
            count++;
        }
        // Check down-left
        for (int i = lastRow + 1, j = lastCol - 1; i < 6 && j >= 0 && board[i][j] == player; i++, j--) {
            count++;
        }
        if (count >= 4) return player;
        
        return ' ';
    }
    
    private boolean isBoardFull(char[][] board) {
        for (int j = 0; j < 7; j++) {
            if (board[0][j] == ' ') {
                return false;
            }
        }
        return true;
    }
}