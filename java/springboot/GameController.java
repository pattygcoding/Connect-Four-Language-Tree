package com.yourcompany.connectfour;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
public class GameController {

    private final GameService gameService;

    public GameController(GameService gameService) {
        this.gameService = gameService;
    }

    @PostMapping("/newgame")
    public ResponseEntity<String> startNewGame() {
        String gameId = gameService.createNewGame();
        return ResponseEntity.ok(gameId);
    }

    @PostMapping("/move/{gameId}/{column}")
    public ResponseEntity<String> makeMove(@PathVariable String gameId, @PathVariable int column) {
        MoveResult result = gameService.makeMove(gameId, column);
        return ResponseEntity.ok(result.getMessage());
    }

    @GetMapping("/board/{gameId}")
    public ResponseEntity<BoardState> getBoardState(@PathVariable String gameId) {
        BoardState boardState = gameService.getBoardState(gameId);
        return ResponseEntity.ok(boardState);
    }
}
