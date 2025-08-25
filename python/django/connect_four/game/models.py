from django.db import models
import json

class Game(models.Model):
    board = models.JSONField(default=list)
    current_player = models.CharField(max_length=1, default='R')
    winner = models.CharField(max_length=1, blank=True, null=True)
    created_at = models.DateTimeField(auto_now_add=True)
    
    def __str__(self):
        return f"Game {self.id} - Player: {self.current_player}"

class Move(models.Model):
    game = models.ForeignKey(Game, on_delete=models.CASCADE, related_name='moves')
    column = models.IntegerField()
    player = models.CharField(max_length=1)
    move_number = models.IntegerField()
    created_at = models.DateTimeField(auto_now_add=True)
    
    class Meta:
        ordering = ['move_number']
    
    def __str__(self):
        return f"Game {self.game.id} - Move {self.move_number}: Player {self.player} -> Column {self.column}"
