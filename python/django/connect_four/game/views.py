from django.shortcuts import render, get_object_or_404, redirect
from django.http import JsonResponse
from .models import Game, Move
from django.views.decorators.csrf import csrf_exempt

def index(request):
    game = Game.objects.create(board=[['.' for _ in range(7)] for _ in range(6)])
    return render(request, 'game/index.html', {
        'game': game,
        'rows': range(6),
        'cols': range(7)
    })

def check_winner(board, player):
    # Check horizontal
    for row in board:
        for col in range(4):
            if row[col] == row[col + 1] == row[col + 2] == row[col + 3] == player:
                return True
    # Check vertical
    for col in range(7):
        for row in range(3):
            if board[row][col] == board[row + 1][col] == board[row + 2][col] == board[row + 3][col] == player:
                return True
    # Check diagonal /
    for row in range(3):
        for col in range(4):
            if board[row][col] == board[row + 1][col + 1] == board[row + 2][col + 2] == board[row + 3][col + 3] == player:
                return True
    # Check diagonal \
    for row in range(3, 6):
        for col in range(4):
            if board[row][col] == board[row - 1][col + 1] == board[row - 2][col + 2] == board[row - 3][col + 3] == player:
                return True
    return False

@csrf_exempt
def drop_disc(request, game_id):
    game = get_object_or_404(Game, id=game_id)
    if request.method == 'POST':
        col = int(request.POST['col'])
        player = game.current_player

        board = game.board
        for row in range(5, -1, -1):
            if board[row][col] == '.':
                board[row][col] = player
                Move.objects.create(game=game, column=col, player=player, move_number=len(game.moves.all()) + 1)
                break

        winner = check_winner(board, player)
        if winner:
            game.winner = player
        game.current_player = 'Y' if game.current_player == 'R' else 'R'
        game.board = board
        game.save()

        return JsonResponse({'board': board, 'winner': winner})
