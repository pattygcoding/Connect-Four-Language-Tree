import tkinter as tk

def create_board():
    return [[None] * 7 for _ in range(6)]

def check_winner(board, piece):
    # Check horizontal locations for win
    for c in range(4):
        for r in range(6):
            if board[r][c] == board[r][c+1] == board[r][c+2] == board[r][c+3] == piece:
                return True
    # Check vertical locations for win
    for c in range(7):
        for r in range(3):
            if board[r][c] == board[r+1][c] == board[r+2][c] == board[r+3][c] == piece:
                return True
    # Check positively sloped diagonals
    for c in range(4):
        for r in range(3):
            if board[r][c] == board[r+1][c+1] == board[r+2][c+2] == board[r+3][c+3] == piece:
                return True
    # Check negatively sloped diagonals
    for c in range(4):
        for r in range(3, 6):
            if board[r][c] == board[r-1][c+1] == board[r-2][c+2] == board[r-3][c+3] == piece:
                return True
    return False

class ConnectFour:
    def __init__(self, master):
        self.master = master
        self.master.title("Connect Four")
        self.current_player = "Red"  # Red goes first
        self.piece = "O"  # Red is "O"
        self.board = create_board()
        self.buttons = []
        self.labels = [[None]*7 for _ in range(6)]
        frame = tk.Frame(self.master)
        frame.pack()
        
        # Creating column buttons to drop pieces
        for c in range(7):
            button = tk.Button(frame, text=str(c+1), command=lambda c=c: self.make_move(c))
            button.grid(row=0, column=c)
            self.buttons.append(button)
        
        # Setting up the grid of labels to display pieces
        for r in range(6):
            for c in range(7):
                label = tk.Label(frame, text=' ', font=('Arial', 20), width=4, height=2, borderwidth=2, relief="groove")
                label.grid(row=r+1, column=c)
                self.labels[r][c] = label

    def make_move(self, column):
        if self.current_player == "Red":  # Human player
            row = next((r for r in range(5, -1, -1) if self.board[r][column] is None), None)
            if row is None:
                return  # Column is full
            self.board[row][column] = self.current_player
            self.labels[row][column].config(text=self.piece, fg='Red')
            if check_winner(self.board, self.current_player):
                self.end_game(f"Player {self.current_player} wins!")
            self.current_player = "Black"
            self.piece = "O"
            self.master.after(500, self.ai_move)  # AI moves after a delay

    def ai_move(self):
        if self.current_player == "Black":  # AI player
            column, _ = self.minimax(self.board, 4, float('-inf'), float('inf'), True)
            if column is not None:
                row = next((r for r in range(5, -1, -1) if self.board[r][column] is None), None)
                if row is not None:
                    self.board[row][column] = self.current_player
                    self.labels[row][column].config(text=self.piece, fg='Black')
                    if check_winner(self.board, self.current_player):
                        self.end_game(f"Player {self.current_player} wins!")
            self.current_player = "Red"
            self.piece = "O"

    def minimax(self, board, depth, alpha, beta, maximizing_player):
        if depth == 0 or check_winner(board, "Red") or check_winner(board, "Black"):
            if check_winner(board, "Red"):
                return (None, -1000)
            elif check_winner(board, "Black"):
                return (None, 1000)
            else:
                return (None, 0)
        
        if maximizing_player:
            max_eval = float('-inf')
            best_column = None
            for col in range(7):
                row = next((r for r in range(5, -1, -1) if board[r][col] is None), None)
                if row is not None:
                    board[row][col] = "Black"
                    eval = self.minimax(board, depth - 1, alpha, beta, False)[1]
                    board[row][col] = None
                    if eval > max_eval:
                        max_eval = eval
                        best_column = col
                    alpha = max(alpha, eval)
                    if beta <= alpha:
                        break
            return best_column, max_eval
        else:
            min_eval = float('inf')
            best_column = None
            for col in range(7):
                row = next((r for r in range(5, -1, -1) if board[r][col] is None), None)
                if row is not None:
                    board[row][col] = "Red"
                    eval = self.minimax(board, depth - 1, alpha, beta, True)[1]
                    board[row][col] = None
                    if eval < min_eval:
                        min_eval = eval
                        best_column = col
                    beta = min(beta, eval)
                    if beta <= alpha:
                        break
            return best_column, min_eval

    def end_game(self, message):
        for button in self.buttons:
            button.config(state='disabled')
        tk.Label(self.master, text=message, font=('Arial', 20)).pack()

def main():
    root = tk.Tk()
    game = ConnectFour(root)
    root.mainloop()

if __name__ == "__main__":
    main()
