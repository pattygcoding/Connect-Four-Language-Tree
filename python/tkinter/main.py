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
        # Place a piece in the first available row in the selected column
        row = next((r for r in range(5, -1, -1) if self.board[r][column] is None), None)
        if row is None:
            return  # Column is full
        self.board[row][column] = self.current_player
        self.labels[row][column].config(text=self.piece, fg='Red' if self.current_player == "Red" else 'Black')
        
        if check_winner(self.board, self.current_player):
            self.end_game(f"Player {self.current_player} wins!")
        
        # Switch player
        if self.current_player == "Red":
            self.current_player = "Black"
            self.piece = "O"
        else:
            self.current_player = "Red"
            self.piece = "O"

    def end_game(self, message):
        # Disable all buttons after the game ends
        for button in self.buttons:
            button.config(state='disabled')
        tk.Label(self.master, text=message, font=('Arial', 20)).pack()

def main():
    root = tk.Tk()
    game = ConnectFour(root)
    root.mainloop()

if __name__ == "__main__":
    main()
