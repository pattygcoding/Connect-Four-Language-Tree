const std = @import("std");

const rows = 6;
const columns = 7;

const player1: u8 = 'X';
const player2: u8 = 'O';
const empty: u8 = ' ';

fn createBoard() [rows][columns]u8 {
    return [_][columns]u8{[_]u8{empty} ** columns} ** rows;
}

fn displayBoard(board: [rows][columns]u8) void {
    for (board) |row| {
        for (row) |cell| {
            std.debug.print("{c} ", .{cell});
        }
        std.debug.print("\n", .{});
    }
    for (columns) |i| {
        std.debug.print("{d} ", .{i + 1});
    }
    std.debug.print("\n", .{});
}

fn getColumn() !usize {
    var input: [2]u8 = undefined;
    _ = try std.io.getStdOut().writeAll("Enter column (1-7): ");
    _ = try std.io.getStdIn().readUntilDelimiterOrEof(&input, '\n');
    const col = @atoi(u8, input[0..1]);
    return col - 1;
}

fn isValidMove(board: [rows][columns]u8, col: usize) bool {
    return board[0][col] == empty;
}

fn dropToken(board: *[rows][columns]u8, col: usize, token: u8) void {
    for (0..rows) |i| {
        const row = rows - 1 - i;
        if (board[row][col] == empty {
            board[row][col] = token;
            return;
        }
    }
}

fn checkLine(board: [rows][columns]u8, token: u8, x: usize, y: usize, dx: isize, dy: isize) bool {
    var count: usize = 0;
    for (0..4) |i| {
        const nx = @intCast(usize, @intCast(isize, x) + i * dx);
        const ny = @intCast(usize, @intCast(isize, y) + i * dy);
        if (nx < rows and ny < columns and board[nx][ny] == token) {
            count += 1;
        } else {
            break;
        }
    }
    return count == 4;
}

fn checkWin(board: [rows][columns]u8, token: u8) bool {
    for (0..rows) |i| {
        for (0..columns) |j| {
            if (checkLine(board, token, i, j, 1, 0) or
                checkLine(board, token, i, j, 0, 1) or
                checkLine(board, token, i, j, 1, 1) or
                checkLine(board, token, i, j, 1, -1)) {
                return true;
            }
        }
    }
    return false;
}

fn isFull(board: [rows][columns]u8) bool {
    for (board) |row| {
        for (row) |cell| {
            if (cell == empty) {
                return false;
            }
        }
    }
    return true;
}

pub fn main() void {
    var board = createBoard();
    var currentPlayer: u8 = player1;

    while (true) {
        displayBoard(board);
        std.debug.print("Player {c}'s turn.\n", .{currentPlayer});

        var col: usize = 0;
        while (true) {
            col = switch (getColumn()) {
                error.InvalidInteger => {
                    std.debug.print("Invalid input. Try again.\n", .{});
                    continue;
                },
                |val| val,
            };
            if (col >= columns or !isValidMove(board, col)) {
                std.debug.print("Invalid move. Try again.\n", .{});
                continue;
            }
            break;
        }

        dropToken(&board, col, currentPlayer);

        if (checkWin(board, currentPlayer)) {
            displayBoard(board);
            std.debug.print("Player {c} wins!\n", .{currentPlayer});
            break;
        }

        if (isFull(board)) {
            displayBoard(board);
            std.debug.print("It's a draw!\n", .{});
            break;
        }

        currentPlayer = if (currentPlayer == player1) player2 else player1;
    }
}
