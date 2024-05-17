local ROWS = 6
local COLUMNS = 7

-- Initialize the board with empty spaces
local board = {}
for row = 1, ROWS do
    board[row] = {}
    for col = 1, COLUMNS do
        board[row][col] = ' '
    end
end

-- Function to print the board
local function printBoard()
    for row = 1, ROWS do
        for col = 1, COLUMNS do
            io.write("| " .. board[row][col] .. " ")
        end
        io.write("|\n")
    end
    for col = 1, COLUMNS do
        io.write("----")
    end
    io.write("-\n")
    for col = 1, COLUMNS do
        io.write("  " .. col .. " ")
    end
    io.write("\n")
end

-- Function to drop a disc into a column
local function dropDisc(column, disc)
    for row = ROWS, 1, -1 do
        if board[row][column] == ' ' then
            board[row][column] = disc
            return true
        end
    end
    return false -- Column is full
end

-- Function to check for a win
local function checkWin(disc)
    -- Check horizontal, vertical, and diagonal lines
    local directions = {
        {0, 1},  -- Horizontal
        {1, 0},  -- Vertical
        {1, 1},  -- Diagonal /
        {1, -1}  -- Diagonal \
    }

    for row = 1, ROWS do
        for col = 1, COLUMNS do
            if board[row][col] == disc then
                for _, direction in ipairs(directions) do
                    local count = 0
                    for i = 0, 3 do
                        local r = row + direction[1] * i
                        local c = col + direction[2] * i
                        if r > 0 and r <= ROWS and c > 0 and c <= COLUMNS and board[r][c] == disc then
                            count = count + 1
                        else
                            break
                        end
                    end
                    if count == 4 then
                        return true
                    end
                end
            end
        end
    end
    return false
end

-- Main game loop
local function playGame()
    local currentPlayer = 'X'
    while true do
        printBoard()
        print("Player " .. currentPlayer .. ", choose a column (1-" .. COLUMNS .. "):")
        local column = tonumber(io.read())
        if column and column >= 1 and column <= COLUMNS then
            if dropDisc(column, currentPlayer) then
                if checkWin(currentPlayer) then
                    printBoard()
                    print("Player " .. currentPlayer .. " wins!")
                    break
                end
                -- Switch player
                currentPlayer = (currentPlayer == 'X') and 'O' or 'X'
            else
                print("Column is full! Choose another column.")
            end
        else
            print("Invalid input! Please enter a column number between 1 and " .. COLUMNS .. ".")
        end
    end
end

-- Start the game
playGame()
