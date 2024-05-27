open System

let board = Array2D.create 6 7 0
let mutable currentPlayer = 1

let printBoard () =
    Console.Clear()
    for row in 0 .. 5 do
        for col in 0 .. 6 do
            let cell = 
                match board.[row, col] with
                | 0 -> "."
                | 1 -> "X"
                | 2 -> "O"
                | _ -> "?"
            Console.Write(sprintf "%s " cell)
        Console.WriteLine()
    Console.WriteLine("1 2 3 4 5 6 7")

let dropDisc column =
    let mutable placed = false
    for row in 5 .. -1 .. 0 do
        if board.[row, column] = 0 && not placed then
            board.[row, column] <- currentPlayer
            placed <- true
    placed

let checkDirection row col rowDir colDir player =
    let mutable count = 0
    let mutable result = true
    for i in 0 .. 3 do
        let newRow = row + i * rowDir
        let newCol = col + i * colDir
        if newRow < 0 || newRow >= 6 || newCol < 0 || newCol >= 7 then
            result <- false
        elif board.[newRow, newCol] = player then
            count <- count + 1
        else
            result <- false
    count = 4 && result

let checkForWin player =
    let mutable win = false
    for row in 0 .. 5 do
        for col in 0 .. 6 do
            if checkDirection row col 1 0 player ||
               checkDirection row col 0 1 player ||
               checkDirection row col 1 1 player ||
               checkDirection row col 1 -1 player then
                win <- true
    win

[<EntryPoint>]
let main argv =
    let mutable gameOver = false
    while not gameOver do
        printBoard()
        printfn "Player %d's turn" currentPlayer
        printf "Enter column (1-7): "
        let input = Console.ReadLine()
        match Int32.TryParse(input) with
        | (true, column) when column >= 1 && column <= 7 ->
            let column = column - 1
            if dropDisc column then
                if checkForWin currentPlayer then
                    printBoard()
                    printfn "Player %d wins!" currentPlayer
                    gameOver <- true
                else
                    currentPlayer <- if currentPlayer = 1 then 2 else 1
            else
                printfn "Column is full, try again."
        | _ -> printfn "Invalid input, try again."
    0
