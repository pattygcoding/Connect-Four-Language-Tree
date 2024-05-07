program ConnectFour
    implicit none

    integer, parameter :: rows = 6
    integer, parameter :: cols = 7
    character(len=1), parameter :: empty = ' '
    character(len=1), parameter :: player1 = 'X'
    character(len=1), parameter :: player2 = 'O'

    character(len=1) :: board(rows, cols)
    character(len=1) :: currentPlayer
    logical :: gameOver
    integer :: col, row, i

    ! Initialize the game board
    call initializeBoard(board)

    ! Start the game loop
    currentPlayer = player1
    gameOver = .false.

    do while (.not. gameOver)
        ! Display the game board
        call displayBoard(board)

        ! Get player's move
        print *, 'Player ', currentPlayer, 's turn. Enter column (1-7): '
        read(*, *) col

        ! Drop the player's piece into the board
        if (isValidMove(board, col)) then
            call dropPiece(board, col, currentPlayer)

            ! Check for win condition
            if (checkWin(board, currentPlayer)) then
                call displayBoard(board)
                print *, 'Player ', currentPlayer, ' wins!'
                gameOver = .true.
            else
                ! Check for draw condition
                if (isBoardFull(board)) then
                    call displayBoard(board)
                    print *, 'Its a draw! The board is full.'
                    gameOver = .true.
                else
                    ! Switch player
                    if (currentPlayer == player1) then
                        currentPlayer = player2
                    else
                        currentPlayer = player1
                    end if
                end if
            end if
        else
            print *, 'Invalid move. Please choose a valid column (1-7).'
        end if
    end do

contains

    ! Subroutine to initialize the game board
    subroutine initializeBoard(board)
        character(len=1), intent(out) :: board(rows, cols)
        integer :: i, j

        do i = 1, rows
            do j = 1, cols
                board(i, j) = empty
            end do
        end do
    end subroutine initializeBoard

    ! Subroutine to display the game board
subroutine displayBoard(board)
    character(len=1), intent(in) :: board(rows, cols)
    integer :: i, j

    print *, '1 2 3 4 5 6 7'
    do i = 1, rows
        ! Start the row with a separator
        write(*, '(A)', advance='no') '|'
        ! Loop over each column in the row
        do j = 1, cols
            ! Print each board cell followed by a separator, without advancing to the next line
            write(*, '(A, A)', advance='no') board(i, j), '|'
        end do
        ! After finishing the row, advance to the next line
        print *
    end do
    print *, '---------------'
end subroutine displayBoard


    ! Function to check if a move is valid
    logical function isValidMove(board, col)
        character(len=1), intent(in) :: board(rows, cols)
        integer, intent(in) :: col

        if (col < 1 .or. col > cols) then
            isValidMove = .false.
        else
            isValidMove = (board(1, col) == empty)
        end if
    end function isValidMove

    ! Subroutine to drop a piece into the board
    subroutine dropPiece(board, col, player)
        character(len=1), intent(inout) :: board(rows, cols)
        integer, intent(in) :: col
        character(len=1), intent(in) :: player

        do row = rows, 1, -1
            if (board(row, col) == empty) then
                board(row, col) = player
                exit
            end if
        end do
    end subroutine dropPiece

    ! Function to check for a win condition
    logical function checkWin(board, player)
        character(len=1), intent(in) :: board(rows, cols)
        character(len=1), intent(in) :: player
        integer :: i, j

        ! Check horizontal
        do i = 1, rows
            do j = 1, cols - 3
                if (board(i, j) == player .and. &
                    board(i, j+1) == player .and. &
                    board(i, j+2) == player .and. &
                    board(i, j+3) == player) then
                    checkWin = .true.
                    return
                end if
            end do
        end do

        ! Check vertical
        do j = 1, cols
            do i = 1, rows - 3
                if (board(i, j) == player .and. &
                    board(i+1, j) == player .and. &
                    board(i+2, j) == player .and. &
                    board(i+3, j) == player) then
                    checkWin = .true.
                    return
                end if
            end do
        end do

        ! Check diagonal (top-left to bottom-right)
        do i = 1, rows - 3
            do j = 1, cols - 3
                if (board(i, j) == player .and. &
                    board(i+1, j+1) == player .and. &
                    board(i+2, j+2) == player .and. &
                    board(i+3, j+3) == player) then
                    checkWin = .true.
                    return
                end if
            end do
        end do

        ! Check diagonal (top-right to bottom-left)
        do i = 1, rows - 3
            do j = 4, cols
                if (board(i, j) == player .and. &
                    board(i+1, j-1) == player .and. &
                    board(i+2, j-2) == player .and. &
                    board(i+3, j-3) == player) then
                    checkWin = .true.
                    return
                end if
            end do
        end do

        ! No win condition found
        checkWin = .false.
    end function checkWin

    ! Function to check if the board is full (draw condition)
    logical function isBoardFull(board)
        character(len=1), intent(in) :: board(rows, cols)
        integer :: i, j

        do i = 1, rows
            do j = 1, cols
                if (board(i, j) == empty) then
                    isBoardFull = .false.
                    return
                end if
            end do
        end do

        ! Board is full (draw condition)
        isBoardFull = .true.
    end function isBoardFull

end program ConnectFour