section .data
    ROWS        equ 6
    COLS        equ 7
    EMPTY       equ ' '
    PLAYER1     equ 'X'
    PLAYER2     equ 'O'
    PROMPT      db 'Player %c\'s turn. Enter column (1-7): ', 0
    WIN_MSG     db 'Player %c wins!', 0
    DRAW_MSG    db 'It\'s a draw! The board is full.', 0

section .bss
    board       resb ROWS * COLS       ; Game board

section .text
    global _start

_start:
    ; Initialize the board
    call initializeBoard

    ; Main game loop
gameLoop:
    ; Display the board
    call displayBoard

    ; Determine current player
    mov dl, PLAYER1
    call getPlayerInput

    ; Check if current player wins
    mov dl, PLAYER1
    call checkWin
    jnz playerWins

    ; Check if the board is full (draw)
    call isBoardFull
    jnz gameOverDraw

    ; Switch player
    mov dl, PLAYER2
    call getPlayerInput

    ; Check if player 2 wins
    mov dl, PLAYER2
    call checkWin
    jnz playerWins

    ; Check if the board is full (draw)
    call isBoardFull
    jnz gameOverDraw

    ; Continue game loop
    jmp gameLoop

playerWins:
    ; Display win message
    mov eax, 4
    mov ebx, 1
    mov ecx, WIN_MSG
    call printf

    ; Exit program
    mov eax, 1
    xor ebx, ebx
    int 0x80

gameOverDraw:
    ; Display draw message
    mov eax, 4
    mov ebx, 1
    mov ecx, DRAW_MSG
    call printf

    ; Exit program
    mov eax, 1
    xor ebx, ebx
    int 0x80

initializeBoard:
    ; Clear the board
    mov edi, board
    mov ecx, ROWS * COLS
    mov al, EMPTY
    rep stosb
    ret

displayBoard:
    ; Display the board
    mov esi, board
    mov ecx, ROWS
displayRow:
    push ecx        ; Save outer loop counter
    mov ecx, COLS
    mov edi, esi    ; Point edi to current row
    rep movsb       ; Copy row to edi
    mov eax, 4
    mov ebx, 1
    mov ecx, prompt
    call printf
    pop ecx         ; Restore outer loop counter
    loop displayRow
    ret

getPlayerInput:
    ; Prompt player for input
    mov eax, 4
    mov ebx, 1
    mov ecx, PROMPT
    int 0x80

    ; Read player input (column number)
    mov eax, 3
    mov ebx, 0
    lea ecx, [esi]  ; Input buffer (reuse esi)
    mov edx, 2      ; Read up to 2 bytes (column + newline)
    int 0x80

    ; Convert column input to 0-based index
    sub byte [esi], '1'
    ret

checkWin:
    ; Check horizontal
    mov edi, board
    mov ecx, ROWS
horizLoop:
    push ecx        ; Save outer loop counter
    mov esi, edi    ; Point esi to current row
    mov ecx, COLS - 3
horizCheck:
    cmpsb           ; Compare four consecutive characters
    jne notHorizWin
    loop horizCheck
    ; Horizontal win found
    pop ecx         ; Restore outer loop counter
    ret
notHorizWin:
    pop ecx         ; Restore outer loop counter
    loop horizLoop
    ret

isBoardFull:
    ; Check if the board is full
    mov esi, board
    mov ecx, ROWS * COLS
    xor al, al      ; Empty flag
isFullLoop:
    lodsb           ; Load next board cell
    cmp al, EMPTY
    jne notFull
    dec ecx         ; Decrement counter if empty cell found
    jnz isFullLoop
    ; Board is full (counter reached zero)
    mov al, 1
    ret
notFull:
    xor al, al      ; Reset to zero
    ret

section .data
    prompt      db 'Player %c\'s turn. Enter column (1-7): ', 0
    winMsg      db 'Player %c wins!', 0
    drawMsg     db 'It\'s a draw! The board is full.', 0
