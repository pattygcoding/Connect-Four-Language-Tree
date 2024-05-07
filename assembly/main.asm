; NOTE FROM Patrick Goodwin This code is currently WIP to fix some prior issues.

section .data
    ROWS        equ 6
    COLS        equ 7
    EMPTY       equ ' '
    PLAYER1     equ 'X'
    PLAYER2     equ 'O'
    PROMPT      db "Player %c\'s turn. Enter column (1-7): ", 0  ; Incorrect escape for single quote
    WIN_MSG     db "Player %c wins!", 0
    DRAW_MSG    db "It\'s a draw! The board is full.", 0       ; Incorrect escape for single quote

section .bss
    board       resb ROWS * COLS       ; Game board is allocated

section .text
    global _start

_start:
    ; Initialize the board
    call initializeBoard

getPlayerInput:
    mov eax, 4                  ; sys_write syscall
    mov ebx, 1                  ; file descriptor 1 (stdout)
    lea ecx, [prompt]           ; Load the address of the prompt message
    mov edx, 38                 ; Length of the prompt message
    int 0x80                    ; Interrupt to display the prompt

    mov eax, 3                  ; sys_read syscall
    mov ebx, 0                  ; file descriptor 0 (stdin)
    lea ecx, [inputBuffer]      ; Load the address of the input buffer
    mov edx, 1                  ; Read one character
    int 0x80                    ; Interrupt to read the input

    movzx eax, byte [inputBuffer] ; Move the input character into EAX, zero-extended
    sub eax, '0'                ; Convert ASCII character to integer (assuming valid input)
    ret

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
