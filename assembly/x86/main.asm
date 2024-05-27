section .data
    board db ".......", 10, \
              ".......", 10, \
              ".......", 10, \
              ".......", 10, \
              ".......", 10, \
              ".......", 10, \
              "1234567", 10, 0  ; The board layout with newlines and a null terminator
    prompt1 db "Player 1, enter column (1-7): ", 0
    prompt2 db "Player 2, enter column (1-7): ", 0
    newline db 10, 0
    win1 db "Player 1 wins!", 10, 0
    win2 db "Player 2 wins!", 10, 0

section .bss
    input resb 1
    current_player resb 1

section .text
    global _start

_start:
    ; Initialize current_player to 1 (Player 1 starts)
    mov byte [current_player], 1

    ; Main loop
main_loop:
    ; Display the board
    mov eax, 4          ; sys_write
    mov ebx, 1          ; file descriptor (stdout)
    mov ecx, board      ; pointer to the board data
    mov edx, 56         ; number of bytes to write
    int 0x80

    ; Display the prompt based on current player
    mov al, [current_player]
    cmp al, 1
    je player1_prompt
    cmp al, 2
    je player2_prompt

player1_prompt:
    mov eax, 4          ; sys_write
    mov ebx, 1          ; file descriptor (stdout)
    mov ecx, prompt1    ; pointer to Player 1's prompt
    mov edx, 29         ; number of bytes to write
    int 0x80
    jmp read_input

player2_prompt:
    mov eax, 4          ; sys_write
    mov ebx, 1          ; file descriptor (stdout)
    mov ecx, prompt2    ; pointer to Player 2's prompt
    mov edx, 29         ; number of bytes to write
    int 0x80

read_input:
    ; Read input from stdin
    mov eax, 3          ; sys_read
    mov ebx, 0          ; file descriptor (stdin)
    mov ecx, input      ; pointer to the input buffer
    mov edx, 1          ; number of bytes to read
    int 0x80

    ; Convert input to column index (0-6)
    movzx ebx, byte [input]
    sub ebx, '1'
    cmp ebx, 6
    ja invalid_input

    ; Place the piece in the correct column
    call place_piece

    ; Check for vertical win
    call check_vertical_win

    ; Check for horizontal win
    call check_horizontal_win

    ; Alternate the player turn
    mov al, [current_player]
    cmp al, 1
    je switch_to_player2
    cmp al, 2
    je switch_to_player1

switch_to_player1:
    mov byte [current_player], 1
    jmp main_loop

switch_to_player2:
    mov byte [current_player], 2
    jmp main_loop

invalid_input:
    ; Handle invalid input (do nothing)
    jmp main_loop

place_piece:
    ; Place piece in the lowest available row in the specified column
    mov ecx, 5          ; start from the bottom row (5th index)
place_loop:
    mov edx, ecx
    imul edx, 8         ; multiply by 8 (7 chars + newline)
    add edx, ebx        ; add the column index
    add edx, board      ; add the base address of the board

    cmp byte [edx], '.' ; check if the cell is empty
    je place_found      ; if empty, place the piece
    
    loop place_loop     ; decrement ecx and repeat

    ret

place_found:
    ; Place 'X' or 'O' based on current player
    mov al, [current_player]
    cmp al, 1
    je place_X
    cmp al, 2
    je place_O

place_X:
    mov byte [edx], 'X'
    ret

place_O:
    mov byte [edx], 'O'
    ret

check_vertical_win:
    ; Check each column for a vertical win
    mov ebx, 0          ; start with the first column (0)
check_column:
    mov ecx, 0          ; start with the first row (0)
check_rows:
    mov edx, ecx
    imul edx, 8         ; multiply by 8 (7 chars + newline)
    add edx, ebx        ; add the column index
    add edx, board      ; add the base address of the board

    mov al, [edx]
    cmp al, '.'         ; if cell is empty, skip
    je next_row

    ; Check for 4 in a row vertically
    mov esi, edx        ; store the starting position
    mov edi, 1          ; initialize the counter
vertical_check:
    add esi, 8          ; move to the cell in the next row
    cmp byte [esi], al
    jne end_vertical_check ; if not the same, end check
    inc edi             ; increment the counter
    cmp edi, 4          ; if counter reaches 4, win
    je announce_winner
    jmp vertical_check

next_row:
    inc ecx
    cmp ecx, 3          ; only need to check up to row 3
    jl check_rows

    inc ebx
    cmp ebx, 7          ; check all 7 columns
    jl check_column

    ret

end_vertical_check:
    ret

check_horizontal_win:
    ; Check each row for a horizontal win
    mov ecx, 0          ; start with the first row (0)
check_row:
    mov ebx, 0          ; start with the first column (0)
check_columns:
    mov edx, ecx
    imul edx, 8         ; multiply by 8 (7 chars + newline)
    add edx, ebx        ; add the column index
    add edx, board      ; add the base address of the board

    mov al, [edx]
    cmp al, '.'         ; if cell is empty, skip
    je next_column

    ; Check for 4 in a row horizontally
    mov esi, edx        ; store the starting position
    mov edi, 1          ; initialize the counter
horizontal_check:
    inc esi             ; move to the next cell in the row
    cmp byte [esi], al
    jne end_horizontal_check ; if not the same, end check
    inc edi             ; increment the counter
    cmp edi, 4          ; if counter reaches 4, win
    je announce_winner
    jmp horizontal_check

next_column:
    inc ebx
    cmp ebx, 4          ; only need to check up to column 4
    jl check_columns

    inc ecx
    cmp ecx, 6          ; check all 6 rows
    jl check_row

    ret

end_horizontal_check:
    ret

announce_winner:
    ; Announce the winner and exit
    mov al, [current_player]
    cmp al, 1
    je player1_wins
    cmp al, 2
    je player2_wins

player1_wins:
    mov eax, 4          ; sys_write
    mov ebx, 1          ; file descriptor (stdout)
    mov ecx, win1       ; pointer to Player 1's win message
    mov edx, 14         ; number of bytes to write
    int 0x80
    jmp game_end

player2_wins:
    mov eax, 4          ; sys_write
    mov ebx, 1          ; file descriptor (stdout)
    mov ecx, win2       ; pointer to Player 2's win message
    mov edx, 14         ; number of bytes to write
    int 0x80
    jmp game_end

game_end:
    mov eax, 1          ; sys_exit
    xor ebx, ebx        ; status 0
    int 0x80
