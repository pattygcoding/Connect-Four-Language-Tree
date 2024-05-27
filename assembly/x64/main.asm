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
    mov rax, 1          ; sys_write
    mov rdi, 1          ; file descriptor (stdout)
    mov rsi, board      ; pointer to the board data
    mov rdx, 56         ; number of bytes to write
    syscall

    ; Display the prompt based on current player
    mov al, [current_player]
    cmp al, 1
    je player1_prompt
    cmp al, 2
    je player2_prompt

player1_prompt:
    mov rax, 1          ; sys_write
    mov rdi, 1          ; file descriptor (stdout)
    mov rsi, prompt1    ; pointer to Player 1's prompt
    mov rdx, 29         ; number of bytes to write
    syscall
    jmp read_input

player2_prompt:
    mov rax, 1          ; sys_write
    mov rdi, 1          ; file descriptor (stdout)
    mov rsi, prompt2    ; pointer to Player 2's prompt
    mov rdx, 29         ; number of bytes to write
    syscall

read_input:
    ; Read input from stdin
    mov rax, 0          ; sys_read
    mov rdi, 0          ; file descriptor (stdin)
    mov rsi, input      ; pointer to the input buffer
    mov rdx, 1          ; number of bytes to read
    syscall

    ; Convert input to column index (0-6)
    movzx rbx, byte [input]
    sub rbx, '1'
    cmp rbx, 6
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
    mov rcx, 5          ; start from the bottom row (5th index)
place_loop:
    mov rdx, rcx
    imul rdx, 8         ; multiply by 8 (7 chars + newline)
    add rdx, rbx        ; add the column index
    add rdx, board      ; add the base address of the board

    cmp byte [rdx], '.' ; check if the cell is empty
    je place_found      ; if empty, place the piece
    
    loop place_loop     ; decrement rcx and repeat

    ret

place_found:
    ; Place 'X' or 'O' based on current player
    mov al, [current_player]
    cmp al, 1
    je place_X
    cmp al, 2
    je place_O

place_X:
    mov byte [rdx], 'X'
    ret

place_O:
    mov byte [rdx], 'O'
    ret

check_vertical_win:
    ; Check each column for a vertical win
    mov rbx, 0          ; start with the first column (0)
check_column:
    mov rcx, 0          ; start with the first row (0)
check_rows:
    mov rdx, rcx
    imul rdx, 8         ; multiply by 8 (7 chars + newline)
    add rdx, rbx        ; add the column index
    add rdx, board      ; add the base address of the board

    mov al, [rdx]
    cmp al, '.'         ; if cell is empty, skip
    je next_row

    ; Check for 4 in a row vertically
    mov rsi, rdx        ; store the starting position
    mov rdi, 1          ; initialize the counter
vertical_check:
    add rsi, 8          ; move to the cell in the next row
    cmp byte [rsi], al
    jne end_vertical_check ; if not the same, end check
    inc rdi             ; increment the counter
    cmp rdi, 4          ; if counter reaches 4, win
    je announce_winner
    jmp vertical_check

next_row:
    inc rcx
    cmp rcx, 3          ; only need to check up to row 3
    jl check_rows

    inc rbx
    cmp rbx, 7          ; check all 7 columns
    jl check_column

    ret

end_vertical_check:
    ret

check_horizontal_win:
    ; Check each row for a horizontal win
    mov rcx, 0          ; start with the first row (0)
check_row:
    mov rbx, 0          ; start with the first column (0)
check_columns:
    mov rdx, rcx
    imul rdx, 8         ; multiply by 8 (7 chars + newline)
    add rdx, rbx        ; add the column index
    add rdx, board      ; add the base address of the board

    mov al, [rdx]
    cmp al, '.'         ; if cell is empty, skip
    je next_column

    ; Check for 4 in a row horizontally
    mov rsi, rdx        ; store the starting position
    mov rdi, 1          ; initialize the counter
horizontal_check:
    inc rsi             ; move to the next cell in the row
    cmp byte [rsi], al
    jne end_horizontal_check ; if not the same, end check
    inc rdi             ; increment the counter
    cmp rdi, 4          ; if counter reaches 4, win
    je announce_winner
    jmp horizontal_check

next_column:
    inc rbx
    cmp rbx, 4          ; only need to check up to column 4
    jl check_columns

    inc rcx
    cmp rcx, 6          ; check all 6 rows
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
    mov rax, 1          ; sys_write
    mov rdi, 1          ; file descriptor (stdout)
    mov rsi, win1       ; pointer to Player 1's win message
    mov rdx, 14         ; number of bytes to write
    syscall
    jmp game_end

player2_wins:
    mov rax, 1          ; sys_write
    mov rdi, 1          ; file descriptor (stdout)
    mov rsi, win2       ; pointer to Player 2's win message
    mov rdx, 14         ; number of bytes to write
    syscall
    jmp game_end

game_end:
    mov rax, 60         ; sys_exit
    xor rdi, rdi        ; status 0
    syscall
