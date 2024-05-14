section .data
    board db '1234567', 10, '.......', 10, '.......', 10, '.......', 10, '.......', 10, '.......', 10, '.......', 10, 0
    prompt db 'Player ', 0
    player1 db '1', 0
    player2 db '2', 0
    newline db 10, 0
    column_full db 'Column full, try another column.', 10, 0
    invalid_input db 'Invalid input, try again.', 10, 0
    victory db 'Player ', 0
    win_message db ' wins!', 10, 0

section .bss
    input resb 2
    turn resb 1
    current_player resb 1

section .text
    global _start

_start:
    ; Print initial board
    mov rsi, board
    call print_string

game_loop:
    ; Set current player
    movzx rax, byte [turn]
    test rax, rax
    jz set_player1
    mov rsi, player2
    jmp set_prompt

set_player1:
    mov rsi, player1

set_prompt:
    ; Print prompt
    mov rdx, rsi
    mov rsi, prompt
    call print_string
    mov rsi, rdx
    call print_string
    call get_input

    ; Check input
    sub byte [input], '1'
    jb invalid_move
    cmp byte [input], 6
    ja invalid_move

    ; Update board
    movzx rdi, byte [input]
    call place_piece
    test rax, rax
    jz game_loop

    ; Print updated board
    mov rsi, board
    call print_string

    ; Check for victory
    call check_victory
    test rax, rax
    jz switch_turn

    ; Print victory message
    mov rsi, victory
    call print_string
    mov rsi, current_player
    call print_string
    mov rsi, win_message
    call print_string
    jmp exit

switch_turn:
    ; Switch player turn
    movzx rax, byte [turn]
    xor rax, 1
    mov [turn], al
    jmp game_loop

invalid_move:
    mov rsi, invalid_input
    call print_string
    jmp game_loop

place_piece:
    ; Place piece in the column
    imul rdi, rdi, 8
    add rdi, 8

find_row:
    cmp byte [board + rdi], '.'
    je place_here
    sub rdi, 8
    cmp rdi, 7
    ja find_row
    mov rax, 0
    ret

place_here:
    test byte [turn], 1
    jz place_player1
    mov byte [board + rdi], 'O'
    jmp place_done

place_player1:
    mov byte [board + rdi], 'X'

place_done:
    mov al, byte [rsi]
    mov [current_player], al
    mov rax, 1
    ret

check_victory:
    ; Placeholder for victory checking logic
    mov rax, 0
    ret

print_string:
    ; Print string at rsi
    mov rdx, rsi
.print_loop:
    lodsb
    test al, al
    jz .done
    mov rdi, 1
    mov rax, 1
    syscall
    jmp .print_loop
.done:
    ret

get_input:
    ; Read user input
    mov rdi, 0
    mov rax, 0
    lea rsi, [input]
    mov rdx, 2
    syscall
    ret

exit:
    ; Exit the program
    mov rax, 60
    xor rdi, rdi
    syscall
