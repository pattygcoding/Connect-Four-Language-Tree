section .data
    board db ' 1   2   3   4   5   6   7 ', 10, \
              '|   |   |   |   |   |   |   |', 10, \
              '|   |   |   |   |   |   |   |', 10, \
              '|   |   |   |   |   |   |   |', 10, \
              '|   |   |   |   |   |   |   |', 10, \
              '|   |   |   |   |   |   |   |', 10, \
              '|   |   |   |   |   |   |   |', 10, \
              '+---+---+---+---+---+---+---+', 10
    len equ $ - board

section .bss
    col resb 1
    board_state resb 7 * 6  ; 7 columns, 6 rows

section .text
    global _start

_start:
    call print_board
    call game_loop

game_loop:
    ; Player 1 turn
    mov rdi, 'x'
    call player_turn

    ; Player 2 turn
    mov rdi, 'o'
    call player_turn

    jmp game_loop

player_turn:
    ; Print prompt
    mov rax, 1
    mov rdi, 1
    mov rsi, prompt
    mov rdx, prompt_len
    syscall

    ; Read input
    mov rax, 0
    mov rdi, 0
    mov rsi, col
    mov rdx, 1
    syscall

    ; Convert input to column index (0-6)
    sub byte [col], '1'
    
    ; Check bounds
    cmp byte [col], 6
    ja invalid_input

    ; Drop piece
    movzx rsi, byte [col]
    call drop_piece

    ret

invalid_input:
    ; Invalid input handling
    ret

drop_piece:
    ; rdi contains the player piece ('x' or 'o')
    ; rsi contains the column index
    mov rcx, 5  ; Start from the bottom row
.drop_loop:
    mov rbx, 7
    mul rbx
    add rbx, rsi
    add rbx, rcx
    mov al, byte [board_state + rbx]
    cmp al, 0
    je .place_piece
    dec rcx
    jns .drop_loop
    ret

.place_piece:
    ; Place the piece
    mov byte [board_state + rbx], dil

    call update_board

    ret

update_board:
    ; Update the visual board based on board_state
    mov rdi, board
    mov rbx, 6 * 7

.update_loop:
    cmp rbx, 0
    je .print_board

    dec rbx
    mov al, byte [board_state + rbx]
    cmp al, 0
    je .next

    ; Calculate the position in the visual board
    mov rcx, rbx
    mov rdx, 4
    mul rdx
    add rdi, 37
    add rdi, rcx

    ; Update the visual board
    mov [rdi], al

.next:
    jmp .update_loop

.print_board:
    call print_board
    ret

print_board:
    ; Print the board
    mov rax, 1
    mov rdi, 1
    mov rsi, board
    mov rdx, len
    syscall
    ret

section .data
prompt db 'Enter column (1-7): ', 0
prompt_len equ $ - prompt
