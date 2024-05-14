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

section .text
    global _start

_start:
    ; sys_write (syscall number 1)
    mov rax, 1
    mov rdi, 1
    mov rsi, board
    mov rdx, len
    syscall

    ; sys_exit (syscall number 60)
    mov rax, 60
    xor rdi, rdi
    syscall
