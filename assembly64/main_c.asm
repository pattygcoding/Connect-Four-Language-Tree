	.file	"main.c"
	.intel_syntax noprefix
	.text
	.globl	initialize_board
	.type	initialize_board, @function
initialize_board:
.LFB0:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -24[rbp], rdi
	mov	DWORD PTR -8[rbp], 0
	jmp	.L2
.L5:
	mov	DWORD PTR -4[rbp], 0
	jmp	.L3
.L4:
	mov	eax, DWORD PTR -8[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR -24[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -4[rbp]
	cdqe
	mov	BYTE PTR [rdx+rax], 32
	add	DWORD PTR -4[rbp], 1
.L3:
	cmp	DWORD PTR -4[rbp], 6
	jle	.L4
	add	DWORD PTR -8[rbp], 1
.L2:
	cmp	DWORD PTR -8[rbp], 5
	jle	.L5
	nop
	nop
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	initialize_board, .-initialize_board
	.section	.rodata
.LC0:
	.string	"| 1 | 2 | 3 | 4 | 5 | 6 | 7 |"
.LC1:
	.string	"| "
.LC2:
	.string	"%c | "
	.text
	.globl	display_board
	.type	display_board, @function
display_board:
.LFB1:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -24[rbp], rdi
	mov	edi, 10
	call	putchar@PLT
	lea	rax, .LC0[rip]
	mov	rdi, rax
	call	puts@PLT
	mov	DWORD PTR -8[rbp], 0
	jmp	.L7
.L10:
	lea	rax, .LC1[rip]
	mov	rdi, rax
	mov	eax, 0
	call	printf@PLT
	mov	DWORD PTR -4[rbp], 0
	jmp	.L8
.L9:
	mov	eax, DWORD PTR -8[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR -24[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -4[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	movsx	eax, al
	mov	esi, eax
	lea	rax, .LC2[rip]
	mov	rdi, rax
	mov	eax, 0
	call	printf@PLT
	add	DWORD PTR -4[rbp], 1
.L8:
	cmp	DWORD PTR -4[rbp], 6
	jle	.L9
	mov	edi, 10
	call	putchar@PLT
	add	DWORD PTR -8[rbp], 1
.L7:
	cmp	DWORD PTR -8[rbp], 5
	jle	.L10
	nop
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	display_board, .-display_board
	.globl	is_column_full
	.type	is_column_full, @function
is_column_full:
.LFB2:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	DWORD PTR -12[rbp], esi
	mov	rdx, QWORD PTR -8[rbp]
	mov	eax, DWORD PTR -12[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	al, 32
	setne	al
	movzx	eax, al
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2:
	.size	is_column_full, .-is_column_full
	.globl	drop_piece
	.type	drop_piece, @function
drop_piece:
.LFB3:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -24[rbp], rdi
	mov	DWORD PTR -28[rbp], esi
	mov	eax, edx
	mov	BYTE PTR -32[rbp], al
	mov	DWORD PTR -4[rbp], 5
	jmp	.L14
.L17:
	mov	eax, DWORD PTR -4[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR -24[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -28[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	al, 32
	jne	.L15
	mov	eax, DWORD PTR -4[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR -24[rbp]
	add	rcx, rax
	mov	eax, DWORD PTR -28[rbp]
	cdqe
	movzx	edx, BYTE PTR -32[rbp]
	mov	BYTE PTR [rcx+rax], dl
	jmp	.L16
.L15:
	sub	DWORD PTR -4[rbp], 1
.L14:
	cmp	DWORD PTR -4[rbp], 0
	jns	.L17
	nop
.L16:
	nop
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE3:
	.size	drop_piece, .-drop_piece
	.globl	check_win
	.type	check_win, @function
check_win:
.LFB4:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -40[rbp], rdi
	mov	eax, esi
	mov	BYTE PTR -44[rbp], al
	mov	DWORD PTR -32[rbp], 0
	jmp	.L19
.L24:
	mov	DWORD PTR -28[rbp], 0
	jmp	.L20
.L23:
	mov	eax, DWORD PTR -32[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR -40[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -28[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR -44[rbp], al
	jne	.L21
	mov	eax, DWORD PTR -32[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR -40[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -28[rbp]
	add	eax, 1
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR -44[rbp], al
	jne	.L21
	mov	eax, DWORD PTR -32[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR -40[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -28[rbp]
	add	eax, 2
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR -44[rbp], al
	jne	.L21
	mov	eax, DWORD PTR -32[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR -40[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -28[rbp]
	add	eax, 3
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR -44[rbp], al
	jne	.L21
	mov	eax, 1
	jmp	.L22
.L21:
	add	DWORD PTR -28[rbp], 1
.L20:
	cmp	DWORD PTR -28[rbp], 3
	jle	.L23
	add	DWORD PTR -32[rbp], 1
.L19:
	cmp	DWORD PTR -32[rbp], 5
	jle	.L24
	mov	DWORD PTR -24[rbp], 0
	jmp	.L25
.L29:
	mov	DWORD PTR -20[rbp], 0
	jmp	.L26
.L28:
	mov	eax, DWORD PTR -20[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR -40[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -24[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR -44[rbp], al
	jne	.L27
	mov	eax, DWORD PTR -20[rbp]
	cdqe
	lea	rdx, 1[rax]
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR -40[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -24[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR -44[rbp], al
	jne	.L27
	mov	eax, DWORD PTR -20[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	lea	rdx, 14[rax]
	mov	rax, QWORD PTR -40[rbp]
	add	rdx, rax
	mov	eax, DWORD PTR -24[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR -44[rbp], al
	jne	.L27
	mov	eax, DWORD PTR -20[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	lea	rdx, 21[rax]
	mov	rax, QWORD PTR -40[rbp]
	add	rdx, rax
	mov	eax, DWORD PTR -24[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR -44[rbp], al
	jne	.L27
	mov	eax, 1
	jmp	.L22
.L27:
	add	DWORD PTR -20[rbp], 1
.L26:
	cmp	DWORD PTR -20[rbp], 2
	jle	.L28
	add	DWORD PTR -24[rbp], 1
.L25:
	cmp	DWORD PTR -24[rbp], 6
	jle	.L29
	mov	DWORD PTR -16[rbp], 0
	jmp	.L30
.L34:
	mov	DWORD PTR -12[rbp], 0
	jmp	.L31
.L33:
	mov	eax, DWORD PTR -16[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR -40[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -12[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR -44[rbp], al
	jne	.L32
	mov	eax, DWORD PTR -16[rbp]
	cdqe
	lea	rdx, 1[rax]
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR -40[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -12[rbp]
	add	eax, 1
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR -44[rbp], al
	jne	.L32
	mov	eax, DWORD PTR -16[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	lea	rdx, 14[rax]
	mov	rax, QWORD PTR -40[rbp]
	add	rdx, rax
	mov	eax, DWORD PTR -12[rbp]
	add	eax, 2
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR -44[rbp], al
	jne	.L32
	mov	eax, DWORD PTR -16[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	lea	rdx, 21[rax]
	mov	rax, QWORD PTR -40[rbp]
	add	rdx, rax
	mov	eax, DWORD PTR -12[rbp]
	add	eax, 3
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR -44[rbp], al
	jne	.L32
	mov	eax, 1
	jmp	.L22
.L32:
	add	DWORD PTR -12[rbp], 1
.L31:
	cmp	DWORD PTR -12[rbp], 3
	jle	.L33
	add	DWORD PTR -16[rbp], 1
.L30:
	cmp	DWORD PTR -16[rbp], 2
	jle	.L34
	mov	DWORD PTR -8[rbp], 3
	jmp	.L35
.L39:
	mov	DWORD PTR -4[rbp], 0
	jmp	.L36
.L38:
	mov	eax, DWORD PTR -8[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR -40[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -4[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR -44[rbp], al
	jne	.L37
	mov	eax, DWORD PTR -8[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	lea	rdx, -7[rax]
	mov	rax, QWORD PTR -40[rbp]
	add	rdx, rax
	mov	eax, DWORD PTR -4[rbp]
	add	eax, 1
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR -44[rbp], al
	jne	.L37
	mov	eax, DWORD PTR -8[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	lea	rdx, -14[rax]
	mov	rax, QWORD PTR -40[rbp]
	add	rdx, rax
	mov	eax, DWORD PTR -4[rbp]
	add	eax, 2
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR -44[rbp], al
	jne	.L37
	mov	eax, DWORD PTR -8[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	lea	rdx, -21[rax]
	mov	rax, QWORD PTR -40[rbp]
	add	rdx, rax
	mov	eax, DWORD PTR -4[rbp]
	add	eax, 3
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR -44[rbp], al
	jne	.L37
	mov	eax, 1
	jmp	.L22
.L37:
	add	DWORD PTR -4[rbp], 1
.L36:
	cmp	DWORD PTR -4[rbp], 3
	jle	.L38
	add	DWORD PTR -8[rbp], 1
.L35:
	cmp	DWORD PTR -8[rbp], 5
	jle	.L39
	mov	eax, 0
.L22:
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE4:
	.size	check_win, .-check_win
	.globl	is_board_full
	.type	is_board_full, @function
is_board_full:
.LFB5:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -24[rbp], rdi
	mov	DWORD PTR -8[rbp], 0
	jmp	.L41
.L46:
	mov	DWORD PTR -4[rbp], 0
	jmp	.L42
.L45:
	mov	eax, DWORD PTR -8[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR -24[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -4[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	al, 32
	jne	.L43
	mov	eax, 0
	jmp	.L44
.L43:
	add	DWORD PTR -4[rbp], 1
.L42:
	cmp	DWORD PTR -4[rbp], 6
	jle	.L45
	add	DWORD PTR -8[rbp], 1
.L41:
	cmp	DWORD PTR -8[rbp], 5
	jle	.L46
	mov	eax, 1
.L44:
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE5:
	.size	is_board_full, .-is_board_full
	.section	.rodata
	.align 8
.LC3:
	.string	"Player %d, enter column (1-7): "
.LC4:
	.string	"%d"
.LC5:
	.string	"Invalid move. Try again."
.LC6:
	.string	"Player %d wins!\n"
.LC7:
	.string	"It's a draw!"
	.text
	.globl	main
	.type	main, @function
main:
.LFB6:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 80
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR -8[rbp], rax
	xor	eax, eax
	mov	DWORD PTR -68[rbp], 1
	lea	rax, -64[rbp]
	mov	rdi, rax
	call	initialize_board
.L58:
	lea	rax, -64[rbp]
	mov	rdi, rax
	call	display_board
	mov	eax, DWORD PTR -68[rbp]
	mov	esi, eax
	lea	rax, .LC3[rip]
	mov	rdi, rax
	mov	eax, 0
	call	printf@PLT
	lea	rax, -72[rbp]
	mov	rsi, rax
	lea	rax, .LC4[rip]
	mov	rdi, rax
	mov	eax, 0
	call	__isoc99_scanf@PLT
	mov	eax, DWORD PTR -72[rbp]
	sub	eax, 1
	mov	DWORD PTR -72[rbp], eax
	mov	eax, DWORD PTR -72[rbp]
	test	eax, eax
	js	.L48
	mov	eax, DWORD PTR -72[rbp]
	cmp	eax, 6
	jg	.L48
	mov	edx, DWORD PTR -72[rbp]
	lea	rax, -64[rbp]
	mov	esi, edx
	mov	rdi, rax
	call	is_column_full
	test	eax, eax
	je	.L49
.L48:
	lea	rax, .LC5[rip]
	mov	rdi, rax
	call	puts@PLT
	jmp	.L50
.L49:
	cmp	DWORD PTR -68[rbp], 1
	jne	.L51
	mov	eax, 88
	jmp	.L52
.L51:
	mov	eax, 79
.L52:
	mov	BYTE PTR -73[rbp], al
	movsx	edx, BYTE PTR -73[rbp]
	mov	ecx, DWORD PTR -72[rbp]
	lea	rax, -64[rbp]
	mov	esi, ecx
	mov	rdi, rax
	call	drop_piece
	movsx	edx, BYTE PTR -73[rbp]
	lea	rax, -64[rbp]
	mov	esi, edx
	mov	rdi, rax
	call	check_win
	test	eax, eax
	je	.L53
	lea	rax, -64[rbp]
	mov	rdi, rax
	call	display_board
	mov	eax, DWORD PTR -68[rbp]
	mov	esi, eax
	lea	rax, .LC6[rip]
	mov	rdi, rax
	mov	eax, 0
	call	printf@PLT
	jmp	.L54
.L53:
	lea	rax, -64[rbp]
	mov	rdi, rax
	call	is_board_full
	test	al, al
	je	.L55
	lea	rax, -64[rbp]
	mov	rdi, rax
	call	display_board
	lea	rax, .LC7[rip]
	mov	rdi, rax
	call	puts@PLT
	jmp	.L54
.L55:
	cmp	DWORD PTR -68[rbp], 1
	jne	.L56
	mov	eax, 2
	jmp	.L57
.L56:
	mov	eax, 1
.L57:
	mov	DWORD PTR -68[rbp], eax
.L50:
	jmp	.L58
.L54:
	mov	eax, 0
	mov	rdx, QWORD PTR -8[rbp]
	sub	rdx, QWORD PTR fs:40
	je	.L60
	call	__stack_chk_fail@PLT
.L60:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE6:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0"
	.section	.note.GNU-stack,"",@progbits
	.section	.note.gnu.property,"a"
	.align 8
	.long	1f - 0f
	.long	4f - 1f
	.long	5
0:
	.string	"GNU"
1:
	.align 8
	.long	0xc0000002
	.long	3f - 2f
2:
	.long	0x3
3:
	.align 8
4:
