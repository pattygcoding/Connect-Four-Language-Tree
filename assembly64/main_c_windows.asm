	.file	"main.c"
	.intel_syntax noprefix
	.text
	.globl	initialize_board
	.def	initialize_board;	.scl	2;	.type	32;	.endef
	.seh_proc	initialize_board
initialize_board:
	push	rbp
	.seh_pushreg	rbp
	mov	rbp, rsp
	.seh_setframe	rbp, 0
	sub	rsp, 16
	.seh_stackalloc	16
	.seh_endprologue
	mov	QWORD PTR 16[rbp], rcx
	mov	DWORD PTR -4[rbp], 0
	jmp	.L2
.L5:
	mov	DWORD PTR -8[rbp], 0
	jmp	.L3
.L4:
	mov	eax, DWORD PTR -4[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR 16[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -8[rbp]
	cdqe
	mov	BYTE PTR [rdx+rax], 32
	add	DWORD PTR -8[rbp], 1
.L3:
	cmp	DWORD PTR -8[rbp], 6
	jle	.L4
	add	DWORD PTR -4[rbp], 1
.L2:
	cmp	DWORD PTR -4[rbp], 5
	jle	.L5
	nop
	nop
	add	rsp, 16
	pop	rbp
	ret
	.seh_endproc
	.section .rdata,"dr"
.LC0:
	.ascii "| 1 | 2 | 3 | 4 | 5 | 6 | 7 |\0"
.LC1:
	.ascii "| \0"
.LC2:
	.ascii "%c | \0"
	.text
	.globl	display_board
	.def	display_board;	.scl	2;	.type	32;	.endef
	.seh_proc	display_board
display_board:
	push	rbp
	.seh_pushreg	rbp
	mov	rbp, rsp
	.seh_setframe	rbp, 0
	sub	rsp, 48
	.seh_stackalloc	48
	.seh_endprologue
	mov	QWORD PTR 16[rbp], rcx
	mov	ecx, 10
	call	putchar
	lea	rax, .LC0[rip]
	mov	rcx, rax
	call	puts
	mov	DWORD PTR -4[rbp], 0
	jmp	.L7
.L10:
	lea	rax, .LC1[rip]
	mov	rcx, rax
	call	printf
	mov	DWORD PTR -8[rbp], 0
	jmp	.L8
.L9:
	mov	eax, DWORD PTR -4[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR 16[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -8[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	movsx	eax, al
	mov	edx, eax
	lea	rax, .LC2[rip]
	mov	rcx, rax
	call	printf
	add	DWORD PTR -8[rbp], 1
.L8:
	cmp	DWORD PTR -8[rbp], 6
	jle	.L9
	mov	ecx, 10
	call	putchar
	add	DWORD PTR -4[rbp], 1
.L7:
	cmp	DWORD PTR -4[rbp], 5
	jle	.L10
	nop
	nop
	add	rsp, 48
	pop	rbp
	ret
	.seh_endproc
	.globl	is_column_full
	.def	is_column_full;	.scl	2;	.type	32;	.endef
	.seh_proc	is_column_full
is_column_full:
	push	rbp
	.seh_pushreg	rbp
	mov	rbp, rsp
	.seh_setframe	rbp, 0
	.seh_endprologue
	mov	QWORD PTR 16[rbp], rcx
	mov	DWORD PTR 24[rbp], edx
	mov	rdx, QWORD PTR 16[rbp]
	mov	eax, DWORD PTR 24[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	al, 32
	setne	al
	movzx	eax, al
	pop	rbp
	ret
	.seh_endproc
	.globl	drop_piece
	.def	drop_piece;	.scl	2;	.type	32;	.endef
	.seh_proc	drop_piece
drop_piece:
	push	rbp
	.seh_pushreg	rbp
	mov	rbp, rsp
	.seh_setframe	rbp, 0
	sub	rsp, 16
	.seh_stackalloc	16
	.seh_endprologue
	mov	QWORD PTR 16[rbp], rcx
	mov	DWORD PTR 24[rbp], edx
	mov	eax, r8d
	mov	BYTE PTR 32[rbp], al
	mov	DWORD PTR -4[rbp], 5
	jmp	.L14
.L17:
	mov	eax, DWORD PTR -4[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR 16[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR 24[rbp]
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
	mov	rax, QWORD PTR 16[rbp]
	add	rcx, rax
	mov	eax, DWORD PTR 24[rbp]
	cdqe
	movzx	edx, BYTE PTR 32[rbp]
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
	add	rsp, 16
	pop	rbp
	ret
	.seh_endproc
	.globl	check_win
	.def	check_win;	.scl	2;	.type	32;	.endef
	.seh_proc	check_win
check_win:
	push	rbp
	.seh_pushreg	rbp
	mov	rbp, rsp
	.seh_setframe	rbp, 0
	sub	rsp, 32
	.seh_stackalloc	32
	.seh_endprologue
	mov	QWORD PTR 16[rbp], rcx
	mov	eax, edx
	mov	BYTE PTR 24[rbp], al
	mov	DWORD PTR -4[rbp], 0
	jmp	.L19
.L24:
	mov	DWORD PTR -8[rbp], 0
	jmp	.L20
.L23:
	mov	eax, DWORD PTR -4[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR 16[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -8[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR 24[rbp], al
	jne	.L21
	mov	eax, DWORD PTR -4[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR 16[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -8[rbp]
	add	eax, 1
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR 24[rbp], al
	jne	.L21
	mov	eax, DWORD PTR -4[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR 16[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -8[rbp]
	add	eax, 2
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR 24[rbp], al
	jne	.L21
	mov	eax, DWORD PTR -4[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR 16[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -8[rbp]
	add	eax, 3
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR 24[rbp], al
	jne	.L21
	mov	eax, 1
	jmp	.L22
.L21:
	add	DWORD PTR -8[rbp], 1
.L20:
	cmp	DWORD PTR -8[rbp], 3
	jle	.L23
	add	DWORD PTR -4[rbp], 1
.L19:
	cmp	DWORD PTR -4[rbp], 5
	jle	.L24
	mov	DWORD PTR -12[rbp], 0
	jmp	.L25
.L29:
	mov	DWORD PTR -16[rbp], 0
	jmp	.L26
.L28:
	mov	eax, DWORD PTR -16[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR 16[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -12[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR 24[rbp], al
	jne	.L27
	mov	eax, DWORD PTR -16[rbp]
	cdqe
	lea	rdx, 1[rax]
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR 16[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -12[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR 24[rbp], al
	jne	.L27
	mov	eax, DWORD PTR -16[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	lea	rdx, 14[rax]
	mov	rax, QWORD PTR 16[rbp]
	add	rdx, rax
	mov	eax, DWORD PTR -12[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR 24[rbp], al
	jne	.L27
	mov	eax, DWORD PTR -16[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	lea	rdx, 21[rax]
	mov	rax, QWORD PTR 16[rbp]
	add	rdx, rax
	mov	eax, DWORD PTR -12[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR 24[rbp], al
	jne	.L27
	mov	eax, 1
	jmp	.L22
.L27:
	add	DWORD PTR -16[rbp], 1
.L26:
	cmp	DWORD PTR -16[rbp], 2
	jle	.L28
	add	DWORD PTR -12[rbp], 1
.L25:
	cmp	DWORD PTR -12[rbp], 6
	jle	.L29
	mov	DWORD PTR -20[rbp], 0
	jmp	.L30
.L34:
	mov	DWORD PTR -24[rbp], 0
	jmp	.L31
.L33:
	mov	eax, DWORD PTR -20[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR 16[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -24[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR 24[rbp], al
	jne	.L32
	mov	eax, DWORD PTR -20[rbp]
	cdqe
	lea	rdx, 1[rax]
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR 16[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -24[rbp]
	add	eax, 1
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR 24[rbp], al
	jne	.L32
	mov	eax, DWORD PTR -20[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	lea	rdx, 14[rax]
	mov	rax, QWORD PTR 16[rbp]
	add	rdx, rax
	mov	eax, DWORD PTR -24[rbp]
	add	eax, 2
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR 24[rbp], al
	jne	.L32
	mov	eax, DWORD PTR -20[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	lea	rdx, 21[rax]
	mov	rax, QWORD PTR 16[rbp]
	add	rdx, rax
	mov	eax, DWORD PTR -24[rbp]
	add	eax, 3
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR 24[rbp], al
	jne	.L32
	mov	eax, 1
	jmp	.L22
.L32:
	add	DWORD PTR -24[rbp], 1
.L31:
	cmp	DWORD PTR -24[rbp], 3
	jle	.L33
	add	DWORD PTR -20[rbp], 1
.L30:
	cmp	DWORD PTR -20[rbp], 2
	jle	.L34
	mov	DWORD PTR -28[rbp], 3
	jmp	.L35
.L39:
	mov	DWORD PTR -32[rbp], 0
	jmp	.L36
.L38:
	mov	eax, DWORD PTR -28[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR 16[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -32[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR 24[rbp], al
	jne	.L37
	mov	eax, DWORD PTR -28[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	lea	rdx, -7[rax]
	mov	rax, QWORD PTR 16[rbp]
	add	rdx, rax
	mov	eax, DWORD PTR -32[rbp]
	add	eax, 1
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR 24[rbp], al
	jne	.L37
	mov	eax, DWORD PTR -28[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	lea	rdx, -14[rax]
	mov	rax, QWORD PTR 16[rbp]
	add	rdx, rax
	mov	eax, DWORD PTR -32[rbp]
	add	eax, 2
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR 24[rbp], al
	jne	.L37
	mov	eax, DWORD PTR -28[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	lea	rdx, -21[rax]
	mov	rax, QWORD PTR 16[rbp]
	add	rdx, rax
	mov	eax, DWORD PTR -32[rbp]
	add	eax, 3
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	BYTE PTR 24[rbp], al
	jne	.L37
	mov	eax, 1
	jmp	.L22
.L37:
	add	DWORD PTR -32[rbp], 1
.L36:
	cmp	DWORD PTR -32[rbp], 3
	jle	.L38
	add	DWORD PTR -28[rbp], 1
.L35:
	cmp	DWORD PTR -28[rbp], 5
	jle	.L39
	mov	eax, 0
.L22:
	add	rsp, 32
	pop	rbp
	ret
	.seh_endproc
	.globl	is_board_full
	.def	is_board_full;	.scl	2;	.type	32;	.endef
	.seh_proc	is_board_full
is_board_full:
	push	rbp
	.seh_pushreg	rbp
	mov	rbp, rsp
	.seh_setframe	rbp, 0
	sub	rsp, 16
	.seh_stackalloc	16
	.seh_endprologue
	mov	QWORD PTR 16[rbp], rcx
	mov	DWORD PTR -4[rbp], 0
	jmp	.L41
.L46:
	mov	DWORD PTR -8[rbp], 0
	jmp	.L42
.L45:
	mov	eax, DWORD PTR -4[rbp]
	movsx	rdx, eax
	mov	rax, rdx
	sal	rax, 3
	sub	rax, rdx
	mov	rcx, rax
	mov	rax, QWORD PTR 16[rbp]
	lea	rdx, [rcx+rax]
	mov	eax, DWORD PTR -8[rbp]
	cdqe
	movzx	eax, BYTE PTR [rdx+rax]
	cmp	al, 32
	jne	.L43
	mov	eax, 0
	jmp	.L44
.L43:
	add	DWORD PTR -8[rbp], 1
.L42:
	cmp	DWORD PTR -8[rbp], 6
	jle	.L45
	add	DWORD PTR -4[rbp], 1
.L41:
	cmp	DWORD PTR -4[rbp], 5
	jle	.L46
	mov	eax, 1
.L44:
	add	rsp, 16
	pop	rbp
	ret
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC3:
	.ascii "Player %d, enter column (1-7): \0"
.LC4:
	.ascii "%d\0"
.LC5:
	.ascii "Invalid move. Try again.\0"
.LC6:
	.ascii "Player %d wins!\12\0"
.LC7:
	.ascii "It's a draw!\0"
	.text
	.globl	main
	.def	main;	.scl	2;	.type	32;	.endef
	.seh_proc	main
main:
	push	rbp
	.seh_pushreg	rbp
	mov	rbp, rsp
	.seh_setframe	rbp, 0
	sub	rsp, 96
	.seh_stackalloc	96
	.seh_endprologue
	call	__main
	mov	DWORD PTR -4[rbp], 1
	lea	rax, -48[rbp]
	mov	rcx, rax
	call	initialize_board
.L58:
	lea	rax, -48[rbp]
	mov	rcx, rax
	call	display_board
	mov	eax, DWORD PTR -4[rbp]
	mov	edx, eax
	lea	rax, .LC3[rip]
	mov	rcx, rax
	call	printf
	lea	rax, -52[rbp]
	mov	rdx, rax
	lea	rax, .LC4[rip]
	mov	rcx, rax
	call	scanf
	mov	eax, DWORD PTR -52[rbp]
	sub	eax, 1
	mov	DWORD PTR -52[rbp], eax
	mov	eax, DWORD PTR -52[rbp]
	test	eax, eax
	js	.L48
	mov	eax, DWORD PTR -52[rbp]
	cmp	eax, 6
	jg	.L48
	mov	edx, DWORD PTR -52[rbp]
	lea	rax, -48[rbp]
	mov	rcx, rax
	call	is_column_full
	test	eax, eax
	je	.L49
.L48:
	lea	rax, .LC5[rip]
	mov	rcx, rax
	call	puts
	jmp	.L50
.L49:
	cmp	DWORD PTR -4[rbp], 1
	jne	.L51
	mov	eax, 88
	jmp	.L52
.L51:
	mov	eax, 79
.L52:
	mov	BYTE PTR -5[rbp], al
	movsx	ecx, BYTE PTR -5[rbp]
	mov	edx, DWORD PTR -52[rbp]
	lea	rax, -48[rbp]
	mov	r8d, ecx
	mov	rcx, rax
	call	drop_piece
	movsx	edx, BYTE PTR -5[rbp]
	lea	rax, -48[rbp]
	mov	rcx, rax
	call	check_win
	test	eax, eax
	je	.L53
	lea	rax, -48[rbp]
	mov	rcx, rax
	call	display_board
	mov	eax, DWORD PTR -4[rbp]
	mov	edx, eax
	lea	rax, .LC6[rip]
	mov	rcx, rax
	call	printf
	jmp	.L54
.L53:
	lea	rax, -48[rbp]
	mov	rcx, rax
	call	is_board_full
	test	al, al
	je	.L55
	lea	rax, -48[rbp]
	mov	rcx, rax
	call	display_board
	lea	rax, .LC7[rip]
	mov	rcx, rax
	call	puts
	jmp	.L54
.L55:
	cmp	DWORD PTR -4[rbp], 1
	jne	.L56
	mov	eax, 2
	jmp	.L57
.L56:
	mov	eax, 1
.L57:
	mov	DWORD PTR -4[rbp], eax
.L50:
	jmp	.L58
.L54:
	mov	eax, 0
	add	rsp, 96
	pop	rbp
	ret
	.seh_endproc
	.def	__main;	.scl	2;	.type	32;	.endef
	.ident	"GCC: (MinGW-W64 x86_64-ucrt-posix-seh, built by Brecht Sanders, r1) 14.1.0"
	.def	putchar;	.scl	2;	.type	32;	.endef
	.def	puts;	.scl	2;	.type	32;	.endef
	.def	printf;	.scl	2;	.type	32;	.endef
	.def	scanf;	.scl	2;	.type	32;	.endef
