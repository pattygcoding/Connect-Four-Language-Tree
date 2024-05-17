	.file	"main.cpp"
	.intel_syntax noprefix
	.text
	.section	.text._ZnwmPv,"axG",@progbits,_ZnwmPv,comdat
	.weak	_ZnwmPv
	.type	_ZnwmPv, @function
_ZnwmPv:
.LFB38:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	rax, QWORD PTR -16[rbp]
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE38:
	.size	_ZnwmPv, .-_ZnwmPv
	.section	.text._ZdlPvS_,"axG",@progbits,_ZdlPvS_,comdat
	.weak	_ZdlPvS_
	.type	_ZdlPvS_, @function
_ZdlPvS_:
.LFB40:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	nop
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE40:
	.size	_ZdlPvS_, .-_ZdlPvS_
	.section	.text._ZSt17__size_to_integerm,"axG",@progbits,_ZSt17__size_to_integerm,comdat
	.weak	_ZSt17__size_to_integerm
	.type	_ZSt17__size_to_integerm, @function
_ZSt17__size_to_integerm:
.LFB371:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE371:
	.size	_ZSt17__size_to_integerm, .-_ZSt17__size_to_integerm
	.local	_ZStL8__ioinit
	.comm	_ZStL8__ioinit,1,1
	.section	.text._ZN8Connect4C2Ev,"axG",@progbits,_ZN8Connect4C5Ev,comdat
	.align 2
	.weak	_ZN8Connect4C2Ev
	.type	_ZN8Connect4C2Ev, @function
_ZN8Connect4C2Ev:
.LFB2087:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA2087
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	push	rbx
	sub	rsp, 72
	.cfi_offset 3, -24
	mov	QWORD PTR -72[rbp], rdi
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR -24[rbp], rax
	xor	eax, eax
	mov	rax, QWORD PTR -72[rbp]
	mov	DWORD PTR [rax], 6
	mov	rax, QWORD PTR -72[rbp]
	mov	DWORD PTR 4[rax], 7
	mov	rax, QWORD PTR -72[rbp]
	lea	rbx, 8[rax]
	lea	rax, -49[rbp]
	mov	rdi, rax
	call	_ZNSaISt6vectorIcSaIcEEEC1Ev
	lea	rax, -50[rbp]
	mov	rdi, rax
	call	_ZNSaIcEC1Ev@PLT
	mov	BYTE PTR -51[rbp], 32
	mov	rax, QWORD PTR -72[rbp]
	mov	eax, DWORD PTR 4[rax]
	movsx	rsi, eax
	lea	rcx, -50[rbp]
	lea	rdx, -51[rbp]
	lea	rax, -48[rbp]
	mov	rdi, rax
.LEHB0:
	call	_ZNSt6vectorIcSaIcEEC1EmRKcRKS0_
.LEHE0:
	mov	rax, QWORD PTR -72[rbp]
	mov	eax, DWORD PTR [rax]
	cdqe
	lea	rcx, -49[rbp]
	lea	rdx, -48[rbp]
	mov	rsi, rax
	mov	rdi, rbx
.LEHB1:
	call	_ZNSt6vectorIS_IcSaIcEESaIS1_EEC1EmRKS1_RKS2_
.LEHE1:
	lea	rax, -48[rbp]
	mov	rdi, rax
	call	_ZNSt6vectorIcSaIcEED1Ev
	lea	rax, -50[rbp]
	mov	rdi, rax
	call	_ZNSaIcED1Ev@PLT
	lea	rax, -49[rbp]
	mov	rdi, rax
	call	_ZNSaISt6vectorIcSaIcEEED1Ev
	mov	rax, QWORD PTR -72[rbp]
	mov	DWORD PTR 32[rax], 1
	nop
	mov	rax, QWORD PTR -24[rbp]
	sub	rax, QWORD PTR fs:40
	je	.L9
	jmp	.L12
.L11:
	endbr64
	mov	rbx, rax
	lea	rax, -48[rbp]
	mov	rdi, rax
	call	_ZNSt6vectorIcSaIcEED1Ev
	jmp	.L8
.L10:
	endbr64
	mov	rbx, rax
.L8:
	lea	rax, -50[rbp]
	mov	rdi, rax
	call	_ZNSaIcED1Ev@PLT
	lea	rax, -49[rbp]
	mov	rdi, rax
	call	_ZNSaISt6vectorIcSaIcEEED1Ev
	mov	rax, rbx
	mov	rdi, rax
.LEHB2:
	call	_Unwind_Resume@PLT
.LEHE2:
.L12:
	call	__stack_chk_fail@PLT
.L9:
	mov	rbx, QWORD PTR -8[rbp]
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2087:
	.globl	__gxx_personality_v0
	.section	.gcc_except_table._ZN8Connect4C2Ev,"aG",@progbits,_ZN8Connect4C5Ev,comdat
.LLSDA2087:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE2087-.LLSDACSB2087
.LLSDACSB2087:
	.uleb128 .LEHB0-.LFB2087
	.uleb128 .LEHE0-.LEHB0
	.uleb128 .L10-.LFB2087
	.uleb128 0
	.uleb128 .LEHB1-.LFB2087
	.uleb128 .LEHE1-.LEHB1
	.uleb128 .L11-.LFB2087
	.uleb128 0
	.uleb128 .LEHB2-.LFB2087
	.uleb128 .LEHE2-.LEHB2
	.uleb128 0
	.uleb128 0
.LLSDACSE2087:
	.section	.text._ZN8Connect4C2Ev,"axG",@progbits,_ZN8Connect4C5Ev,comdat
	.size	_ZN8Connect4C2Ev, .-_ZN8Connect4C2Ev
	.weak	_ZN8Connect4C1Ev
	.set	_ZN8Connect4C1Ev,_ZN8Connect4C2Ev
	.section	.rodata
.LC0:
	.string	"| "
.LC1:
	.string	" | "
.LC2:
	.string	"-----------------------------"
.LC3:
	.string	"| 1 | 2 | 3 | 4 | 5 | 6 | 7 |"
	.section	.text._ZNK8Connect412displayBoardEv,"axG",@progbits,_ZNK8Connect412displayBoardEv,comdat
	.align 2
	.weak	_ZNK8Connect412displayBoardEv
	.type	_ZNK8Connect412displayBoardEv, @function
_ZNK8Connect412displayBoardEv:
.LFB2089:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -24[rbp], rdi
	mov	rax, QWORD PTR _ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_@GOTPCREL[rip]
	mov	rsi, rax
	lea	rax, _ZSt4cout[rip]
	mov	rdi, rax
	call	_ZNSolsEPFRSoS_E@PLT
	mov	DWORD PTR -8[rbp], 0
	jmp	.L14
.L17:
	lea	rax, .LC0[rip]
	mov	rsi, rax
	lea	rax, _ZSt4cout[rip]
	mov	rdi, rax
	call	_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc@PLT
	mov	DWORD PTR -4[rbp], 0
	jmp	.L15
.L16:
	mov	rax, QWORD PTR -24[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -8[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -4[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	movsx	eax, al
	mov	esi, eax
	lea	rax, _ZSt4cout[rip]
	mov	rdi, rax
	call	_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_c@PLT
	mov	rdx, rax
	lea	rax, .LC1[rip]
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc@PLT
	add	DWORD PTR -4[rbp], 1
.L15:
	mov	rax, QWORD PTR -24[rbp]
	mov	eax, DWORD PTR 4[rax]
	cmp	DWORD PTR -4[rbp], eax
	jl	.L16
	mov	rax, QWORD PTR _ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_@GOTPCREL[rip]
	mov	rsi, rax
	lea	rax, _ZSt4cout[rip]
	mov	rdi, rax
	call	_ZNSolsEPFRSoS_E@PLT
	add	DWORD PTR -8[rbp], 1
.L14:
	mov	rax, QWORD PTR -24[rbp]
	mov	eax, DWORD PTR [rax]
	cmp	DWORD PTR -8[rbp], eax
	jl	.L17
	lea	rax, .LC2[rip]
	mov	rsi, rax
	lea	rax, _ZSt4cout[rip]
	mov	rdi, rax
	call	_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc@PLT
	mov	rdx, QWORD PTR _ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_@GOTPCREL[rip]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSolsEPFRSoS_E@PLT
	lea	rax, .LC3[rip]
	mov	rsi, rax
	lea	rax, _ZSt4cout[rip]
	mov	rdi, rax
	call	_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc@PLT
	mov	rdx, QWORD PTR _ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_@GOTPCREL[rip]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSolsEPFRSoS_E@PLT
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2089:
	.size	_ZNK8Connect412displayBoardEv, .-_ZNK8Connect412displayBoardEv
	.section	.rodata
	.align 8
.LC4:
	.string	"Invalid column. Please try again."
	.align 8
.LC5:
	.string	"Column is full. Please choose another column."
	.section	.text._ZN8Connect49dropPieceEi,"axG",@progbits,_ZN8Connect49dropPieceEi,comdat
	.align 2
	.weak	_ZN8Connect49dropPieceEi
	.type	_ZN8Connect49dropPieceEi, @function
_ZN8Connect49dropPieceEi:
.LFB2090:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	push	rbx
	sub	rsp, 40
	.cfi_offset 3, -24
	mov	QWORD PTR -40[rbp], rdi
	mov	DWORD PTR -44[rbp], esi
	cmp	DWORD PTR -44[rbp], 0
	js	.L19
	mov	rax, QWORD PTR -40[rbp]
	mov	eax, DWORD PTR 4[rax]
	cmp	DWORD PTR -44[rbp], eax
	jl	.L20
.L19:
	lea	rax, .LC4[rip]
	mov	rsi, rax
	lea	rax, _ZSt4cout[rip]
	mov	rdi, rax
	call	_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc@PLT
	mov	rdx, QWORD PTR _ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_@GOTPCREL[rip]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSolsEPFRSoS_E@PLT
	mov	eax, 0
	jmp	.L21
.L20:
	mov	rax, QWORD PTR -40[rbp]
	mov	eax, DWORD PTR [rax]
	sub	eax, 1
	mov	DWORD PTR -20[rbp], eax
	jmp	.L22
.L26:
	mov	rax, QWORD PTR -40[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -20[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -44[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	cmp	al, 32
	sete	al
	test	al, al
	je	.L23
	mov	rax, QWORD PTR -40[rbp]
	mov	eax, DWORD PTR 32[rax]
	cmp	eax, 1
	jne	.L24
	mov	ebx, 88
	jmp	.L25
.L24:
	mov	ebx, 79
.L25:
	mov	rax, QWORD PTR -40[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -20[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -44[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNSt6vectorIcSaIcEEixEm
	mov	BYTE PTR [rax], bl
	mov	eax, 1
	jmp	.L21
.L23:
	sub	DWORD PTR -20[rbp], 1
.L22:
	cmp	DWORD PTR -20[rbp], 0
	jns	.L26
	lea	rax, .LC5[rip]
	mov	rsi, rax
	lea	rax, _ZSt4cout[rip]
	mov	rdi, rax
	call	_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc@PLT
	mov	rdx, QWORD PTR _ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_@GOTPCREL[rip]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSolsEPFRSoS_E@PLT
	mov	eax, 0
.L21:
	mov	rbx, QWORD PTR -8[rbp]
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2090:
	.size	_ZN8Connect49dropPieceEi, .-_ZN8Connect49dropPieceEi
	.section	.text._ZNK8Connect48checkWinEv,"axG",@progbits,_ZNK8Connect48checkWinEv,comdat
	.align 2
	.weak	_ZNK8Connect48checkWinEv
	.type	_ZNK8Connect48checkWinEv, @function
_ZNK8Connect48checkWinEv:
.LFB2091:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 64
	mov	QWORD PTR -56[rbp], rdi
	mov	rax, QWORD PTR -56[rbp]
	mov	eax, DWORD PTR 32[rax]
	cmp	eax, 1
	jne	.L28
	mov	eax, 88
	jmp	.L29
.L28:
	mov	eax, 79
.L29:
	mov	BYTE PTR -33[rbp], al
	mov	DWORD PTR -32[rbp], 0
	jmp	.L30
.L37:
	mov	DWORD PTR -28[rbp], 0
	jmp	.L31
.L36:
	mov	rax, QWORD PTR -56[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -32[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -28[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	cmp	BYTE PTR -33[rbp], al
	jne	.L32
	mov	rax, QWORD PTR -56[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -32[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -28[rbp]
	add	eax, 1
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	cmp	BYTE PTR -33[rbp], al
	jne	.L32
	mov	rax, QWORD PTR -56[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -32[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -28[rbp]
	add	eax, 2
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	cmp	BYTE PTR -33[rbp], al
	jne	.L32
	mov	rax, QWORD PTR -56[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -32[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -28[rbp]
	add	eax, 3
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	cmp	BYTE PTR -33[rbp], al
	jne	.L32
	mov	eax, 1
	jmp	.L33
.L32:
	mov	eax, 0
.L33:
	test	al, al
	je	.L34
	mov	eax, 1
	jmp	.L35
.L34:
	add	DWORD PTR -28[rbp], 1
.L31:
	mov	rax, QWORD PTR -56[rbp]
	mov	eax, DWORD PTR 4[rax]
	sub	eax, 3
	cmp	DWORD PTR -28[rbp], eax
	jl	.L36
	add	DWORD PTR -32[rbp], 1
.L30:
	mov	rax, QWORD PTR -56[rbp]
	mov	eax, DWORD PTR [rax]
	cmp	DWORD PTR -32[rbp], eax
	jl	.L37
	mov	DWORD PTR -24[rbp], 0
	jmp	.L38
.L44:
	mov	DWORD PTR -20[rbp], 0
	jmp	.L39
.L43:
	mov	rax, QWORD PTR -56[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -20[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -24[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	cmp	BYTE PTR -33[rbp], al
	jne	.L40
	mov	rax, QWORD PTR -56[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -20[rbp]
	add	eax, 1
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -24[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	cmp	BYTE PTR -33[rbp], al
	jne	.L40
	mov	rax, QWORD PTR -56[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -20[rbp]
	add	eax, 2
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -24[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	cmp	BYTE PTR -33[rbp], al
	jne	.L40
	mov	rax, QWORD PTR -56[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -20[rbp]
	add	eax, 3
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -24[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	cmp	BYTE PTR -33[rbp], al
	jne	.L40
	mov	eax, 1
	jmp	.L41
.L40:
	mov	eax, 0
.L41:
	test	al, al
	je	.L42
	mov	eax, 1
	jmp	.L35
.L42:
	add	DWORD PTR -20[rbp], 1
.L39:
	mov	rax, QWORD PTR -56[rbp]
	mov	eax, DWORD PTR [rax]
	sub	eax, 3
	cmp	DWORD PTR -20[rbp], eax
	jl	.L43
	add	DWORD PTR -24[rbp], 1
.L38:
	mov	rax, QWORD PTR -56[rbp]
	mov	eax, DWORD PTR 4[rax]
	cmp	DWORD PTR -24[rbp], eax
	jl	.L44
	mov	DWORD PTR -16[rbp], 0
	jmp	.L45
.L51:
	mov	DWORD PTR -12[rbp], 0
	jmp	.L46
.L50:
	mov	rax, QWORD PTR -56[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -16[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -12[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	cmp	BYTE PTR -33[rbp], al
	jne	.L47
	mov	rax, QWORD PTR -56[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -16[rbp]
	add	eax, 1
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -12[rbp]
	add	eax, 1
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	cmp	BYTE PTR -33[rbp], al
	jne	.L47
	mov	rax, QWORD PTR -56[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -16[rbp]
	add	eax, 2
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -12[rbp]
	add	eax, 2
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	cmp	BYTE PTR -33[rbp], al
	jne	.L47
	mov	rax, QWORD PTR -56[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -16[rbp]
	add	eax, 3
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -12[rbp]
	add	eax, 3
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	cmp	BYTE PTR -33[rbp], al
	jne	.L47
	mov	eax, 1
	jmp	.L48
.L47:
	mov	eax, 0
.L48:
	test	al, al
	je	.L49
	mov	eax, 1
	jmp	.L35
.L49:
	add	DWORD PTR -12[rbp], 1
.L46:
	mov	rax, QWORD PTR -56[rbp]
	mov	eax, DWORD PTR 4[rax]
	sub	eax, 3
	cmp	DWORD PTR -12[rbp], eax
	jl	.L50
	add	DWORD PTR -16[rbp], 1
.L45:
	mov	rax, QWORD PTR -56[rbp]
	mov	eax, DWORD PTR [rax]
	sub	eax, 3
	cmp	DWORD PTR -16[rbp], eax
	jl	.L51
	mov	DWORD PTR -8[rbp], 0
	jmp	.L52
.L58:
	mov	rax, QWORD PTR -56[rbp]
	mov	eax, DWORD PTR 4[rax]
	sub	eax, 1
	mov	DWORD PTR -4[rbp], eax
	jmp	.L53
.L57:
	mov	rax, QWORD PTR -56[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -8[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -4[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	cmp	BYTE PTR -33[rbp], al
	jne	.L54
	mov	rax, QWORD PTR -56[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -8[rbp]
	add	eax, 1
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -4[rbp]
	sub	eax, 1
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	cmp	BYTE PTR -33[rbp], al
	jne	.L54
	mov	rax, QWORD PTR -56[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -8[rbp]
	add	eax, 2
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -4[rbp]
	sub	eax, 2
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	cmp	BYTE PTR -33[rbp], al
	jne	.L54
	mov	rax, QWORD PTR -56[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -8[rbp]
	add	eax, 3
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -4[rbp]
	sub	eax, 3
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	cmp	BYTE PTR -33[rbp], al
	jne	.L54
	mov	eax, 1
	jmp	.L55
.L54:
	mov	eax, 0
.L55:
	test	al, al
	je	.L56
	mov	eax, 1
	jmp	.L35
.L56:
	sub	DWORD PTR -4[rbp], 1
.L53:
	cmp	DWORD PTR -4[rbp], 2
	jg	.L57
	add	DWORD PTR -8[rbp], 1
.L52:
	mov	rax, QWORD PTR -56[rbp]
	mov	eax, DWORD PTR [rax]
	sub	eax, 3
	cmp	DWORD PTR -8[rbp], eax
	jl	.L58
	mov	eax, 0
.L35:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2091:
	.size	_ZNK8Connect48checkWinEv, .-_ZNK8Connect48checkWinEv
	.section	.text._ZNK8Connect49boardFullEv,"axG",@progbits,_ZNK8Connect49boardFullEv,comdat
	.align 2
	.weak	_ZNK8Connect49boardFullEv
	.type	_ZNK8Connect49boardFullEv, @function
_ZNK8Connect49boardFullEv:
.LFB2092:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -24[rbp], rdi
	mov	DWORD PTR -8[rbp], 0
	jmp	.L60
.L65:
	mov	DWORD PTR -4[rbp], 0
	jmp	.L61
.L64:
	mov	rax, QWORD PTR -24[rbp]
	lea	rdx, 8[rax]
	mov	eax, DWORD PTR -8[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	mov	rdx, rax
	mov	eax, DWORD PTR -4[rbp]
	cdqe
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZNKSt6vectorIcSaIcEEixEm
	movzx	eax, BYTE PTR [rax]
	cmp	al, 32
	sete	al
	test	al, al
	je	.L62
	mov	eax, 0
	jmp	.L63
.L62:
	add	DWORD PTR -4[rbp], 1
.L61:
	mov	rax, QWORD PTR -24[rbp]
	mov	eax, DWORD PTR 4[rax]
	cmp	DWORD PTR -4[rbp], eax
	jl	.L64
	add	DWORD PTR -8[rbp], 1
.L60:
	mov	rax, QWORD PTR -24[rbp]
	mov	eax, DWORD PTR [rax]
	cmp	DWORD PTR -8[rbp], eax
	jl	.L65
	mov	eax, 1
.L63:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2092:
	.size	_ZNK8Connect49boardFullEv, .-_ZNK8Connect49boardFullEv
	.section	.rodata
.LC6:
	.string	"Player "
.LC7:
	.string	", enter column (1-7): "
.LC8:
	.string	" wins!"
.LC9:
	.string	"It's a draw!"
	.section	.text._ZN8Connect44playEv,"axG",@progbits,_ZN8Connect44playEv,comdat
	.align 2
	.weak	_ZN8Connect44playEv
	.type	_ZN8Connect44playEv, @function
_ZN8Connect44playEv:
.LFB2093:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -24[rbp], rdi
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR -8[rbp], rax
	xor	eax, eax
	mov	rax, QWORD PTR -24[rbp]
	mov	rdi, rax
	call	_ZNK8Connect412displayBoardEv
.L73:
	lea	rax, .LC6[rip]
	mov	rsi, rax
	lea	rax, _ZSt4cout[rip]
	mov	rdi, rax
	call	_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc@PLT
	mov	rdx, rax
	mov	rax, QWORD PTR -24[rbp]
	mov	eax, DWORD PTR 32[rax]
	mov	esi, eax
	mov	rdi, rdx
	call	_ZNSolsEi@PLT
	mov	rdx, rax
	lea	rax, .LC7[rip]
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc@PLT
	lea	rax, -12[rbp]
	mov	rsi, rax
	lea	rax, _ZSt3cin[rip]
	mov	rdi, rax
	call	_ZNSirsERi@PLT
	mov	eax, DWORD PTR -12[rbp]
	sub	eax, 1
	mov	DWORD PTR -12[rbp], eax
	mov	edx, DWORD PTR -12[rbp]
	mov	rax, QWORD PTR -24[rbp]
	mov	esi, edx
	mov	rdi, rax
	call	_ZN8Connect49dropPieceEi
	test	al, al
	je	.L73
	mov	rax, QWORD PTR -24[rbp]
	mov	rdi, rax
	call	_ZNK8Connect48checkWinEv
	test	al, al
	je	.L68
	mov	rax, QWORD PTR -24[rbp]
	mov	rdi, rax
	call	_ZNK8Connect412displayBoardEv
	lea	rax, .LC6[rip]
	mov	rsi, rax
	lea	rax, _ZSt4cout[rip]
	mov	rdi, rax
	call	_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc@PLT
	mov	rdx, rax
	mov	rax, QWORD PTR -24[rbp]
	mov	eax, DWORD PTR 32[rax]
	mov	esi, eax
	mov	rdi, rdx
	call	_ZNSolsEi@PLT
	mov	rdx, rax
	lea	rax, .LC8[rip]
	mov	rsi, rax
	mov	rdi, rdx
	call	_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc@PLT
	mov	rdx, QWORD PTR _ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_@GOTPCREL[rip]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSolsEPFRSoS_E@PLT
	jmp	.L69
.L68:
	mov	rax, QWORD PTR -24[rbp]
	mov	rdi, rax
	call	_ZNK8Connect49boardFullEv
	test	al, al
	je	.L70
	mov	rax, QWORD PTR -24[rbp]
	mov	rdi, rax
	call	_ZNK8Connect412displayBoardEv
	lea	rax, .LC9[rip]
	mov	rsi, rax
	lea	rax, _ZSt4cout[rip]
	mov	rdi, rax
	call	_ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc@PLT
	mov	rdx, QWORD PTR _ZSt4endlIcSt11char_traitsIcEERSt13basic_ostreamIT_T0_ES6_@GOTPCREL[rip]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSolsEPFRSoS_E@PLT
	jmp	.L69
.L70:
	mov	rax, QWORD PTR -24[rbp]
	mov	eax, DWORD PTR 32[rax]
	cmp	eax, 1
	jne	.L71
	mov	edx, 2
	jmp	.L72
.L71:
	mov	edx, 1
.L72:
	mov	rax, QWORD PTR -24[rbp]
	mov	DWORD PTR 32[rax], edx
	mov	rax, QWORD PTR -24[rbp]
	mov	rdi, rax
	call	_ZNK8Connect412displayBoardEv
	jmp	.L73
.L69:
	nop
	mov	rax, QWORD PTR -8[rbp]
	sub	rax, QWORD PTR fs:40
	je	.L74
	call	__stack_chk_fail@PLT
.L74:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2093:
	.size	_ZN8Connect44playEv, .-_ZN8Connect44playEv
	.section	.text._ZN8Connect4D2Ev,"axG",@progbits,_ZN8Connect4D5Ev,comdat
	.align 2
	.weak	_ZN8Connect4D2Ev
	.type	_ZN8Connect4D2Ev, @function
_ZN8Connect4D2Ev:
.LFB2096:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	add	rax, 8
	mov	rdi, rax
	call	_ZNSt6vectorIS_IcSaIcEESaIS1_EED1Ev
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2096:
	.size	_ZN8Connect4D2Ev, .-_ZN8Connect4D2Ev
	.weak	_ZN8Connect4D1Ev
	.set	_ZN8Connect4D1Ev,_ZN8Connect4D2Ev
	.text
	.globl	main
	.type	main, @function
main:
.LFB2094:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA2094
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	push	rbx
	sub	rsp, 56
	.cfi_offset 3, -24
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR -24[rbp], rax
	xor	eax, eax
	lea	rax, -64[rbp]
	mov	rdi, rax
.LEHB3:
	call	_ZN8Connect4C1Ev
.LEHE3:
	lea	rax, -64[rbp]
	mov	rdi, rax
.LEHB4:
	call	_ZN8Connect44playEv
.LEHE4:
	mov	ebx, 0
	lea	rax, -64[rbp]
	mov	rdi, rax
	call	_ZN8Connect4D1Ev
	mov	eax, ebx
	mov	rdx, QWORD PTR -24[rbp]
	sub	rdx, QWORD PTR fs:40
	je	.L79
	jmp	.L81
.L80:
	endbr64
	mov	rbx, rax
	lea	rax, -64[rbp]
	mov	rdi, rax
	call	_ZN8Connect4D1Ev
	mov	rax, rbx
	mov	rdi, rax
.LEHB5:
	call	_Unwind_Resume@PLT
.LEHE5:
.L81:
	call	__stack_chk_fail@PLT
.L79:
	mov	rbx, QWORD PTR -8[rbp]
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2094:
	.section	.gcc_except_table,"a",@progbits
.LLSDA2094:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE2094-.LLSDACSB2094
.LLSDACSB2094:
	.uleb128 .LEHB3-.LFB2094
	.uleb128 .LEHE3-.LEHB3
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB4-.LFB2094
	.uleb128 .LEHE4-.LEHB4
	.uleb128 .L80-.LFB2094
	.uleb128 0
	.uleb128 .LEHB5-.LFB2094
	.uleb128 .LEHE5-.LEHB5
	.uleb128 0
	.uleb128 0
.LLSDACSE2094:
	.text
	.size	main, .-main
	.section	.text._ZNSt6vectorIcSaIcEEC2EmRKcRKS0_,"axG",@progbits,_ZNSt6vectorIcSaIcEEC5EmRKcRKS0_,comdat
	.align 2
	.weak	_ZNSt6vectorIcSaIcEEC2EmRKcRKS0_
	.type	_ZNSt6vectorIcSaIcEEC2EmRKcRKS0_, @function
_ZNSt6vectorIcSaIcEEC2EmRKcRKS0_:
.LFB2359:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA2359
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	push	rbx
	sub	rsp, 40
	.cfi_offset 3, -24
	mov	QWORD PTR -24[rbp], rdi
	mov	QWORD PTR -32[rbp], rsi
	mov	QWORD PTR -40[rbp], rdx
	mov	QWORD PTR -48[rbp], rcx
	mov	rbx, QWORD PTR -24[rbp]
	mov	rdx, QWORD PTR -48[rbp]
	mov	rax, QWORD PTR -32[rbp]
	mov	rsi, rdx
	mov	rdi, rax
.LEHB6:
	call	_ZNSt6vectorIcSaIcEE17_S_check_init_lenEmRKS0_
	mov	rcx, rax
	mov	rax, QWORD PTR -48[rbp]
	mov	rdx, rax
	mov	rsi, rcx
	mov	rdi, rbx
	call	_ZNSt12_Vector_baseIcSaIcEEC2EmRKS0_
.LEHE6:
	mov	rdx, QWORD PTR -40[rbp]
	mov	rcx, QWORD PTR -32[rbp]
	mov	rax, QWORD PTR -24[rbp]
	mov	rsi, rcx
	mov	rdi, rax
.LEHB7:
	call	_ZNSt6vectorIcSaIcEE18_M_fill_initializeEmRKc
.LEHE7:
	jmp	.L85
.L84:
	endbr64
	mov	rbx, rax
	mov	rax, QWORD PTR -24[rbp]
	mov	rdi, rax
	call	_ZNSt12_Vector_baseIcSaIcEED2Ev
	mov	rax, rbx
	mov	rdi, rax
.LEHB8:
	call	_Unwind_Resume@PLT
.LEHE8:
.L85:
	mov	rbx, QWORD PTR -8[rbp]
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2359:
	.section	.gcc_except_table
.LLSDA2359:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE2359-.LLSDACSB2359
.LLSDACSB2359:
	.uleb128 .LEHB6-.LFB2359
	.uleb128 .LEHE6-.LEHB6
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB7-.LFB2359
	.uleb128 .LEHE7-.LEHB7
	.uleb128 .L84-.LFB2359
	.uleb128 0
	.uleb128 .LEHB8-.LFB2359
	.uleb128 .LEHE8-.LEHB8
	.uleb128 0
	.uleb128 0
.LLSDACSE2359:
	.section	.text._ZNSt6vectorIcSaIcEEC2EmRKcRKS0_,"axG",@progbits,_ZNSt6vectorIcSaIcEEC5EmRKcRKS0_,comdat
	.size	_ZNSt6vectorIcSaIcEEC2EmRKcRKS0_, .-_ZNSt6vectorIcSaIcEEC2EmRKcRKS0_
	.weak	_ZNSt6vectorIcSaIcEEC1EmRKcRKS0_
	.set	_ZNSt6vectorIcSaIcEEC1EmRKcRKS0_,_ZNSt6vectorIcSaIcEEC2EmRKcRKS0_
	.section	.text._ZNSt6vectorIcSaIcEED2Ev,"axG",@progbits,_ZNSt6vectorIcSaIcEED5Ev,comdat
	.align 2
	.weak	_ZNSt6vectorIcSaIcEED2Ev
	.type	_ZNSt6vectorIcSaIcEED2Ev, @function
_ZNSt6vectorIcSaIcEED2Ev:
.LFB2362:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA2362
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNSt12_Vector_baseIcSaIcEE19_M_get_Tp_allocatorEv
	mov	rdx, rax
	mov	rax, QWORD PTR -8[rbp]
	mov	rcx, QWORD PTR 8[rax]
	mov	rax, QWORD PTR -8[rbp]
	mov	rax, QWORD PTR [rax]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZSt8_DestroyIPccEvT_S1_RSaIT0_E
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNSt12_Vector_baseIcSaIcEED2Ev
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2362:
	.section	.gcc_except_table
.LLSDA2362:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE2362-.LLSDACSB2362
.LLSDACSB2362:
.LLSDACSE2362:
	.section	.text._ZNSt6vectorIcSaIcEED2Ev,"axG",@progbits,_ZNSt6vectorIcSaIcEED5Ev,comdat
	.size	_ZNSt6vectorIcSaIcEED2Ev, .-_ZNSt6vectorIcSaIcEED2Ev
	.weak	_ZNSt6vectorIcSaIcEED1Ev
	.set	_ZNSt6vectorIcSaIcEED1Ev,_ZNSt6vectorIcSaIcEED2Ev
	.section	.text._ZNSaISt6vectorIcSaIcEEEC2Ev,"axG",@progbits,_ZNSaISt6vectorIcSaIcEEEC5Ev,comdat
	.align 2
	.weak	_ZNSaISt6vectorIcSaIcEEEC2Ev
	.type	_ZNSaISt6vectorIcSaIcEEEC2Ev, @function
_ZNSaISt6vectorIcSaIcEEEC2Ev:
.LFB2365:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC2Ev
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2365:
	.size	_ZNSaISt6vectorIcSaIcEEEC2Ev, .-_ZNSaISt6vectorIcSaIcEEEC2Ev
	.weak	_ZNSaISt6vectorIcSaIcEEEC1Ev
	.set	_ZNSaISt6vectorIcSaIcEEEC1Ev,_ZNSaISt6vectorIcSaIcEEEC2Ev
	.section	.text._ZNSaISt6vectorIcSaIcEEED2Ev,"axG",@progbits,_ZNSaISt6vectorIcSaIcEEED5Ev,comdat
	.align 2
	.weak	_ZNSaISt6vectorIcSaIcEEED2Ev
	.type	_ZNSaISt6vectorIcSaIcEEED2Ev, @function
_ZNSaISt6vectorIcSaIcEEED2Ev:
.LFB2368:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEED2Ev
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2368:
	.size	_ZNSaISt6vectorIcSaIcEEED2Ev, .-_ZNSaISt6vectorIcSaIcEEED2Ev
	.weak	_ZNSaISt6vectorIcSaIcEEED1Ev
	.set	_ZNSaISt6vectorIcSaIcEEED1Ev,_ZNSaISt6vectorIcSaIcEEED2Ev
	.section	.text._ZNSt6vectorIS_IcSaIcEESaIS1_EEC2EmRKS1_RKS2_,"axG",@progbits,_ZNSt6vectorIS_IcSaIcEESaIS1_EEC5EmRKS1_RKS2_,comdat
	.align 2
	.weak	_ZNSt6vectorIS_IcSaIcEESaIS1_EEC2EmRKS1_RKS2_
	.type	_ZNSt6vectorIS_IcSaIcEESaIS1_EEC2EmRKS1_RKS2_, @function
_ZNSt6vectorIS_IcSaIcEESaIS1_EEC2EmRKS1_RKS2_:
.LFB2371:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA2371
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	push	rbx
	sub	rsp, 40
	.cfi_offset 3, -24
	mov	QWORD PTR -24[rbp], rdi
	mov	QWORD PTR -32[rbp], rsi
	mov	QWORD PTR -40[rbp], rdx
	mov	QWORD PTR -48[rbp], rcx
	mov	rbx, QWORD PTR -24[rbp]
	mov	rdx, QWORD PTR -48[rbp]
	mov	rax, QWORD PTR -32[rbp]
	mov	rsi, rdx
	mov	rdi, rax
.LEHB9:
	call	_ZNSt6vectorIS_IcSaIcEESaIS1_EE17_S_check_init_lenEmRKS2_
	mov	rcx, rax
	mov	rax, QWORD PTR -48[rbp]
	mov	rdx, rax
	mov	rsi, rcx
	mov	rdi, rbx
	call	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EEC2EmRKS3_
.LEHE9:
	mov	rdx, QWORD PTR -40[rbp]
	mov	rcx, QWORD PTR -32[rbp]
	mov	rax, QWORD PTR -24[rbp]
	mov	rsi, rcx
	mov	rdi, rax
.LEHB10:
	call	_ZNSt6vectorIS_IcSaIcEESaIS1_EE18_M_fill_initializeEmRKS1_
.LEHE10:
	jmp	.L92
.L91:
	endbr64
	mov	rbx, rax
	mov	rax, QWORD PTR -24[rbp]
	mov	rdi, rax
	call	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EED2Ev
	mov	rax, rbx
	mov	rdi, rax
.LEHB11:
	call	_Unwind_Resume@PLT
.LEHE11:
.L92:
	mov	rbx, QWORD PTR -8[rbp]
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2371:
	.section	.gcc_except_table
.LLSDA2371:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE2371-.LLSDACSB2371
.LLSDACSB2371:
	.uleb128 .LEHB9-.LFB2371
	.uleb128 .LEHE9-.LEHB9
	.uleb128 0
	.uleb128 0
	.uleb128 .LEHB10-.LFB2371
	.uleb128 .LEHE10-.LEHB10
	.uleb128 .L91-.LFB2371
	.uleb128 0
	.uleb128 .LEHB11-.LFB2371
	.uleb128 .LEHE11-.LEHB11
	.uleb128 0
	.uleb128 0
.LLSDACSE2371:
	.section	.text._ZNSt6vectorIS_IcSaIcEESaIS1_EEC2EmRKS1_RKS2_,"axG",@progbits,_ZNSt6vectorIS_IcSaIcEESaIS1_EEC5EmRKS1_RKS2_,comdat
	.size	_ZNSt6vectorIS_IcSaIcEESaIS1_EEC2EmRKS1_RKS2_, .-_ZNSt6vectorIS_IcSaIcEESaIS1_EEC2EmRKS1_RKS2_
	.weak	_ZNSt6vectorIS_IcSaIcEESaIS1_EEC1EmRKS1_RKS2_
	.set	_ZNSt6vectorIS_IcSaIcEESaIS1_EEC1EmRKS1_RKS2_,_ZNSt6vectorIS_IcSaIcEESaIS1_EEC2EmRKS1_RKS2_
	.section	.text._ZNSt6vectorIS_IcSaIcEESaIS1_EED2Ev,"axG",@progbits,_ZNSt6vectorIS_IcSaIcEESaIS1_EED5Ev,comdat
	.align 2
	.weak	_ZNSt6vectorIS_IcSaIcEESaIS1_EED2Ev
	.type	_ZNSt6vectorIS_IcSaIcEESaIS1_EED2Ev, @function
_ZNSt6vectorIS_IcSaIcEESaIS1_EED2Ev:
.LFB2374:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA2374
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE19_M_get_Tp_allocatorEv
	mov	rdx, rax
	mov	rax, QWORD PTR -8[rbp]
	mov	rcx, QWORD PTR 8[rax]
	mov	rax, QWORD PTR -8[rbp]
	mov	rax, QWORD PTR [rax]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZSt8_DestroyIPSt6vectorIcSaIcEES2_EvT_S4_RSaIT0_E
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EED2Ev
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2374:
	.section	.gcc_except_table
.LLSDA2374:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE2374-.LLSDACSB2374
.LLSDACSB2374:
.LLSDACSE2374:
	.section	.text._ZNSt6vectorIS_IcSaIcEESaIS1_EED2Ev,"axG",@progbits,_ZNSt6vectorIS_IcSaIcEESaIS1_EED5Ev,comdat
	.size	_ZNSt6vectorIS_IcSaIcEESaIS1_EED2Ev, .-_ZNSt6vectorIS_IcSaIcEESaIS1_EED2Ev
	.weak	_ZNSt6vectorIS_IcSaIcEESaIS1_EED1Ev
	.set	_ZNSt6vectorIS_IcSaIcEESaIS1_EED1Ev,_ZNSt6vectorIS_IcSaIcEESaIS1_EED2Ev
	.section	.text._ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm,"axG",@progbits,_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm,comdat
	.align 2
	.weak	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	.type	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm, @function
_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm:
.LFB2379:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	rax, QWORD PTR -8[rbp]
	mov	rcx, QWORD PTR [rax]
	mov	rdx, QWORD PTR -16[rbp]
	mov	rax, rdx
	add	rax, rax
	add	rax, rdx
	sal	rax, 3
	add	rax, rcx
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2379:
	.size	_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm, .-_ZNKSt6vectorIS_IcSaIcEESaIS1_EEixEm
	.section	.text._ZNKSt6vectorIcSaIcEEixEm,"axG",@progbits,_ZNKSt6vectorIcSaIcEEixEm,comdat
	.align 2
	.weak	_ZNKSt6vectorIcSaIcEEixEm
	.type	_ZNKSt6vectorIcSaIcEEixEm, @function
_ZNKSt6vectorIcSaIcEEixEm:
.LFB2380:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	rax, QWORD PTR -8[rbp]
	mov	rdx, QWORD PTR [rax]
	mov	rax, QWORD PTR -16[rbp]
	add	rax, rdx
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2380:
	.size	_ZNKSt6vectorIcSaIcEEixEm, .-_ZNKSt6vectorIcSaIcEEixEm
	.section	.text._ZNSt6vectorIS_IcSaIcEESaIS1_EEixEm,"axG",@progbits,_ZNSt6vectorIS_IcSaIcEESaIS1_EEixEm,comdat
	.align 2
	.weak	_ZNSt6vectorIS_IcSaIcEESaIS1_EEixEm
	.type	_ZNSt6vectorIS_IcSaIcEESaIS1_EEixEm, @function
_ZNSt6vectorIS_IcSaIcEESaIS1_EEixEm:
.LFB2382:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	rax, QWORD PTR -8[rbp]
	mov	rcx, QWORD PTR [rax]
	mov	rdx, QWORD PTR -16[rbp]
	mov	rax, rdx
	add	rax, rax
	add	rax, rdx
	sal	rax, 3
	add	rax, rcx
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2382:
	.size	_ZNSt6vectorIS_IcSaIcEESaIS1_EEixEm, .-_ZNSt6vectorIS_IcSaIcEESaIS1_EEixEm
	.section	.text._ZNSt6vectorIcSaIcEEixEm,"axG",@progbits,_ZNSt6vectorIcSaIcEEixEm,comdat
	.align 2
	.weak	_ZNSt6vectorIcSaIcEEixEm
	.type	_ZNSt6vectorIcSaIcEEixEm, @function
_ZNSt6vectorIcSaIcEEixEm:
.LFB2383:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	rax, QWORD PTR -8[rbp]
	mov	rdx, QWORD PTR [rax]
	mov	rax, QWORD PTR -16[rbp]
	add	rax, rdx
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2383:
	.size	_ZNSt6vectorIcSaIcEEixEm, .-_ZNSt6vectorIcSaIcEEixEm
	.section	.rodata
	.align 8
.LC10:
	.string	"cannot create std::vector larger than max_size()"
	.section	.text._ZNSt6vectorIcSaIcEE17_S_check_init_lenEmRKS0_,"axG",@progbits,_ZNSt6vectorIcSaIcEE17_S_check_init_lenEmRKS0_,comdat
	.weak	_ZNSt6vectorIcSaIcEE17_S_check_init_lenEmRKS0_
	.type	_ZNSt6vectorIcSaIcEE17_S_check_init_lenEmRKS0_, @function
_ZNSt6vectorIcSaIcEE17_S_check_init_lenEmRKS0_:
.LFB2461:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	push	rbx
	sub	rsp, 40
	.cfi_offset 3, -24
	mov	QWORD PTR -40[rbp], rdi
	mov	QWORD PTR -48[rbp], rsi
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR -24[rbp], rax
	xor	eax, eax
	mov	rdx, QWORD PTR -48[rbp]
	lea	rax, -25[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSaIcEC1ERKS_@PLT
	lea	rax, -25[rbp]
	mov	rdi, rax
	call	_ZNSt6vectorIcSaIcEE11_S_max_sizeERKS0_
	cmp	QWORD PTR -40[rbp], rax
	seta	bl
	lea	rax, -25[rbp]
	mov	rdi, rax
	call	_ZNSaIcED1Ev@PLT
	test	bl, bl
	je	.L103
	lea	rax, .LC10[rip]
	mov	rdi, rax
	call	_ZSt20__throw_length_errorPKc@PLT
.L103:
	mov	rax, QWORD PTR -40[rbp]
	mov	rdx, QWORD PTR -24[rbp]
	sub	rdx, QWORD PTR fs:40
	je	.L105
	call	__stack_chk_fail@PLT
.L105:
	mov	rbx, QWORD PTR -8[rbp]
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2461:
	.size	_ZNSt6vectorIcSaIcEE17_S_check_init_lenEmRKS0_, .-_ZNSt6vectorIcSaIcEE17_S_check_init_lenEmRKS0_
	.section	.text._ZNSt12_Vector_baseIcSaIcEE12_Vector_implD2Ev,"axG",@progbits,_ZNSt12_Vector_baseIcSaIcEE12_Vector_implD5Ev,comdat
	.align 2
	.weak	_ZNSt12_Vector_baseIcSaIcEE12_Vector_implD2Ev
	.type	_ZNSt12_Vector_baseIcSaIcEE12_Vector_implD2Ev, @function
_ZNSt12_Vector_baseIcSaIcEE12_Vector_implD2Ev:
.LFB2464:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNSaIcED2Ev@PLT
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2464:
	.size	_ZNSt12_Vector_baseIcSaIcEE12_Vector_implD2Ev, .-_ZNSt12_Vector_baseIcSaIcEE12_Vector_implD2Ev
	.weak	_ZNSt12_Vector_baseIcSaIcEE12_Vector_implD1Ev
	.set	_ZNSt12_Vector_baseIcSaIcEE12_Vector_implD1Ev,_ZNSt12_Vector_baseIcSaIcEE12_Vector_implD2Ev
	.section	.text._ZNSt12_Vector_baseIcSaIcEEC2EmRKS0_,"axG",@progbits,_ZNSt12_Vector_baseIcSaIcEEC5EmRKS0_,comdat
	.align 2
	.weak	_ZNSt12_Vector_baseIcSaIcEEC2EmRKS0_
	.type	_ZNSt12_Vector_baseIcSaIcEEC2EmRKS0_, @function
_ZNSt12_Vector_baseIcSaIcEEC2EmRKS0_:
.LFB2466:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA2466
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	push	rbx
	sub	rsp, 40
	.cfi_offset 3, -24
	mov	QWORD PTR -24[rbp], rdi
	mov	QWORD PTR -32[rbp], rsi
	mov	QWORD PTR -40[rbp], rdx
	mov	rax, QWORD PTR -24[rbp]
	mov	rdx, QWORD PTR -40[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSt12_Vector_baseIcSaIcEE12_Vector_implC1ERKS0_
	mov	rdx, QWORD PTR -32[rbp]
	mov	rax, QWORD PTR -24[rbp]
	mov	rsi, rdx
	mov	rdi, rax
.LEHB12:
	call	_ZNSt12_Vector_baseIcSaIcEE17_M_create_storageEm
.LEHE12:
	jmp	.L110
.L109:
	endbr64
	mov	rbx, rax
	mov	rax, QWORD PTR -24[rbp]
	mov	rdi, rax
	call	_ZNSt12_Vector_baseIcSaIcEE12_Vector_implD1Ev
	mov	rax, rbx
	mov	rdi, rax
.LEHB13:
	call	_Unwind_Resume@PLT
.LEHE13:
.L110:
	mov	rbx, QWORD PTR -8[rbp]
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2466:
	.section	.gcc_except_table
.LLSDA2466:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE2466-.LLSDACSB2466
.LLSDACSB2466:
	.uleb128 .LEHB12-.LFB2466
	.uleb128 .LEHE12-.LEHB12
	.uleb128 .L109-.LFB2466
	.uleb128 0
	.uleb128 .LEHB13-.LFB2466
	.uleb128 .LEHE13-.LEHB13
	.uleb128 0
	.uleb128 0
.LLSDACSE2466:
	.section	.text._ZNSt12_Vector_baseIcSaIcEEC2EmRKS0_,"axG",@progbits,_ZNSt12_Vector_baseIcSaIcEEC5EmRKS0_,comdat
	.size	_ZNSt12_Vector_baseIcSaIcEEC2EmRKS0_, .-_ZNSt12_Vector_baseIcSaIcEEC2EmRKS0_
	.weak	_ZNSt12_Vector_baseIcSaIcEEC1EmRKS0_
	.set	_ZNSt12_Vector_baseIcSaIcEEC1EmRKS0_,_ZNSt12_Vector_baseIcSaIcEEC2EmRKS0_
	.section	.text._ZNSt12_Vector_baseIcSaIcEED2Ev,"axG",@progbits,_ZNSt12_Vector_baseIcSaIcEED5Ev,comdat
	.align 2
	.weak	_ZNSt12_Vector_baseIcSaIcEED2Ev
	.type	_ZNSt12_Vector_baseIcSaIcEED2Ev, @function
_ZNSt12_Vector_baseIcSaIcEED2Ev:
.LFB2469:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA2469
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	mov	rdx, QWORD PTR 16[rax]
	mov	rax, QWORD PTR -8[rbp]
	mov	rcx, QWORD PTR [rax]
	mov	rax, rdx
	sub	rax, rcx
	mov	rdx, rax
	mov	rax, QWORD PTR -8[rbp]
	mov	rcx, QWORD PTR [rax]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZNSt12_Vector_baseIcSaIcEE13_M_deallocateEPcm
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNSt12_Vector_baseIcSaIcEE12_Vector_implD1Ev
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2469:
	.section	.gcc_except_table
.LLSDA2469:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE2469-.LLSDACSB2469
.LLSDACSB2469:
.LLSDACSE2469:
	.section	.text._ZNSt12_Vector_baseIcSaIcEED2Ev,"axG",@progbits,_ZNSt12_Vector_baseIcSaIcEED5Ev,comdat
	.size	_ZNSt12_Vector_baseIcSaIcEED2Ev, .-_ZNSt12_Vector_baseIcSaIcEED2Ev
	.weak	_ZNSt12_Vector_baseIcSaIcEED1Ev
	.set	_ZNSt12_Vector_baseIcSaIcEED1Ev,_ZNSt12_Vector_baseIcSaIcEED2Ev
	.section	.text._ZNSt6vectorIcSaIcEE18_M_fill_initializeEmRKc,"axG",@progbits,_ZNSt6vectorIcSaIcEE18_M_fill_initializeEmRKc,comdat
	.align 2
	.weak	_ZNSt6vectorIcSaIcEE18_M_fill_initializeEmRKc
	.type	_ZNSt6vectorIcSaIcEE18_M_fill_initializeEmRKc, @function
_ZNSt6vectorIcSaIcEE18_M_fill_initializeEmRKc:
.LFB2471:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNSt12_Vector_baseIcSaIcEE19_M_get_Tp_allocatorEv
	mov	rcx, rax
	mov	rax, QWORD PTR -8[rbp]
	mov	rax, QWORD PTR [rax]
	mov	rdx, QWORD PTR -24[rbp]
	mov	rsi, QWORD PTR -16[rbp]
	mov	rdi, rax
	call	_ZSt24__uninitialized_fill_n_aIPcmccET_S1_T0_RKT1_RSaIT2_E
	mov	rdx, QWORD PTR -8[rbp]
	mov	QWORD PTR 8[rdx], rax
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2471:
	.size	_ZNSt6vectorIcSaIcEE18_M_fill_initializeEmRKc, .-_ZNSt6vectorIcSaIcEE18_M_fill_initializeEmRKc
	.section	.text._ZNSt12_Vector_baseIcSaIcEE19_M_get_Tp_allocatorEv,"axG",@progbits,_ZNSt12_Vector_baseIcSaIcEE19_M_get_Tp_allocatorEv,comdat
	.align 2
	.weak	_ZNSt12_Vector_baseIcSaIcEE19_M_get_Tp_allocatorEv
	.type	_ZNSt12_Vector_baseIcSaIcEE19_M_get_Tp_allocatorEv, @function
_ZNSt12_Vector_baseIcSaIcEE19_M_get_Tp_allocatorEv:
.LFB2472:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2472:
	.size	_ZNSt12_Vector_baseIcSaIcEE19_M_get_Tp_allocatorEv, .-_ZNSt12_Vector_baseIcSaIcEE19_M_get_Tp_allocatorEv
	.section	.text._ZSt8_DestroyIPccEvT_S1_RSaIT0_E,"axG",@progbits,_ZSt8_DestroyIPccEvT_S1_RSaIT0_E,comdat
	.weak	_ZSt8_DestroyIPccEvT_S1_RSaIT0_E
	.type	_ZSt8_DestroyIPccEvT_S1_RSaIT0_E, @function
_ZSt8_DestroyIPccEvT_S1_RSaIT0_E:
.LFB2473:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	mov	rdx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZSt8_DestroyIPcEvT_S1_
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2473:
	.size	_ZSt8_DestroyIPccEvT_S1_RSaIT0_E, .-_ZSt8_DestroyIPccEvT_S1_RSaIT0_E
	.section	.text._ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC2Ev,"axG",@progbits,_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC5Ev,comdat
	.align 2
	.weak	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC2Ev
	.type	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC2Ev, @function
_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC2Ev:
.LFB2475:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	nop
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2475:
	.size	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC2Ev, .-_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC2Ev
	.weak	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC1Ev
	.set	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC1Ev,_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC2Ev
	.section	.text._ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEED2Ev,"axG",@progbits,_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEED5Ev,comdat
	.align 2
	.weak	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEED2Ev
	.type	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEED2Ev, @function
_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEED2Ev:
.LFB2478:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	nop
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2478:
	.size	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEED2Ev, .-_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEED2Ev
	.weak	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEED1Ev
	.set	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEED1Ev,_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEED2Ev
	.section	.text._ZNSt6vectorIS_IcSaIcEESaIS1_EE17_S_check_init_lenEmRKS2_,"axG",@progbits,_ZNSt6vectorIS_IcSaIcEESaIS1_EE17_S_check_init_lenEmRKS2_,comdat
	.weak	_ZNSt6vectorIS_IcSaIcEESaIS1_EE17_S_check_init_lenEmRKS2_
	.type	_ZNSt6vectorIS_IcSaIcEESaIS1_EE17_S_check_init_lenEmRKS2_, @function
_ZNSt6vectorIS_IcSaIcEESaIS1_EE17_S_check_init_lenEmRKS2_:
.LFB2480:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	push	rbx
	sub	rsp, 40
	.cfi_offset 3, -24
	mov	QWORD PTR -40[rbp], rdi
	mov	QWORD PTR -48[rbp], rsi
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR -24[rbp], rax
	xor	eax, eax
	mov	rdx, QWORD PTR -48[rbp]
	lea	rax, -25[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSaISt6vectorIcSaIcEEEC1ERKS2_
	lea	rax, -25[rbp]
	mov	rdi, rax
	call	_ZNSt6vectorIS_IcSaIcEESaIS1_EE11_S_max_sizeERKS2_
	cmp	QWORD PTR -40[rbp], rax
	seta	bl
	lea	rax, -25[rbp]
	mov	rdi, rax
	call	_ZNSaISt6vectorIcSaIcEEED1Ev
	test	bl, bl
	je	.L119
	lea	rax, .LC10[rip]
	mov	rdi, rax
	call	_ZSt20__throw_length_errorPKc@PLT
.L119:
	mov	rax, QWORD PTR -40[rbp]
	mov	rdx, QWORD PTR -24[rbp]
	sub	rdx, QWORD PTR fs:40
	je	.L121
	call	__stack_chk_fail@PLT
.L121:
	mov	rbx, QWORD PTR -8[rbp]
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2480:
	.size	_ZNSt6vectorIS_IcSaIcEESaIS1_EE17_S_check_init_lenEmRKS2_, .-_ZNSt6vectorIS_IcSaIcEESaIS1_EE17_S_check_init_lenEmRKS2_
	.section	.text._ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implD2Ev,"axG",@progbits,_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implD5Ev,comdat
	.align 2
	.weak	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implD2Ev
	.type	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implD2Ev, @function
_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implD2Ev:
.LFB2483:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNSaISt6vectorIcSaIcEEED2Ev
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2483:
	.size	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implD2Ev, .-_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implD2Ev
	.weak	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implD1Ev
	.set	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implD1Ev,_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implD2Ev
	.section	.text._ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EEC2EmRKS3_,"axG",@progbits,_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EEC5EmRKS3_,comdat
	.align 2
	.weak	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EEC2EmRKS3_
	.type	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EEC2EmRKS3_, @function
_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EEC2EmRKS3_:
.LFB2485:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA2485
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	push	rbx
	sub	rsp, 40
	.cfi_offset 3, -24
	mov	QWORD PTR -24[rbp], rdi
	mov	QWORD PTR -32[rbp], rsi
	mov	QWORD PTR -40[rbp], rdx
	mov	rax, QWORD PTR -24[rbp]
	mov	rdx, QWORD PTR -40[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implC1ERKS3_
	mov	rdx, QWORD PTR -32[rbp]
	mov	rax, QWORD PTR -24[rbp]
	mov	rsi, rdx
	mov	rdi, rax
.LEHB14:
	call	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_M_create_storageEm
.LEHE14:
	jmp	.L126
.L125:
	endbr64
	mov	rbx, rax
	mov	rax, QWORD PTR -24[rbp]
	mov	rdi, rax
	call	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implD1Ev
	mov	rax, rbx
	mov	rdi, rax
.LEHB15:
	call	_Unwind_Resume@PLT
.LEHE15:
.L126:
	mov	rbx, QWORD PTR -8[rbp]
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2485:
	.section	.gcc_except_table
.LLSDA2485:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE2485-.LLSDACSB2485
.LLSDACSB2485:
	.uleb128 .LEHB14-.LFB2485
	.uleb128 .LEHE14-.LEHB14
	.uleb128 .L125-.LFB2485
	.uleb128 0
	.uleb128 .LEHB15-.LFB2485
	.uleb128 .LEHE15-.LEHB15
	.uleb128 0
	.uleb128 0
.LLSDACSE2485:
	.section	.text._ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EEC2EmRKS3_,"axG",@progbits,_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EEC5EmRKS3_,comdat
	.size	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EEC2EmRKS3_, .-_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EEC2EmRKS3_
	.weak	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EEC1EmRKS3_
	.set	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EEC1EmRKS3_,_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EEC2EmRKS3_
	.section	.text._ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EED2Ev,"axG",@progbits,_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EED5Ev,comdat
	.align 2
	.weak	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EED2Ev
	.type	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EED2Ev, @function
_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EED2Ev:
.LFB2488:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA2488
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	mov	rdx, QWORD PTR 16[rax]
	mov	rax, QWORD PTR -8[rbp]
	mov	rcx, QWORD PTR [rax]
	mov	rax, rdx
	sub	rax, rcx
	sar	rax, 3
	mov	rdx, rax
	movabs	rax, -6148914691236517205
	imul	rax, rdx
	mov	rdx, rax
	mov	rax, QWORD PTR -8[rbp]
	mov	rcx, QWORD PTR [rax]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE13_M_deallocateEPS2_m
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implD1Ev
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2488:
	.section	.gcc_except_table
.LLSDA2488:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE2488-.LLSDACSB2488
.LLSDACSB2488:
.LLSDACSE2488:
	.section	.text._ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EED2Ev,"axG",@progbits,_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EED5Ev,comdat
	.size	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EED2Ev, .-_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EED2Ev
	.weak	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EED1Ev
	.set	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EED1Ev,_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EED2Ev
	.section	.text._ZNSt6vectorIS_IcSaIcEESaIS1_EE18_M_fill_initializeEmRKS1_,"axG",@progbits,_ZNSt6vectorIS_IcSaIcEESaIS1_EE18_M_fill_initializeEmRKS1_,comdat
	.align 2
	.weak	_ZNSt6vectorIS_IcSaIcEESaIS1_EE18_M_fill_initializeEmRKS1_
	.type	_ZNSt6vectorIS_IcSaIcEESaIS1_EE18_M_fill_initializeEmRKS1_, @function
_ZNSt6vectorIS_IcSaIcEESaIS1_EE18_M_fill_initializeEmRKS1_:
.LFB2490:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE19_M_get_Tp_allocatorEv
	mov	rcx, rax
	mov	rax, QWORD PTR -8[rbp]
	mov	rax, QWORD PTR [rax]
	mov	rdx, QWORD PTR -24[rbp]
	mov	rsi, QWORD PTR -16[rbp]
	mov	rdi, rax
	call	_ZSt24__uninitialized_fill_n_aIPSt6vectorIcSaIcEEmS2_S2_ET_S4_T0_RKT1_RSaIT2_E
	mov	rdx, QWORD PTR -8[rbp]
	mov	QWORD PTR 8[rdx], rax
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2490:
	.size	_ZNSt6vectorIS_IcSaIcEESaIS1_EE18_M_fill_initializeEmRKS1_, .-_ZNSt6vectorIS_IcSaIcEESaIS1_EE18_M_fill_initializeEmRKS1_
	.section	.text._ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE19_M_get_Tp_allocatorEv,"axG",@progbits,_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE19_M_get_Tp_allocatorEv,comdat
	.align 2
	.weak	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE19_M_get_Tp_allocatorEv
	.type	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE19_M_get_Tp_allocatorEv, @function
_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE19_M_get_Tp_allocatorEv:
.LFB2491:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2491:
	.size	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE19_M_get_Tp_allocatorEv, .-_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE19_M_get_Tp_allocatorEv
	.section	.text._ZSt8_DestroyIPSt6vectorIcSaIcEES2_EvT_S4_RSaIT0_E,"axG",@progbits,_ZSt8_DestroyIPSt6vectorIcSaIcEES2_EvT_S4_RSaIT0_E,comdat
	.weak	_ZSt8_DestroyIPSt6vectorIcSaIcEES2_EvT_S4_RSaIT0_E
	.type	_ZSt8_DestroyIPSt6vectorIcSaIcEES2_EvT_S4_RSaIT0_E, @function
_ZSt8_DestroyIPSt6vectorIcSaIcEES2_EvT_S4_RSaIT0_E:
.LFB2492:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	mov	rdx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZSt8_DestroyIPSt6vectorIcSaIcEEEvT_S4_
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2492:
	.size	_ZSt8_DestroyIPSt6vectorIcSaIcEES2_EvT_S4_RSaIT0_E, .-_ZSt8_DestroyIPSt6vectorIcSaIcEES2_EvT_S4_RSaIT0_E
	.section	.text._ZN9__gnu_cxx14__alloc_traitsISaIcEcE17_S_select_on_copyERKS1_,"axG",@progbits,_ZN9__gnu_cxx14__alloc_traitsISaIcEcE17_S_select_on_copyERKS1_,comdat
	.weak	_ZN9__gnu_cxx14__alloc_traitsISaIcEcE17_S_select_on_copyERKS1_
	.type	_ZN9__gnu_cxx14__alloc_traitsISaIcEcE17_S_select_on_copyERKS1_, @function
_ZN9__gnu_cxx14__alloc_traitsISaIcEcE17_S_select_on_copyERKS1_:
.LFB2526:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -24[rbp], rdi
	mov	QWORD PTR -32[rbp], rsi
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR -8[rbp], rax
	xor	eax, eax
	mov	rax, QWORD PTR -24[rbp]
	mov	rdx, QWORD PTR -32[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSt16allocator_traitsISaIcEE37select_on_container_copy_constructionERKS0_
	mov	rax, QWORD PTR -8[rbp]
	sub	rax, QWORD PTR fs:40
	je	.L134
	call	__stack_chk_fail@PLT
.L134:
	mov	rax, QWORD PTR -24[rbp]
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2526:
	.size	_ZN9__gnu_cxx14__alloc_traitsISaIcEcE17_S_select_on_copyERKS1_, .-_ZN9__gnu_cxx14__alloc_traitsISaIcEcE17_S_select_on_copyERKS1_
	.section	.text._ZNSt6vectorIcSaIcEE11_S_max_sizeERKS0_,"axG",@progbits,_ZNSt6vectorIcSaIcEE11_S_max_sizeERKS0_,comdat
	.weak	_ZNSt6vectorIcSaIcEE11_S_max_sizeERKS0_
	.type	_ZNSt6vectorIcSaIcEE11_S_max_sizeERKS0_, @function
_ZNSt6vectorIcSaIcEE11_S_max_sizeERKS0_:
.LFB2530:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 48
	mov	QWORD PTR -40[rbp], rdi
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR -8[rbp], rax
	xor	eax, eax
	movabs	rax, 9223372036854775807
	mov	QWORD PTR -24[rbp], rax
	mov	rax, QWORD PTR -40[rbp]
	mov	rdi, rax
	call	_ZNSt16allocator_traitsISaIcEE8max_sizeERKS0_
	mov	QWORD PTR -16[rbp], rax
	lea	rdx, -16[rbp]
	lea	rax, -24[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZSt3minImERKT_S2_S2_
	mov	rax, QWORD PTR [rax]
	mov	rdx, QWORD PTR -8[rbp]
	sub	rdx, QWORD PTR fs:40
	je	.L137
	call	__stack_chk_fail@PLT
.L137:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2530:
	.size	_ZNSt6vectorIcSaIcEE11_S_max_sizeERKS0_, .-_ZNSt6vectorIcSaIcEE11_S_max_sizeERKS0_
	.section	.text._ZNSt12_Vector_baseIcSaIcEE12_Vector_implC2ERKS0_,"axG",@progbits,_ZNSt12_Vector_baseIcSaIcEE12_Vector_implC5ERKS0_,comdat
	.align 2
	.weak	_ZNSt12_Vector_baseIcSaIcEE12_Vector_implC2ERKS0_
	.type	_ZNSt12_Vector_baseIcSaIcEE12_Vector_implC2ERKS0_, @function
_ZNSt12_Vector_baseIcSaIcEE12_Vector_implC2ERKS0_:
.LFB2532:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	rdx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSaIcEC2ERKS_@PLT
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNSt12_Vector_baseIcSaIcEE17_Vector_impl_dataC2Ev
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2532:
	.size	_ZNSt12_Vector_baseIcSaIcEE12_Vector_implC2ERKS0_, .-_ZNSt12_Vector_baseIcSaIcEE12_Vector_implC2ERKS0_
	.weak	_ZNSt12_Vector_baseIcSaIcEE12_Vector_implC1ERKS0_
	.set	_ZNSt12_Vector_baseIcSaIcEE12_Vector_implC1ERKS0_,_ZNSt12_Vector_baseIcSaIcEE12_Vector_implC2ERKS0_
	.section	.text._ZNSt12_Vector_baseIcSaIcEE17_M_create_storageEm,"axG",@progbits,_ZNSt12_Vector_baseIcSaIcEE17_M_create_storageEm,comdat
	.align 2
	.weak	_ZNSt12_Vector_baseIcSaIcEE17_M_create_storageEm
	.type	_ZNSt12_Vector_baseIcSaIcEE17_M_create_storageEm, @function
_ZNSt12_Vector_baseIcSaIcEE17_M_create_storageEm:
.LFB2534:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	rdx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSt12_Vector_baseIcSaIcEE11_M_allocateEm
	mov	rdx, QWORD PTR -8[rbp]
	mov	QWORD PTR [rdx], rax
	mov	rax, QWORD PTR -8[rbp]
	mov	rdx, QWORD PTR [rax]
	mov	rax, QWORD PTR -8[rbp]
	mov	QWORD PTR 8[rax], rdx
	mov	rax, QWORD PTR -8[rbp]
	mov	rdx, QWORD PTR [rax]
	mov	rax, QWORD PTR -16[rbp]
	add	rdx, rax
	mov	rax, QWORD PTR -8[rbp]
	mov	QWORD PTR 16[rax], rdx
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2534:
	.size	_ZNSt12_Vector_baseIcSaIcEE17_M_create_storageEm, .-_ZNSt12_Vector_baseIcSaIcEE17_M_create_storageEm
	.section	.text._ZNSt12_Vector_baseIcSaIcEE13_M_deallocateEPcm,"axG",@progbits,_ZNSt12_Vector_baseIcSaIcEE13_M_deallocateEPcm,comdat
	.align 2
	.weak	_ZNSt12_Vector_baseIcSaIcEE13_M_deallocateEPcm
	.type	_ZNSt12_Vector_baseIcSaIcEE13_M_deallocateEPcm, @function
_ZNSt12_Vector_baseIcSaIcEE13_M_deallocateEPcm:
.LFB2535:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	cmp	QWORD PTR -16[rbp], 0
	je	.L142
	mov	rax, QWORD PTR -8[rbp]
	mov	rdx, QWORD PTR -24[rbp]
	mov	rcx, QWORD PTR -16[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZNSt16allocator_traitsISaIcEE10deallocateERS0_Pcm
.L142:
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2535:
	.size	_ZNSt12_Vector_baseIcSaIcEE13_M_deallocateEPcm, .-_ZNSt12_Vector_baseIcSaIcEE13_M_deallocateEPcm
	.section	.text._ZSt24__uninitialized_fill_n_aIPcmccET_S1_T0_RKT1_RSaIT2_E,"axG",@progbits,_ZSt24__uninitialized_fill_n_aIPcmccET_S1_T0_RKT1_RSaIT2_E,comdat
	.weak	_ZSt24__uninitialized_fill_n_aIPcmccET_S1_T0_RKT1_RSaIT2_E
	.type	_ZSt24__uninitialized_fill_n_aIPcmccET_S1_T0_RKT1_RSaIT2_E, @function
_ZSt24__uninitialized_fill_n_aIPcmccET_S1_T0_RKT1_RSaIT2_E:
.LFB2536:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	mov	QWORD PTR -32[rbp], rcx
	mov	rdx, QWORD PTR -24[rbp]
	mov	rcx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZSt20uninitialized_fill_nIPcmcET_S1_T0_RKT1_
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2536:
	.size	_ZSt24__uninitialized_fill_n_aIPcmccET_S1_T0_RKT1_RSaIT2_E, .-_ZSt24__uninitialized_fill_n_aIPcmccET_S1_T0_RKT1_RSaIT2_E
	.section	.text._ZSt8_DestroyIPcEvT_S1_,"axG",@progbits,_ZSt8_DestroyIPcEvT_S1_,comdat
	.weak	_ZSt8_DestroyIPcEvT_S1_
	.type	_ZSt8_DestroyIPcEvT_S1_, @function
_ZSt8_DestroyIPcEvT_S1_:
.LFB2537:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	rdx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSt12_Destroy_auxILb1EE9__destroyIPcEEvT_S3_
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2537:
	.size	_ZSt8_DestroyIPcEvT_S1_, .-_ZSt8_DestroyIPcEvT_S1_
	.section	.text._ZNSt6vectorIS_IcSaIcEESaIS1_EE11_S_max_sizeERKS2_,"axG",@progbits,_ZNSt6vectorIS_IcSaIcEESaIS1_EE11_S_max_sizeERKS2_,comdat
	.weak	_ZNSt6vectorIS_IcSaIcEESaIS1_EE11_S_max_sizeERKS2_
	.type	_ZNSt6vectorIS_IcSaIcEESaIS1_EE11_S_max_sizeERKS2_, @function
_ZNSt6vectorIS_IcSaIcEESaIS1_EE11_S_max_sizeERKS2_:
.LFB2538:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 48
	mov	QWORD PTR -40[rbp], rdi
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR -8[rbp], rax
	xor	eax, eax
	movabs	rax, 384307168202282325
	mov	QWORD PTR -24[rbp], rax
	mov	rax, QWORD PTR -40[rbp]
	mov	rdi, rax
	call	_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE8max_sizeERKS3_
	mov	QWORD PTR -16[rbp], rax
	lea	rdx, -16[rbp]
	lea	rax, -24[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZSt3minImERKT_S2_S2_
	mov	rax, QWORD PTR [rax]
	mov	rdx, QWORD PTR -8[rbp]
	sub	rdx, QWORD PTR fs:40
	je	.L148
	call	__stack_chk_fail@PLT
.L148:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2538:
	.size	_ZNSt6vectorIS_IcSaIcEESaIS1_EE11_S_max_sizeERKS2_, .-_ZNSt6vectorIS_IcSaIcEESaIS1_EE11_S_max_sizeERKS2_
	.section	.text._ZNSaISt6vectorIcSaIcEEEC2ERKS2_,"axG",@progbits,_ZNSaISt6vectorIcSaIcEEEC5ERKS2_,comdat
	.align 2
	.weak	_ZNSaISt6vectorIcSaIcEEEC2ERKS2_
	.type	_ZNSaISt6vectorIcSaIcEEEC2ERKS2_, @function
_ZNSaISt6vectorIcSaIcEEEC2ERKS2_:
.LFB2540:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	rdx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC2ERKS4_
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2540:
	.size	_ZNSaISt6vectorIcSaIcEEEC2ERKS2_, .-_ZNSaISt6vectorIcSaIcEEEC2ERKS2_
	.weak	_ZNSaISt6vectorIcSaIcEEEC1ERKS2_
	.set	_ZNSaISt6vectorIcSaIcEEEC1ERKS2_,_ZNSaISt6vectorIcSaIcEEEC2ERKS2_
	.section	.text._ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implC2ERKS3_,"axG",@progbits,_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implC5ERKS3_,comdat
	.align 2
	.weak	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implC2ERKS3_
	.type	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implC2ERKS3_, @function
_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implC2ERKS3_:
.LFB2543:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	rdx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSaISt6vectorIcSaIcEEEC2ERKS2_
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_Vector_impl_dataC2Ev
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2543:
	.size	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implC2ERKS3_, .-_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implC2ERKS3_
	.weak	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implC1ERKS3_
	.set	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implC1ERKS3_,_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE12_Vector_implC2ERKS3_
	.section	.text._ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_M_create_storageEm,"axG",@progbits,_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_M_create_storageEm,comdat
	.align 2
	.weak	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_M_create_storageEm
	.type	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_M_create_storageEm, @function
_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_M_create_storageEm:
.LFB2545:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	rdx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE11_M_allocateEm
	mov	rdx, QWORD PTR -8[rbp]
	mov	QWORD PTR [rdx], rax
	mov	rax, QWORD PTR -8[rbp]
	mov	rdx, QWORD PTR [rax]
	mov	rax, QWORD PTR -8[rbp]
	mov	QWORD PTR 8[rax], rdx
	mov	rax, QWORD PTR -8[rbp]
	mov	rcx, QWORD PTR [rax]
	mov	rdx, QWORD PTR -16[rbp]
	mov	rax, rdx
	add	rax, rax
	add	rax, rdx
	sal	rax, 3
	lea	rdx, [rcx+rax]
	mov	rax, QWORD PTR -8[rbp]
	mov	QWORD PTR 16[rax], rdx
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2545:
	.size	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_M_create_storageEm, .-_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_M_create_storageEm
	.section	.text._ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE13_M_deallocateEPS2_m,"axG",@progbits,_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE13_M_deallocateEPS2_m,comdat
	.align 2
	.weak	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE13_M_deallocateEPS2_m
	.type	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE13_M_deallocateEPS2_m, @function
_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE13_M_deallocateEPS2_m:
.LFB2546:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	cmp	QWORD PTR -16[rbp], 0
	je	.L154
	mov	rax, QWORD PTR -8[rbp]
	mov	rdx, QWORD PTR -24[rbp]
	mov	rcx, QWORD PTR -16[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE10deallocateERS3_PS2_m
.L154:
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2546:
	.size	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE13_M_deallocateEPS2_m, .-_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE13_M_deallocateEPS2_m
	.section	.text._ZSt24__uninitialized_fill_n_aIPSt6vectorIcSaIcEEmS2_S2_ET_S4_T0_RKT1_RSaIT2_E,"axG",@progbits,_ZSt24__uninitialized_fill_n_aIPSt6vectorIcSaIcEEmS2_S2_ET_S4_T0_RKT1_RSaIT2_E,comdat
	.weak	_ZSt24__uninitialized_fill_n_aIPSt6vectorIcSaIcEEmS2_S2_ET_S4_T0_RKT1_RSaIT2_E
	.type	_ZSt24__uninitialized_fill_n_aIPSt6vectorIcSaIcEEmS2_S2_ET_S4_T0_RKT1_RSaIT2_E, @function
_ZSt24__uninitialized_fill_n_aIPSt6vectorIcSaIcEEmS2_S2_ET_S4_T0_RKT1_RSaIT2_E:
.LFB2547:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	mov	QWORD PTR -32[rbp], rcx
	mov	rdx, QWORD PTR -24[rbp]
	mov	rcx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZSt20uninitialized_fill_nIPSt6vectorIcSaIcEEmS2_ET_S4_T0_RKT1_
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2547:
	.size	_ZSt24__uninitialized_fill_n_aIPSt6vectorIcSaIcEEmS2_S2_ET_S4_T0_RKT1_RSaIT2_E, .-_ZSt24__uninitialized_fill_n_aIPSt6vectorIcSaIcEEmS2_S2_ET_S4_T0_RKT1_RSaIT2_E
	.section	.text._ZSt8_DestroyIPSt6vectorIcSaIcEEEvT_S4_,"axG",@progbits,_ZSt8_DestroyIPSt6vectorIcSaIcEEEvT_S4_,comdat
	.weak	_ZSt8_DestroyIPSt6vectorIcSaIcEEEvT_S4_
	.type	_ZSt8_DestroyIPSt6vectorIcSaIcEEEvT_S4_, @function
_ZSt8_DestroyIPSt6vectorIcSaIcEEEvT_S4_:
.LFB2548:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	rdx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSt12_Destroy_auxILb0EE9__destroyIPSt6vectorIcSaIcEEEEvT_S6_
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2548:
	.size	_ZSt8_DestroyIPSt6vectorIcSaIcEEEvT_S4_, .-_ZSt8_DestroyIPSt6vectorIcSaIcEEEvT_S4_
	.section	.text._ZNSt16allocator_traitsISaIcEE10deallocateERS0_Pcm,"axG",@progbits,_ZNSt16allocator_traitsISaIcEE10deallocateERS0_Pcm,comdat
	.weak	_ZNSt16allocator_traitsISaIcEE10deallocateERS0_Pcm
	.type	_ZNSt16allocator_traitsISaIcEE10deallocateERS0_Pcm, @function
_ZNSt16allocator_traitsISaIcEE10deallocateERS0_Pcm:
.LFB2556:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	mov	rdx, QWORD PTR -24[rbp]
	mov	rcx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZN9__gnu_cxx13new_allocatorIcE10deallocateEPcm
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2556:
	.size	_ZNSt16allocator_traitsISaIcEE10deallocateERS0_Pcm, .-_ZNSt16allocator_traitsISaIcEE10deallocateERS0_Pcm
	.section	.text._ZNSt16allocator_traitsISaIcEE37select_on_container_copy_constructionERKS0_,"axG",@progbits,_ZNSt16allocator_traitsISaIcEE37select_on_container_copy_constructionERKS0_,comdat
	.weak	_ZNSt16allocator_traitsISaIcEE37select_on_container_copy_constructionERKS0_
	.type	_ZNSt16allocator_traitsISaIcEE37select_on_container_copy_constructionERKS0_, @function
_ZNSt16allocator_traitsISaIcEE37select_on_container_copy_constructionERKS0_:
.LFB2589:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	rdx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSaIcEC1ERKS_@PLT
	mov	rax, QWORD PTR -8[rbp]
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2589:
	.size	_ZNSt16allocator_traitsISaIcEE37select_on_container_copy_constructionERKS0_, .-_ZNSt16allocator_traitsISaIcEE37select_on_container_copy_constructionERKS0_
	.section	.text._ZNSt16allocator_traitsISaIcEE8max_sizeERKS0_,"axG",@progbits,_ZNSt16allocator_traitsISaIcEE8max_sizeERKS0_,comdat
	.weak	_ZNSt16allocator_traitsISaIcEE8max_sizeERKS0_
	.type	_ZNSt16allocator_traitsISaIcEE8max_sizeERKS0_, @function
_ZNSt16allocator_traitsISaIcEE8max_sizeERKS0_:
.LFB2593:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNK9__gnu_cxx13new_allocatorIcE8max_sizeEv
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2593:
	.size	_ZNSt16allocator_traitsISaIcEE8max_sizeERKS0_, .-_ZNSt16allocator_traitsISaIcEE8max_sizeERKS0_
	.section	.text._ZSt3minImERKT_S2_S2_,"axG",@progbits,_ZSt3minImERKT_S2_S2_,comdat
	.weak	_ZSt3minImERKT_S2_S2_
	.type	_ZSt3minImERKT_S2_S2_, @function
_ZSt3minImERKT_S2_S2_:
.LFB2594:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	rax, QWORD PTR -16[rbp]
	mov	rdx, QWORD PTR [rax]
	mov	rax, QWORD PTR -8[rbp]
	mov	rax, QWORD PTR [rax]
	cmp	rdx, rax
	jnb	.L164
	mov	rax, QWORD PTR -16[rbp]
	jmp	.L165
.L164:
	mov	rax, QWORD PTR -8[rbp]
.L165:
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2594:
	.size	_ZSt3minImERKT_S2_S2_, .-_ZSt3minImERKT_S2_S2_
	.section	.text._ZNSt12_Vector_baseIcSaIcEE17_Vector_impl_dataC2Ev,"axG",@progbits,_ZNSt12_Vector_baseIcSaIcEE17_Vector_impl_dataC5Ev,comdat
	.align 2
	.weak	_ZNSt12_Vector_baseIcSaIcEE17_Vector_impl_dataC2Ev
	.type	_ZNSt12_Vector_baseIcSaIcEE17_Vector_impl_dataC2Ev, @function
_ZNSt12_Vector_baseIcSaIcEE17_Vector_impl_dataC2Ev:
.LFB2596:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	mov	QWORD PTR [rax], 0
	mov	rax, QWORD PTR -8[rbp]
	mov	QWORD PTR 8[rax], 0
	mov	rax, QWORD PTR -8[rbp]
	mov	QWORD PTR 16[rax], 0
	nop
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2596:
	.size	_ZNSt12_Vector_baseIcSaIcEE17_Vector_impl_dataC2Ev, .-_ZNSt12_Vector_baseIcSaIcEE17_Vector_impl_dataC2Ev
	.weak	_ZNSt12_Vector_baseIcSaIcEE17_Vector_impl_dataC1Ev
	.set	_ZNSt12_Vector_baseIcSaIcEE17_Vector_impl_dataC1Ev,_ZNSt12_Vector_baseIcSaIcEE17_Vector_impl_dataC2Ev
	.section	.text._ZNSt12_Vector_baseIcSaIcEE11_M_allocateEm,"axG",@progbits,_ZNSt12_Vector_baseIcSaIcEE11_M_allocateEm,comdat
	.align 2
	.weak	_ZNSt12_Vector_baseIcSaIcEE11_M_allocateEm
	.type	_ZNSt12_Vector_baseIcSaIcEE11_M_allocateEm, @function
_ZNSt12_Vector_baseIcSaIcEE11_M_allocateEm:
.LFB2598:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	cmp	QWORD PTR -16[rbp], 0
	je	.L168
	mov	rax, QWORD PTR -8[rbp]
	mov	rdx, QWORD PTR -16[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSt16allocator_traitsISaIcEE8allocateERS0_m
	jmp	.L170
.L168:
	mov	eax, 0
.L170:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2598:
	.size	_ZNSt12_Vector_baseIcSaIcEE11_M_allocateEm, .-_ZNSt12_Vector_baseIcSaIcEE11_M_allocateEm
	.section	.text._ZSt20uninitialized_fill_nIPcmcET_S1_T0_RKT1_,"axG",@progbits,_ZSt20uninitialized_fill_nIPcmcET_S1_T0_RKT1_,comdat
	.weak	_ZSt20uninitialized_fill_nIPcmcET_S1_T0_RKT1_
	.type	_ZSt20uninitialized_fill_nIPcmcET_S1_T0_RKT1_, @function
_ZSt20uninitialized_fill_nIPcmcET_S1_T0_RKT1_:
.LFB2599:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 48
	mov	QWORD PTR -24[rbp], rdi
	mov	QWORD PTR -32[rbp], rsi
	mov	QWORD PTR -40[rbp], rdx
	mov	BYTE PTR -1[rbp], 1
	mov	rdx, QWORD PTR -40[rbp]
	mov	rcx, QWORD PTR -32[rbp]
	mov	rax, QWORD PTR -24[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZNSt22__uninitialized_fill_nILb1EE15__uninit_fill_nIPcmcEET_S3_T0_RKT1_
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2599:
	.size	_ZSt20uninitialized_fill_nIPcmcET_S1_T0_RKT1_, .-_ZSt20uninitialized_fill_nIPcmcET_S1_T0_RKT1_
	.section	.text._ZNSt12_Destroy_auxILb1EE9__destroyIPcEEvT_S3_,"axG",@progbits,_ZNSt12_Destroy_auxILb1EE9__destroyIPcEEvT_S3_,comdat
	.weak	_ZNSt12_Destroy_auxILb1EE9__destroyIPcEEvT_S3_
	.type	_ZNSt12_Destroy_auxILb1EE9__destroyIPcEEvT_S3_, @function
_ZNSt12_Destroy_auxILb1EE9__destroyIPcEEvT_S3_:
.LFB2600:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	nop
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2600:
	.size	_ZNSt12_Destroy_auxILb1EE9__destroyIPcEEvT_S3_, .-_ZNSt12_Destroy_auxILb1EE9__destroyIPcEEvT_S3_
	.section	.text._ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE8max_sizeERKS3_,"axG",@progbits,_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE8max_sizeERKS3_,comdat
	.weak	_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE8max_sizeERKS3_
	.type	_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE8max_sizeERKS3_, @function
_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE8max_sizeERKS3_:
.LFB2601:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNK9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE8max_sizeEv
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2601:
	.size	_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE8max_sizeERKS3_, .-_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE8max_sizeERKS3_
	.section	.text._ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC2ERKS4_,"axG",@progbits,_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC5ERKS4_,comdat
	.align 2
	.weak	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC2ERKS4_
	.type	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC2ERKS4_, @function
_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC2ERKS4_:
.LFB2603:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	nop
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2603:
	.size	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC2ERKS4_, .-_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC2ERKS4_
	.weak	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC1ERKS4_
	.set	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC1ERKS4_,_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEEC2ERKS4_
	.section	.text._ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_Vector_impl_dataC2Ev,"axG",@progbits,_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_Vector_impl_dataC5Ev,comdat
	.align 2
	.weak	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_Vector_impl_dataC2Ev
	.type	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_Vector_impl_dataC2Ev, @function
_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_Vector_impl_dataC2Ev:
.LFB2606:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	mov	QWORD PTR [rax], 0
	mov	rax, QWORD PTR -8[rbp]
	mov	QWORD PTR 8[rax], 0
	mov	rax, QWORD PTR -8[rbp]
	mov	QWORD PTR 16[rax], 0
	nop
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2606:
	.size	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_Vector_impl_dataC2Ev, .-_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_Vector_impl_dataC2Ev
	.weak	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_Vector_impl_dataC1Ev
	.set	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_Vector_impl_dataC1Ev,_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE17_Vector_impl_dataC2Ev
	.section	.text._ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE11_M_allocateEm,"axG",@progbits,_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE11_M_allocateEm,comdat
	.align 2
	.weak	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE11_M_allocateEm
	.type	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE11_M_allocateEm, @function
_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE11_M_allocateEm:
.LFB2608:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	cmp	QWORD PTR -16[rbp], 0
	je	.L179
	mov	rax, QWORD PTR -8[rbp]
	mov	rdx, QWORD PTR -16[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE8allocateERS3_m
	jmp	.L181
.L179:
	mov	eax, 0
.L181:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2608:
	.size	_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE11_M_allocateEm, .-_ZNSt12_Vector_baseISt6vectorIcSaIcEESaIS2_EE11_M_allocateEm
	.section	.text._ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE10deallocateERS3_PS2_m,"axG",@progbits,_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE10deallocateERS3_PS2_m,comdat
	.weak	_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE10deallocateERS3_PS2_m
	.type	_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE10deallocateERS3_PS2_m, @function
_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE10deallocateERS3_PS2_m:
.LFB2609:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	mov	rdx, QWORD PTR -24[rbp]
	mov	rcx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE10deallocateEPS3_m
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2609:
	.size	_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE10deallocateERS3_PS2_m, .-_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE10deallocateERS3_PS2_m
	.section	.text._ZSt20uninitialized_fill_nIPSt6vectorIcSaIcEEmS2_ET_S4_T0_RKT1_,"axG",@progbits,_ZSt20uninitialized_fill_nIPSt6vectorIcSaIcEEmS2_ET_S4_T0_RKT1_,comdat
	.weak	_ZSt20uninitialized_fill_nIPSt6vectorIcSaIcEEmS2_ET_S4_T0_RKT1_
	.type	_ZSt20uninitialized_fill_nIPSt6vectorIcSaIcEEmS2_ET_S4_T0_RKT1_, @function
_ZSt20uninitialized_fill_nIPSt6vectorIcSaIcEEmS2_ET_S4_T0_RKT1_:
.LFB2610:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 48
	mov	QWORD PTR -24[rbp], rdi
	mov	QWORD PTR -32[rbp], rsi
	mov	QWORD PTR -40[rbp], rdx
	mov	BYTE PTR -1[rbp], 1
	mov	rdx, QWORD PTR -40[rbp]
	mov	rcx, QWORD PTR -32[rbp]
	mov	rax, QWORD PTR -24[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZNSt22__uninitialized_fill_nILb0EE15__uninit_fill_nIPSt6vectorIcSaIcEEmS4_EET_S6_T0_RKT1_
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2610:
	.size	_ZSt20uninitialized_fill_nIPSt6vectorIcSaIcEEmS2_ET_S4_T0_RKT1_, .-_ZSt20uninitialized_fill_nIPSt6vectorIcSaIcEEmS2_ET_S4_T0_RKT1_
	.section	.text._ZNSt12_Destroy_auxILb0EE9__destroyIPSt6vectorIcSaIcEEEEvT_S6_,"axG",@progbits,_ZNSt12_Destroy_auxILb0EE9__destroyIPSt6vectorIcSaIcEEEEvT_S6_,comdat
	.weak	_ZNSt12_Destroy_auxILb0EE9__destroyIPSt6vectorIcSaIcEEEEvT_S6_
	.type	_ZNSt12_Destroy_auxILb0EE9__destroyIPSt6vectorIcSaIcEEEEvT_S6_, @function
_ZNSt12_Destroy_auxILb0EE9__destroyIPSt6vectorIcSaIcEEEEvT_S6_:
.LFB2611:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	jmp	.L186
.L187:
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZSt11__addressofISt6vectorIcSaIcEEEPT_RS3_
	mov	rdi, rax
	call	_ZSt8_DestroyISt6vectorIcSaIcEEEvPT_
	add	QWORD PTR -8[rbp], 24
.L186:
	mov	rax, QWORD PTR -8[rbp]
	cmp	rax, QWORD PTR -16[rbp]
	jne	.L187
	nop
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2611:
	.size	_ZNSt12_Destroy_auxILb0EE9__destroyIPSt6vectorIcSaIcEEEEvT_S6_, .-_ZNSt12_Destroy_auxILb0EE9__destroyIPSt6vectorIcSaIcEEEEvT_S6_
	.section	.text._ZN9__gnu_cxx13new_allocatorIcE10deallocateEPcm,"axG",@progbits,_ZN9__gnu_cxx13new_allocatorIcE10deallocateEPcm,comdat
	.align 2
	.weak	_ZN9__gnu_cxx13new_allocatorIcE10deallocateEPcm
	.type	_ZN9__gnu_cxx13new_allocatorIcE10deallocateEPcm, @function
_ZN9__gnu_cxx13new_allocatorIcE10deallocateEPcm:
.LFB2613:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	mov	rdx, QWORD PTR -24[rbp]
	mov	rax, QWORD PTR -16[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZdlPvm@PLT
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2613:
	.size	_ZN9__gnu_cxx13new_allocatorIcE10deallocateEPcm, .-_ZN9__gnu_cxx13new_allocatorIcE10deallocateEPcm
	.section	.text._ZNK9__gnu_cxx13new_allocatorIcE8max_sizeEv,"axG",@progbits,_ZNK9__gnu_cxx13new_allocatorIcE8max_sizeEv,comdat
	.align 2
	.weak	_ZNK9__gnu_cxx13new_allocatorIcE8max_sizeEv
	.type	_ZNK9__gnu_cxx13new_allocatorIcE8max_sizeEv, @function
_ZNK9__gnu_cxx13new_allocatorIcE8max_sizeEv:
.LFB2655:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNK9__gnu_cxx13new_allocatorIcE11_M_max_sizeEv
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2655:
	.size	_ZNK9__gnu_cxx13new_allocatorIcE8max_sizeEv, .-_ZNK9__gnu_cxx13new_allocatorIcE8max_sizeEv
	.section	.text._ZNSt16allocator_traitsISaIcEE8allocateERS0_m,"axG",@progbits,_ZNSt16allocator_traitsISaIcEE8allocateERS0_m,comdat
	.weak	_ZNSt16allocator_traitsISaIcEE8allocateERS0_m
	.type	_ZNSt16allocator_traitsISaIcEE8allocateERS0_m, @function
_ZNSt16allocator_traitsISaIcEE8allocateERS0_m:
.LFB2656:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	rcx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	edx, 0
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZN9__gnu_cxx13new_allocatorIcE8allocateEmPKv
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2656:
	.size	_ZNSt16allocator_traitsISaIcEE8allocateERS0_m, .-_ZNSt16allocator_traitsISaIcEE8allocateERS0_m
	.section	.text._ZNSt22__uninitialized_fill_nILb1EE15__uninit_fill_nIPcmcEET_S3_T0_RKT1_,"axG",@progbits,_ZNSt22__uninitialized_fill_nILb1EE15__uninit_fill_nIPcmcEET_S3_T0_RKT1_,comdat
	.weak	_ZNSt22__uninitialized_fill_nILb1EE15__uninit_fill_nIPcmcEET_S3_T0_RKT1_
	.type	_ZNSt22__uninitialized_fill_nILb1EE15__uninit_fill_nIPcmcEET_S3_T0_RKT1_, @function
_ZNSt22__uninitialized_fill_nILb1EE15__uninit_fill_nIPcmcEET_S3_T0_RKT1_:
.LFB2657:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	mov	rdx, QWORD PTR -24[rbp]
	mov	rcx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZSt6fill_nIPcmcET_S1_T0_RKT1_
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2657:
	.size	_ZNSt22__uninitialized_fill_nILb1EE15__uninit_fill_nIPcmcEET_S3_T0_RKT1_, .-_ZNSt22__uninitialized_fill_nILb1EE15__uninit_fill_nIPcmcEET_S3_T0_RKT1_
	.section	.text._ZNK9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE8max_sizeEv,"axG",@progbits,_ZNK9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE8max_sizeEv,comdat
	.align 2
	.weak	_ZNK9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE8max_sizeEv
	.type	_ZNK9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE8max_sizeEv, @function
_ZNK9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE8max_sizeEv:
.LFB2658:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNK9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE11_M_max_sizeEv
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2658:
	.size	_ZNK9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE8max_sizeEv, .-_ZNK9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE8max_sizeEv
	.section	.text._ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE8allocateERS3_m,"axG",@progbits,_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE8allocateERS3_m,comdat
	.weak	_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE8allocateERS3_m
	.type	_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE8allocateERS3_m, @function
_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE8allocateERS3_m:
.LFB2659:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	rcx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	edx, 0
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE8allocateEmPKv
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2659:
	.size	_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE8allocateERS3_m, .-_ZNSt16allocator_traitsISaISt6vectorIcSaIcEEEE8allocateERS3_m
	.section	.text._ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE10deallocateEPS3_m,"axG",@progbits,_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE10deallocateEPS3_m,comdat
	.align 2
	.weak	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE10deallocateEPS3_m
	.type	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE10deallocateEPS3_m, @function
_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE10deallocateEPS3_m:
.LFB2660:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	mov	rdx, QWORD PTR -24[rbp]
	mov	rax, rdx
	add	rax, rax
	add	rax, rdx
	sal	rax, 3
	mov	rdx, rax
	mov	rax, QWORD PTR -16[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZdlPvm@PLT
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2660:
	.size	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE10deallocateEPS3_m, .-_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE10deallocateEPS3_m
	.section	.text._ZNSt22__uninitialized_fill_nILb0EE15__uninit_fill_nIPSt6vectorIcSaIcEEmS4_EET_S6_T0_RKT1_,"axG",@progbits,_ZNSt22__uninitialized_fill_nILb0EE15__uninit_fill_nIPSt6vectorIcSaIcEEmS4_EET_S6_T0_RKT1_,comdat
	.weak	_ZNSt22__uninitialized_fill_nILb0EE15__uninit_fill_nIPSt6vectorIcSaIcEEmS4_EET_S6_T0_RKT1_
	.type	_ZNSt22__uninitialized_fill_nILb0EE15__uninit_fill_nIPSt6vectorIcSaIcEEmS4_EET_S6_T0_RKT1_, @function
_ZNSt22__uninitialized_fill_nILb0EE15__uninit_fill_nIPSt6vectorIcSaIcEEmS4_EET_S6_T0_RKT1_:
.LFB2661:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA2661
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	push	rbx
	sub	rsp, 56
	.cfi_offset 3, -24
	mov	QWORD PTR -40[rbp], rdi
	mov	QWORD PTR -48[rbp], rsi
	mov	QWORD PTR -56[rbp], rdx
	mov	rax, QWORD PTR -40[rbp]
	mov	QWORD PTR -24[rbp], rax
	jmp	.L203
.L204:
	mov	rax, QWORD PTR -24[rbp]
	mov	rdi, rax
	call	_ZSt11__addressofISt6vectorIcSaIcEEEPT_RS3_
	mov	rdx, rax
	mov	rax, QWORD PTR -56[rbp]
	mov	rsi, rax
	mov	rdi, rdx
.LEHB16:
	call	_ZSt10_ConstructISt6vectorIcSaIcEEJRKS2_EEvPT_DpOT0_
.LEHE16:
	sub	QWORD PTR -48[rbp], 1
	add	QWORD PTR -24[rbp], 24
.L203:
	cmp	QWORD PTR -48[rbp], 0
	jne	.L204
	mov	rax, QWORD PTR -24[rbp]
	jmp	.L210
.L208:
	endbr64
	mov	rdi, rax
	call	__cxa_begin_catch@PLT
	mov	rdx, QWORD PTR -24[rbp]
	mov	rax, QWORD PTR -40[rbp]
	mov	rsi, rdx
	mov	rdi, rax
.LEHB17:
	call	_ZSt8_DestroyIPSt6vectorIcSaIcEEEvT_S4_
	call	__cxa_rethrow@PLT
.LEHE17:
.L209:
	endbr64
	mov	rbx, rax
	call	__cxa_end_catch@PLT
	mov	rax, rbx
	mov	rdi, rax
.LEHB18:
	call	_Unwind_Resume@PLT
.LEHE18:
.L210:
	mov	rbx, QWORD PTR -8[rbp]
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2661:
	.section	.gcc_except_table
	.align 4
.LLSDA2661:
	.byte	0xff
	.byte	0x9b
	.uleb128 .LLSDATT2661-.LLSDATTD2661
.LLSDATTD2661:
	.byte	0x1
	.uleb128 .LLSDACSE2661-.LLSDACSB2661
.LLSDACSB2661:
	.uleb128 .LEHB16-.LFB2661
	.uleb128 .LEHE16-.LEHB16
	.uleb128 .L208-.LFB2661
	.uleb128 0x1
	.uleb128 .LEHB17-.LFB2661
	.uleb128 .LEHE17-.LEHB17
	.uleb128 .L209-.LFB2661
	.uleb128 0
	.uleb128 .LEHB18-.LFB2661
	.uleb128 .LEHE18-.LEHB18
	.uleb128 0
	.uleb128 0
.LLSDACSE2661:
	.byte	0x1
	.byte	0
	.align 4
	.long	0

.LLSDATT2661:
	.section	.text._ZNSt22__uninitialized_fill_nILb0EE15__uninit_fill_nIPSt6vectorIcSaIcEEmS4_EET_S6_T0_RKT1_,"axG",@progbits,_ZNSt22__uninitialized_fill_nILb0EE15__uninit_fill_nIPSt6vectorIcSaIcEEmS4_EET_S6_T0_RKT1_,comdat
	.size	_ZNSt22__uninitialized_fill_nILb0EE15__uninit_fill_nIPSt6vectorIcSaIcEEmS4_EET_S6_T0_RKT1_, .-_ZNSt22__uninitialized_fill_nILb0EE15__uninit_fill_nIPSt6vectorIcSaIcEEmS4_EET_S6_T0_RKT1_
	.section	.text._ZSt11__addressofISt6vectorIcSaIcEEEPT_RS3_,"axG",@progbits,_ZSt11__addressofISt6vectorIcSaIcEEEPT_RS3_,comdat
	.weak	_ZSt11__addressofISt6vectorIcSaIcEEEPT_RS3_
	.type	_ZSt11__addressofISt6vectorIcSaIcEEEPT_RS3_, @function
_ZSt11__addressofISt6vectorIcSaIcEEEPT_RS3_:
.LFB2662:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2662:
	.size	_ZSt11__addressofISt6vectorIcSaIcEEEPT_RS3_, .-_ZSt11__addressofISt6vectorIcSaIcEEEPT_RS3_
	.section	.text._ZSt8_DestroyISt6vectorIcSaIcEEEvPT_,"axG",@progbits,_ZSt8_DestroyISt6vectorIcSaIcEEEvPT_,comdat
	.weak	_ZSt8_DestroyISt6vectorIcSaIcEEEvPT_
	.type	_ZSt8_DestroyISt6vectorIcSaIcEEEvPT_, @function
_ZSt8_DestroyISt6vectorIcSaIcEEEvPT_:
.LFB2663:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNSt6vectorIcSaIcEED1Ev
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2663:
	.size	_ZSt8_DestroyISt6vectorIcSaIcEEEvPT_, .-_ZSt8_DestroyISt6vectorIcSaIcEEEvPT_
	.section	.text._ZSt19__iterator_categoryIPcENSt15iterator_traitsIT_E17iterator_categoryERKS2_,"axG",@progbits,_ZSt19__iterator_categoryIPcENSt15iterator_traitsIT_E17iterator_categoryERKS2_,comdat
	.weak	_ZSt19__iterator_categoryIPcENSt15iterator_traitsIT_E17iterator_categoryERKS2_
	.type	_ZSt19__iterator_categoryIPcENSt15iterator_traitsIT_E17iterator_categoryERKS2_, @function
_ZSt19__iterator_categoryIPcENSt15iterator_traitsIT_E17iterator_categoryERKS2_:
.LFB2666:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2666:
	.size	_ZSt19__iterator_categoryIPcENSt15iterator_traitsIT_E17iterator_categoryERKS2_, .-_ZSt19__iterator_categoryIPcENSt15iterator_traitsIT_E17iterator_categoryERKS2_
	.section	.text._ZNK9__gnu_cxx13new_allocatorIcE11_M_max_sizeEv,"axG",@progbits,_ZNK9__gnu_cxx13new_allocatorIcE11_M_max_sizeEv,comdat
	.align 2
	.weak	_ZNK9__gnu_cxx13new_allocatorIcE11_M_max_sizeEv
	.type	_ZNK9__gnu_cxx13new_allocatorIcE11_M_max_sizeEv, @function
_ZNK9__gnu_cxx13new_allocatorIcE11_M_max_sizeEv:
.LFB2687:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	movabs	rax, 9223372036854775807
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2687:
	.size	_ZNK9__gnu_cxx13new_allocatorIcE11_M_max_sizeEv, .-_ZNK9__gnu_cxx13new_allocatorIcE11_M_max_sizeEv
	.section	.text._ZN9__gnu_cxx13new_allocatorIcE8allocateEmPKv,"axG",@progbits,_ZN9__gnu_cxx13new_allocatorIcE8allocateEmPKv,comdat
	.align 2
	.weak	_ZN9__gnu_cxx13new_allocatorIcE8allocateEmPKv
	.type	_ZN9__gnu_cxx13new_allocatorIcE8allocateEmPKv, @function
_ZN9__gnu_cxx13new_allocatorIcE8allocateEmPKv:
.LFB2688:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNK9__gnu_cxx13new_allocatorIcE11_M_max_sizeEv
	cmp	QWORD PTR -16[rbp], rax
	seta	al
	movzx	eax, al
	test	rax, rax
	setne	al
	test	al, al
	je	.L219
	call	_ZSt17__throw_bad_allocv@PLT
.L219:
	mov	rax, QWORD PTR -16[rbp]
	mov	rdi, rax
	call	_Znwm@PLT
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2688:
	.size	_ZN9__gnu_cxx13new_allocatorIcE8allocateEmPKv, .-_ZN9__gnu_cxx13new_allocatorIcE8allocateEmPKv
	.section	.text._ZSt6fill_nIPcmcET_S1_T0_RKT1_,"axG",@progbits,_ZSt6fill_nIPcmcET_S1_T0_RKT1_,comdat
	.weak	_ZSt6fill_nIPcmcET_S1_T0_RKT1_
	.type	_ZSt6fill_nIPcmcET_S1_T0_RKT1_, @function
_ZSt6fill_nIPcmcET_S1_T0_RKT1_:
.LFB2689:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	lea	rax, -8[rbp]
	mov	rdi, rax
	call	_ZSt19__iterator_categoryIPcENSt15iterator_traitsIT_E17iterator_categoryERKS2_
	mov	rax, QWORD PTR -16[rbp]
	mov	rdi, rax
	call	_ZSt17__size_to_integerm
	mov	rcx, rax
	mov	rax, QWORD PTR -8[rbp]
	mov	rdx, QWORD PTR -24[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZSt10__fill_n_aIPcmcET_S1_T0_RKT1_St26random_access_iterator_tag
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2689:
	.size	_ZSt6fill_nIPcmcET_S1_T0_RKT1_, .-_ZSt6fill_nIPcmcET_S1_T0_RKT1_
	.section	.text._ZNK9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE11_M_max_sizeEv,"axG",@progbits,_ZNK9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE11_M_max_sizeEv,comdat
	.align 2
	.weak	_ZNK9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE11_M_max_sizeEv
	.type	_ZNK9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE11_M_max_sizeEv, @function
_ZNK9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE11_M_max_sizeEv:
.LFB2690:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	movabs	rax, 384307168202282325
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2690:
	.size	_ZNK9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE11_M_max_sizeEv, .-_ZNK9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE11_M_max_sizeEv
	.section	.text._ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE8allocateEmPKv,"axG",@progbits,_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE8allocateEmPKv,comdat
	.align 2
	.weak	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE8allocateEmPKv
	.type	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE8allocateEmPKv, @function
_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE8allocateEmPKv:
.LFB2691:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	mov	rax, QWORD PTR -8[rbp]
	mov	rdi, rax
	call	_ZNK9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE11_M_max_sizeEv
	cmp	QWORD PTR -16[rbp], rax
	seta	al
	movzx	eax, al
	test	rax, rax
	setne	al
	test	al, al
	je	.L226
	movabs	rax, 768614336404564650
	cmp	QWORD PTR -16[rbp], rax
	jbe	.L227
	call	_ZSt28__throw_bad_array_new_lengthv@PLT
.L227:
	call	_ZSt17__throw_bad_allocv@PLT
.L226:
	mov	rdx, QWORD PTR -16[rbp]
	mov	rax, rdx
	add	rax, rax
	add	rax, rdx
	sal	rax, 3
	mov	rdi, rax
	call	_Znwm@PLT
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2691:
	.size	_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE8allocateEmPKv, .-_ZN9__gnu_cxx13new_allocatorISt6vectorIcSaIcEEE8allocateEmPKv
	.section	.text._ZSt10_ConstructISt6vectorIcSaIcEEJRKS2_EEvPT_DpOT0_,"axG",@progbits,_ZSt10_ConstructISt6vectorIcSaIcEEJRKS2_EEvPT_DpOT0_,comdat
	.weak	_ZSt10_ConstructISt6vectorIcSaIcEEJRKS2_EEvPT_DpOT0_
	.type	_ZSt10_ConstructISt6vectorIcSaIcEEJRKS2_EEvPT_DpOT0_, @function
_ZSt10_ConstructISt6vectorIcSaIcEEJRKS2_EEvPT_DpOT0_:
.LFB2692:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA2692
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	push	r13
	push	r12
	push	rbx
	sub	rsp, 24
	.cfi_offset 13, -24
	.cfi_offset 12, -32
	.cfi_offset 3, -40
	mov	QWORD PTR -40[rbp], rdi
	mov	QWORD PTR -48[rbp], rsi
	mov	rax, QWORD PTR -48[rbp]
	mov	rdi, rax
	call	_ZSt7forwardIRKSt6vectorIcSaIcEEEOT_RNSt16remove_referenceIS5_E4typeE
	mov	r13, rax
	mov	rbx, QWORD PTR -40[rbp]
	mov	rsi, rbx
	mov	edi, 24
	call	_ZnwmPv
	mov	r12, rax
	mov	rsi, r13
	mov	rdi, r12
.LEHB19:
	call	_ZNSt6vectorIcSaIcEEC1ERKS1_
.LEHE19:
	jmp	.L232
.L231:
	endbr64
	mov	r13, rax
	mov	rsi, rbx
	mov	rdi, r12
	call	_ZdlPvS_
	mov	rax, r13
	mov	rdi, rax
.LEHB20:
	call	_Unwind_Resume@PLT
.LEHE20:
.L232:
	add	rsp, 24
	pop	rbx
	pop	r12
	pop	r13
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2692:
	.section	.gcc_except_table
.LLSDA2692:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE2692-.LLSDACSB2692
.LLSDACSB2692:
	.uleb128 .LEHB19-.LFB2692
	.uleb128 .LEHE19-.LEHB19
	.uleb128 .L231-.LFB2692
	.uleb128 0
	.uleb128 .LEHB20-.LFB2692
	.uleb128 .LEHE20-.LEHB20
	.uleb128 0
	.uleb128 0
.LLSDACSE2692:
	.section	.text._ZSt10_ConstructISt6vectorIcSaIcEEJRKS2_EEvPT_DpOT0_,"axG",@progbits,_ZSt10_ConstructISt6vectorIcSaIcEEJRKS2_EEvPT_DpOT0_,comdat
	.size	_ZSt10_ConstructISt6vectorIcSaIcEEJRKS2_EEvPT_DpOT0_, .-_ZSt10_ConstructISt6vectorIcSaIcEEJRKS2_EEvPT_DpOT0_
	.section	.text._ZSt10__fill_n_aIPcmcET_S1_T0_RKT1_St26random_access_iterator_tag,"axG",@progbits,_ZSt10__fill_n_aIPcmcET_S1_T0_RKT1_St26random_access_iterator_tag,comdat
	.weak	_ZSt10__fill_n_aIPcmcET_S1_T0_RKT1_St26random_access_iterator_tag
	.type	_ZSt10__fill_n_aIPcmcET_S1_T0_RKT1_St26random_access_iterator_tag, @function
_ZSt10__fill_n_aIPcmcET_S1_T0_RKT1_St26random_access_iterator_tag:
.LFB2704:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	cmp	QWORD PTR -16[rbp], 0
	jne	.L234
	mov	rax, QWORD PTR -8[rbp]
	jmp	.L235
.L234:
	mov	rdx, QWORD PTR -8[rbp]
	mov	rax, QWORD PTR -16[rbp]
	lea	rcx, [rdx+rax]
	mov	rdx, QWORD PTR -24[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZSt8__fill_aIPccEvT_S1_RKT0_
	mov	rdx, QWORD PTR -8[rbp]
	mov	rax, QWORD PTR -16[rbp]
	add	rax, rdx
.L235:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2704:
	.size	_ZSt10__fill_n_aIPcmcET_S1_T0_RKT1_St26random_access_iterator_tag, .-_ZSt10__fill_n_aIPcmcET_S1_T0_RKT1_St26random_access_iterator_tag
	.section	.text._ZSt7forwardIRKSt6vectorIcSaIcEEEOT_RNSt16remove_referenceIS5_E4typeE,"axG",@progbits,_ZSt7forwardIRKSt6vectorIcSaIcEEEOT_RNSt16remove_referenceIS5_E4typeE,comdat
	.weak	_ZSt7forwardIRKSt6vectorIcSaIcEEEOT_RNSt16remove_referenceIS5_E4typeE
	.type	_ZSt7forwardIRKSt6vectorIcSaIcEEEOT_RNSt16remove_referenceIS5_E4typeE, @function
_ZSt7forwardIRKSt6vectorIcSaIcEEEOT_RNSt16remove_referenceIS5_E4typeE:
.LFB2705:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2705:
	.size	_ZSt7forwardIRKSt6vectorIcSaIcEEEOT_RNSt16remove_referenceIS5_E4typeE, .-_ZSt7forwardIRKSt6vectorIcSaIcEEEOT_RNSt16remove_referenceIS5_E4typeE
	.section	.text._ZNSt6vectorIcSaIcEEC2ERKS1_,"axG",@progbits,_ZNSt6vectorIcSaIcEEC5ERKS1_,comdat
	.align 2
	.weak	_ZNSt6vectorIcSaIcEEC2ERKS1_
	.type	_ZNSt6vectorIcSaIcEEC2ERKS1_, @function
_ZNSt6vectorIcSaIcEEC2ERKS1_:
.LFB2707:
	.cfi_startproc
	.cfi_personality 0x9b,DW.ref.__gxx_personality_v0
	.cfi_lsda 0x1b,.LLSDA2707
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	push	r13
	push	r12
	push	rbx
	sub	rsp, 40
	.cfi_offset 13, -24
	.cfi_offset 12, -32
	.cfi_offset 3, -40
	mov	QWORD PTR -56[rbp], rdi
	mov	QWORD PTR -64[rbp], rsi
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR -40[rbp], rax
	xor	eax, eax
	mov	rbx, QWORD PTR -56[rbp]
	mov	rax, QWORD PTR -64[rbp]
	mov	rdi, rax
	call	_ZNKSt12_Vector_baseIcSaIcEE19_M_get_Tp_allocatorEv
	mov	rdx, rax
	lea	rax, -41[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZN9__gnu_cxx14__alloc_traitsISaIcEcE17_S_select_on_copyERKS1_
	mov	rax, QWORD PTR -64[rbp]
	mov	rdi, rax
	call	_ZNKSt6vectorIcSaIcEE4sizeEv
	mov	rcx, rax
	lea	rax, -41[rbp]
	mov	rdx, rax
	mov	rsi, rcx
	mov	rdi, rbx
.LEHB21:
	call	_ZNSt12_Vector_baseIcSaIcEEC2EmRKS0_
.LEHE21:
	lea	rax, -41[rbp]
	mov	rdi, rax
	call	_ZNSaIcED1Ev@PLT
	mov	rax, QWORD PTR -56[rbp]
	mov	rdi, rax
	call	_ZNSt12_Vector_baseIcSaIcEE19_M_get_Tp_allocatorEv
	mov	r13, rax
	mov	rax, QWORD PTR -56[rbp]
	mov	rbx, QWORD PTR [rax]
	mov	rax, QWORD PTR -64[rbp]
	mov	rdi, rax
	call	_ZNKSt6vectorIcSaIcEE3endEv
	mov	r12, rax
	mov	rax, QWORD PTR -64[rbp]
	mov	rdi, rax
	call	_ZNKSt6vectorIcSaIcEE5beginEv
	mov	rcx, r13
	mov	rdx, rbx
	mov	rsi, r12
	mov	rdi, rax
.LEHB22:
	call	_ZSt22__uninitialized_copy_aIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPccET0_T_SA_S9_RSaIT1_E
.LEHE22:
	mov	rdx, QWORD PTR -56[rbp]
	mov	QWORD PTR 8[rdx], rax
	nop
	mov	rax, QWORD PTR -40[rbp]
	sub	rax, QWORD PTR fs:40
	je	.L241
	jmp	.L244
.L242:
	endbr64
	mov	rbx, rax
	lea	rax, -41[rbp]
	mov	rdi, rax
	call	_ZNSaIcED1Ev@PLT
	mov	rax, rbx
	mov	rdi, rax
.LEHB23:
	call	_Unwind_Resume@PLT
.L243:
	endbr64
	mov	rbx, rax
	mov	rax, QWORD PTR -56[rbp]
	mov	rdi, rax
	call	_ZNSt12_Vector_baseIcSaIcEED2Ev
	mov	rax, rbx
	mov	rdi, rax
	call	_Unwind_Resume@PLT
.LEHE23:
.L244:
	call	__stack_chk_fail@PLT
.L241:
	add	rsp, 40
	pop	rbx
	pop	r12
	pop	r13
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2707:
	.section	.gcc_except_table
.LLSDA2707:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 .LLSDACSE2707-.LLSDACSB2707
.LLSDACSB2707:
	.uleb128 .LEHB21-.LFB2707
	.uleb128 .LEHE21-.LEHB21
	.uleb128 .L242-.LFB2707
	.uleb128 0
	.uleb128 .LEHB22-.LFB2707
	.uleb128 .LEHE22-.LEHB22
	.uleb128 .L243-.LFB2707
	.uleb128 0
	.uleb128 .LEHB23-.LFB2707
	.uleb128 .LEHE23-.LEHB23
	.uleb128 0
	.uleb128 0
.LLSDACSE2707:
	.section	.text._ZNSt6vectorIcSaIcEEC2ERKS1_,"axG",@progbits,_ZNSt6vectorIcSaIcEEC5ERKS1_,comdat
	.size	_ZNSt6vectorIcSaIcEEC2ERKS1_, .-_ZNSt6vectorIcSaIcEEC2ERKS1_
	.weak	_ZNSt6vectorIcSaIcEEC1ERKS1_
	.set	_ZNSt6vectorIcSaIcEEC1ERKS1_,_ZNSt6vectorIcSaIcEEC2ERKS1_
	.section	.text._ZSt8__fill_aIPccEvT_S1_RKT0_,"axG",@progbits,_ZSt8__fill_aIPccEvT_S1_RKT0_,comdat
	.weak	_ZSt8__fill_aIPccEvT_S1_RKT0_
	.type	_ZSt8__fill_aIPccEvT_S1_RKT0_, @function
_ZSt8__fill_aIPccEvT_S1_RKT0_:
.LFB2711:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	mov	rdx, QWORD PTR -24[rbp]
	mov	rcx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZSt9__fill_a1IcEN9__gnu_cxx11__enable_ifIXsrSt9__is_byteIT_E7__valueEvE6__typeEPS3_S7_RKS3_
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2711:
	.size	_ZSt8__fill_aIPccEvT_S1_RKT0_, .-_ZSt8__fill_aIPccEvT_S1_RKT0_
	.section	.text._ZNKSt6vectorIcSaIcEE4sizeEv,"axG",@progbits,_ZNKSt6vectorIcSaIcEE4sizeEv,comdat
	.align 2
	.weak	_ZNKSt6vectorIcSaIcEE4sizeEv
	.type	_ZNKSt6vectorIcSaIcEE4sizeEv, @function
_ZNKSt6vectorIcSaIcEE4sizeEv:
.LFB2712:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	mov	rdx, QWORD PTR 8[rax]
	mov	rax, QWORD PTR -8[rbp]
	mov	rcx, QWORD PTR [rax]
	mov	rax, rdx
	sub	rax, rcx
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2712:
	.size	_ZNKSt6vectorIcSaIcEE4sizeEv, .-_ZNKSt6vectorIcSaIcEE4sizeEv
	.section	.text._ZNKSt12_Vector_baseIcSaIcEE19_M_get_Tp_allocatorEv,"axG",@progbits,_ZNKSt12_Vector_baseIcSaIcEE19_M_get_Tp_allocatorEv,comdat
	.align 2
	.weak	_ZNKSt12_Vector_baseIcSaIcEE19_M_get_Tp_allocatorEv
	.type	_ZNKSt12_Vector_baseIcSaIcEE19_M_get_Tp_allocatorEv, @function
_ZNKSt12_Vector_baseIcSaIcEE19_M_get_Tp_allocatorEv:
.LFB2713:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2713:
	.size	_ZNKSt12_Vector_baseIcSaIcEE19_M_get_Tp_allocatorEv, .-_ZNKSt12_Vector_baseIcSaIcEE19_M_get_Tp_allocatorEv
	.section	.text._ZNKSt6vectorIcSaIcEE5beginEv,"axG",@progbits,_ZNKSt6vectorIcSaIcEE5beginEv,comdat
	.align 2
	.weak	_ZNKSt6vectorIcSaIcEE5beginEv
	.type	_ZNKSt6vectorIcSaIcEE5beginEv, @function
_ZNKSt6vectorIcSaIcEE5beginEv:
.LFB2714:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -24[rbp], rdi
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR -8[rbp], rax
	xor	eax, eax
	mov	rdx, QWORD PTR -24[rbp]
	lea	rax, -16[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEC1ERKS2_
	mov	rax, QWORD PTR -16[rbp]
	mov	rdx, QWORD PTR -8[rbp]
	sub	rdx, QWORD PTR fs:40
	je	.L252
	call	__stack_chk_fail@PLT
.L252:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2714:
	.size	_ZNKSt6vectorIcSaIcEE5beginEv, .-_ZNKSt6vectorIcSaIcEE5beginEv
	.section	.text._ZNKSt6vectorIcSaIcEE3endEv,"axG",@progbits,_ZNKSt6vectorIcSaIcEE3endEv,comdat
	.align 2
	.weak	_ZNKSt6vectorIcSaIcEE3endEv
	.type	_ZNKSt6vectorIcSaIcEE3endEv, @function
_ZNKSt6vectorIcSaIcEE3endEv:
.LFB2715:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -24[rbp], rdi
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR -8[rbp], rax
	xor	eax, eax
	mov	rax, QWORD PTR -24[rbp]
	lea	rdx, 8[rax]
	lea	rax, -16[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEC1ERKS2_
	mov	rax, QWORD PTR -16[rbp]
	mov	rdx, QWORD PTR -8[rbp]
	sub	rdx, QWORD PTR fs:40
	je	.L255
	call	__stack_chk_fail@PLT
.L255:
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2715:
	.size	_ZNKSt6vectorIcSaIcEE3endEv, .-_ZNKSt6vectorIcSaIcEE3endEv
	.section	.text._ZSt22__uninitialized_copy_aIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPccET0_T_SA_S9_RSaIT1_E,"axG",@progbits,_ZSt22__uninitialized_copy_aIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPccET0_T_SA_S9_RSaIT1_E,comdat
	.weak	_ZSt22__uninitialized_copy_aIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPccET0_T_SA_S9_RSaIT1_E
	.type	_ZSt22__uninitialized_copy_aIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPccET0_T_SA_S9_RSaIT1_E, @function
_ZSt22__uninitialized_copy_aIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPccET0_T_SA_S9_RSaIT1_E:
.LFB2716:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	mov	QWORD PTR -32[rbp], rcx
	mov	rdx, QWORD PTR -24[rbp]
	mov	rcx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZSt18uninitialized_copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET0_T_SA_S9_
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2716:
	.size	_ZSt22__uninitialized_copy_aIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPccET0_T_SA_S9_RSaIT1_E, .-_ZSt22__uninitialized_copy_aIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPccET0_T_SA_S9_RSaIT1_E
	.section	.text._ZSt9__fill_a1IcEN9__gnu_cxx11__enable_ifIXsrSt9__is_byteIT_E7__valueEvE6__typeEPS3_S7_RKS3_,"axG",@progbits,_ZSt9__fill_a1IcEN9__gnu_cxx11__enable_ifIXsrSt9__is_byteIT_E7__valueEvE6__typeEPS3_S7_RKS3_,comdat
	.weak	_ZSt9__fill_a1IcEN9__gnu_cxx11__enable_ifIXsrSt9__is_byteIT_E7__valueEvE6__typeEPS3_S7_RKS3_
	.type	_ZSt9__fill_a1IcEN9__gnu_cxx11__enable_ifIXsrSt9__is_byteIT_E7__valueEvE6__typeEPS3_S7_RKS3_, @function
_ZSt9__fill_a1IcEN9__gnu_cxx11__enable_ifIXsrSt9__is_byteIT_E7__valueEvE6__typeEPS3_S7_RKS3_:
.LFB2717:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 48
	mov	QWORD PTR -24[rbp], rdi
	mov	QWORD PTR -32[rbp], rsi
	mov	QWORD PTR -40[rbp], rdx
	mov	rax, QWORD PTR -40[rbp]
	movzx	eax, BYTE PTR [rax]
	mov	BYTE PTR -9[rbp], al
	mov	rax, QWORD PTR -32[rbp]
	sub	rax, QWORD PTR -24[rbp]
	mov	QWORD PTR -8[rbp], rax
	cmp	QWORD PTR -8[rbp], 0
	je	.L260
	movzx	eax, BYTE PTR -9[rbp]
	movzx	ecx, al
	mov	rdx, QWORD PTR -8[rbp]
	mov	rax, QWORD PTR -24[rbp]
	mov	esi, ecx
	mov	rdi, rax
	call	memset@PLT
.L260:
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2717:
	.size	_ZSt9__fill_a1IcEN9__gnu_cxx11__enable_ifIXsrSt9__is_byteIT_E7__valueEvE6__typeEPS3_S7_RKS3_, .-_ZSt9__fill_a1IcEN9__gnu_cxx11__enable_ifIXsrSt9__is_byteIT_E7__valueEvE6__typeEPS3_S7_RKS3_
	.section	.text._ZN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEC2ERKS2_,"axG",@progbits,_ZN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEC5ERKS2_,comdat
	.align 2
	.weak	_ZN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEC2ERKS2_
	.type	_ZN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEC2ERKS2_, @function
_ZN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEC2ERKS2_:
.LFB2719:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	rax, QWORD PTR -16[rbp]
	mov	rdx, QWORD PTR [rax]
	mov	rax, QWORD PTR -8[rbp]
	mov	QWORD PTR [rax], rdx
	nop
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2719:
	.size	_ZN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEC2ERKS2_, .-_ZN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEC2ERKS2_
	.weak	_ZN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEC1ERKS2_
	.set	_ZN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEC1ERKS2_,_ZN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEC2ERKS2_
	.section	.text._ZSt18uninitialized_copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET0_T_SA_S9_,"axG",@progbits,_ZSt18uninitialized_copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET0_T_SA_S9_,comdat
	.weak	_ZSt18uninitialized_copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET0_T_SA_S9_
	.type	_ZSt18uninitialized_copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET0_T_SA_S9_, @function
_ZSt18uninitialized_copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET0_T_SA_S9_:
.LFB2721:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 48
	mov	QWORD PTR -24[rbp], rdi
	mov	QWORD PTR -32[rbp], rsi
	mov	QWORD PTR -40[rbp], rdx
	mov	BYTE PTR -1[rbp], 1
	mov	rdx, QWORD PTR -40[rbp]
	mov	rcx, QWORD PTR -32[rbp]
	mov	rax, QWORD PTR -24[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZNSt20__uninitialized_copyILb1EE13__uninit_copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcEET0_T_SC_SB_
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2721:
	.size	_ZSt18uninitialized_copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET0_T_SA_S9_, .-_ZSt18uninitialized_copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET0_T_SA_S9_
	.section	.text._ZNSt20__uninitialized_copyILb1EE13__uninit_copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcEET0_T_SC_SB_,"axG",@progbits,_ZNSt20__uninitialized_copyILb1EE13__uninit_copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcEET0_T_SC_SB_,comdat
	.weak	_ZNSt20__uninitialized_copyILb1EE13__uninit_copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcEET0_T_SC_SB_
	.type	_ZNSt20__uninitialized_copyILb1EE13__uninit_copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcEET0_T_SC_SB_, @function
_ZNSt20__uninitialized_copyILb1EE13__uninit_copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcEET0_T_SC_SB_:
.LFB2723:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	mov	rdx, QWORD PTR -24[rbp]
	mov	rcx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZSt4copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET0_T_SA_S9_
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2723:
	.size	_ZNSt20__uninitialized_copyILb1EE13__uninit_copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcEET0_T_SC_SB_, .-_ZNSt20__uninitialized_copyILb1EE13__uninit_copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcEET0_T_SC_SB_
	.section	.text._ZSt4copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET0_T_SA_S9_,"axG",@progbits,_ZSt4copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET0_T_SA_S9_,comdat
	.weak	_ZSt4copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET0_T_SA_S9_
	.type	_ZSt4copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET0_T_SA_S9_, @function
_ZSt4copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET0_T_SA_S9_:
.LFB2724:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	push	rbx
	sub	rsp, 40
	.cfi_offset 3, -24
	mov	QWORD PTR -24[rbp], rdi
	mov	QWORD PTR -32[rbp], rsi
	mov	QWORD PTR -40[rbp], rdx
	mov	rax, QWORD PTR -32[rbp]
	mov	rdi, rax
	call	_ZSt12__miter_baseIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEET_S8_
	mov	rbx, rax
	mov	rax, QWORD PTR -24[rbp]
	mov	rdi, rax
	call	_ZSt12__miter_baseIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEET_S8_
	mov	rcx, rax
	mov	rax, QWORD PTR -40[rbp]
	mov	rdx, rax
	mov	rsi, rbx
	mov	rdi, rcx
	call	_ZSt13__copy_move_aILb0EN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET1_T0_SA_S9_
	mov	rbx, QWORD PTR -8[rbp]
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2724:
	.size	_ZSt4copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET0_T_SA_S9_, .-_ZSt4copyIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET0_T_SA_S9_
	.section	.text._ZSt12__miter_baseIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEET_S8_,"axG",@progbits,_ZSt12__miter_baseIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEET_S8_,comdat
	.weak	_ZSt12__miter_baseIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEET_S8_
	.type	_ZSt12__miter_baseIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEET_S8_, @function
_ZSt12__miter_baseIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEET_S8_:
.LFB2725:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2725:
	.size	_ZSt12__miter_baseIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEET_S8_, .-_ZSt12__miter_baseIN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEET_S8_
	.section	.text._ZSt13__copy_move_aILb0EN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET1_T0_SA_S9_,"axG",@progbits,_ZSt13__copy_move_aILb0EN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET1_T0_SA_S9_,comdat
	.weak	_ZSt13__copy_move_aILb0EN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET1_T0_SA_S9_
	.type	_ZSt13__copy_move_aILb0EN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET1_T0_SA_S9_, @function
_ZSt13__copy_move_aILb0EN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET1_T0_SA_S9_:
.LFB2726:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	push	r12
	push	rbx
	sub	rsp, 32
	.cfi_offset 12, -24
	.cfi_offset 3, -32
	mov	QWORD PTR -24[rbp], rdi
	mov	QWORD PTR -32[rbp], rsi
	mov	QWORD PTR -40[rbp], rdx
	mov	rax, QWORD PTR -40[rbp]
	mov	rdi, rax
	call	_ZSt12__niter_baseIPcET_S1_
	mov	r12, rax
	mov	rax, QWORD PTR -32[rbp]
	mov	rdi, rax
	call	_ZSt12__niter_baseIPKcSt6vectorIcSaIcEEET_N9__gnu_cxx17__normal_iteratorIS5_T0_EE
	mov	rbx, rax
	mov	rax, QWORD PTR -24[rbp]
	mov	rdi, rax
	call	_ZSt12__niter_baseIPKcSt6vectorIcSaIcEEET_N9__gnu_cxx17__normal_iteratorIS5_T0_EE
	mov	rdx, r12
	mov	rsi, rbx
	mov	rdi, rax
	call	_ZSt14__copy_move_a1ILb0EPKcPcET1_T0_S4_S3_
	mov	rdx, rax
	lea	rax, -40[rbp]
	mov	rsi, rdx
	mov	rdi, rax
	call	_ZSt12__niter_wrapIPcET_RKS1_S1_
	add	rsp, 32
	pop	rbx
	pop	r12
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2726:
	.size	_ZSt13__copy_move_aILb0EN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET1_T0_SA_S9_, .-_ZSt13__copy_move_aILb0EN9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEEEPcET1_T0_SA_S9_
	.section	.text._ZSt12__niter_baseIPKcSt6vectorIcSaIcEEET_N9__gnu_cxx17__normal_iteratorIS5_T0_EE,"axG",@progbits,_ZSt12__niter_baseIPKcSt6vectorIcSaIcEEET_N9__gnu_cxx17__normal_iteratorIS5_T0_EE,comdat
	.weak	_ZSt12__niter_baseIPKcSt6vectorIcSaIcEEET_N9__gnu_cxx17__normal_iteratorIS5_T0_EE
	.type	_ZSt12__niter_baseIPKcSt6vectorIcSaIcEEET_N9__gnu_cxx17__normal_iteratorIS5_T0_EE, @function
_ZSt12__niter_baseIPKcSt6vectorIcSaIcEEET_N9__gnu_cxx17__normal_iteratorIS5_T0_EE:
.LFB2729:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	QWORD PTR -8[rbp], rdi
	lea	rax, -8[rbp]
	mov	rdi, rax
	call	_ZNK9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEE4baseEv
	mov	rax, QWORD PTR [rax]
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2729:
	.size	_ZSt12__niter_baseIPKcSt6vectorIcSaIcEEET_N9__gnu_cxx17__normal_iteratorIS5_T0_EE, .-_ZSt12__niter_baseIPKcSt6vectorIcSaIcEEET_N9__gnu_cxx17__normal_iteratorIS5_T0_EE
	.section	.text._ZSt12__niter_baseIPcET_S1_,"axG",@progbits,_ZSt12__niter_baseIPcET_S1_,comdat
	.weak	_ZSt12__niter_baseIPcET_S1_
	.type	_ZSt12__niter_baseIPcET_S1_, @function
_ZSt12__niter_baseIPcET_S1_:
.LFB2730:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2730:
	.size	_ZSt12__niter_baseIPcET_S1_, .-_ZSt12__niter_baseIPcET_S1_
	.section	.text._ZSt14__copy_move_a1ILb0EPKcPcET1_T0_S4_S3_,"axG",@progbits,_ZSt14__copy_move_a1ILb0EPKcPcET1_T0_S4_S3_,comdat
	.weak	_ZSt14__copy_move_a1ILb0EPKcPcET1_T0_S4_S3_
	.type	_ZSt14__copy_move_a1ILb0EPKcPcET1_T0_S4_S3_, @function
_ZSt14__copy_move_a1ILb0EPKcPcET1_T0_S4_S3_:
.LFB2731:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	mov	rdx, QWORD PTR -24[rbp]
	mov	rcx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZSt14__copy_move_a2ILb0EPKcPcET1_T0_S4_S3_
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2731:
	.size	_ZSt14__copy_move_a1ILb0EPKcPcET1_T0_S4_S3_, .-_ZSt14__copy_move_a1ILb0EPKcPcET1_T0_S4_S3_
	.section	.text._ZSt12__niter_wrapIPcET_RKS1_S1_,"axG",@progbits,_ZSt12__niter_wrapIPcET_RKS1_S1_,comdat
	.weak	_ZSt12__niter_wrapIPcET_RKS1_S1_
	.type	_ZSt12__niter_wrapIPcET_RKS1_S1_, @function
_ZSt12__niter_wrapIPcET_RKS1_S1_:
.LFB2732:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	rax, QWORD PTR -16[rbp]
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2732:
	.size	_ZSt12__niter_wrapIPcET_RKS1_S1_, .-_ZSt12__niter_wrapIPcET_RKS1_S1_
	.section	.text._ZNK9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEE4baseEv,"axG",@progbits,_ZNK9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEE4baseEv,comdat
	.align 2
	.weak	_ZNK9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEE4baseEv
	.type	_ZNK9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEE4baseEv, @function
_ZNK9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEE4baseEv:
.LFB2733:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	QWORD PTR -8[rbp], rdi
	mov	rax, QWORD PTR -8[rbp]
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2733:
	.size	_ZNK9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEE4baseEv, .-_ZNK9__gnu_cxx17__normal_iteratorIPKcSt6vectorIcSaIcEEE4baseEv
	.section	.text._ZSt14__copy_move_a2ILb0EPKcPcET1_T0_S4_S3_,"axG",@progbits,_ZSt14__copy_move_a2ILb0EPKcPcET1_T0_S4_S3_,comdat
	.weak	_ZSt14__copy_move_a2ILb0EPKcPcET1_T0_S4_S3_
	.type	_ZSt14__copy_move_a2ILb0EPKcPcET1_T0_S4_S3_, @function
_ZSt14__copy_move_a2ILb0EPKcPcET1_T0_S4_S3_:
.LFB2734:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 32
	mov	QWORD PTR -8[rbp], rdi
	mov	QWORD PTR -16[rbp], rsi
	mov	QWORD PTR -24[rbp], rdx
	mov	rdx, QWORD PTR -24[rbp]
	mov	rcx, QWORD PTR -16[rbp]
	mov	rax, QWORD PTR -8[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	_ZNSt11__copy_moveILb0ELb1ESt26random_access_iterator_tagE8__copy_mIcEEPT_PKS3_S6_S4_
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2734:
	.size	_ZSt14__copy_move_a2ILb0EPKcPcET1_T0_S4_S3_, .-_ZSt14__copy_move_a2ILb0EPKcPcET1_T0_S4_S3_
	.section	.text._ZNSt11__copy_moveILb0ELb1ESt26random_access_iterator_tagE8__copy_mIcEEPT_PKS3_S6_S4_,"axG",@progbits,_ZNSt11__copy_moveILb0ELb1ESt26random_access_iterator_tagE8__copy_mIcEEPT_PKS3_S6_S4_,comdat
	.weak	_ZNSt11__copy_moveILb0ELb1ESt26random_access_iterator_tagE8__copy_mIcEEPT_PKS3_S6_S4_
	.type	_ZNSt11__copy_moveILb0ELb1ESt26random_access_iterator_tagE8__copy_mIcEEPT_PKS3_S6_S4_, @function
_ZNSt11__copy_moveILb0ELb1ESt26random_access_iterator_tagE8__copy_mIcEEPT_PKS3_S6_S4_:
.LFB2735:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 48
	mov	QWORD PTR -24[rbp], rdi
	mov	QWORD PTR -32[rbp], rsi
	mov	QWORD PTR -40[rbp], rdx
	mov	rax, QWORD PTR -32[rbp]
	sub	rax, QWORD PTR -24[rbp]
	mov	QWORD PTR -8[rbp], rax
	cmp	QWORD PTR -8[rbp], 0
	je	.L285
	mov	rdx, QWORD PTR -8[rbp]
	mov	rcx, QWORD PTR -24[rbp]
	mov	rax, QWORD PTR -40[rbp]
	mov	rsi, rcx
	mov	rdi, rax
	call	memmove@PLT
.L285:
	mov	rdx, QWORD PTR -8[rbp]
	mov	rax, QWORD PTR -40[rbp]
	add	rax, rdx
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2735:
	.size	_ZNSt11__copy_moveILb0ELb1ESt26random_access_iterator_tagE8__copy_mIcEEPT_PKS3_S6_S4_, .-_ZNSt11__copy_moveILb0ELb1ESt26random_access_iterator_tagE8__copy_mIcEEPT_PKS3_S6_S4_
	.text
	.type	_Z41__static_initialization_and_destruction_0ii, @function
_Z41__static_initialization_and_destruction_0ii:
.LFB2736:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	sub	rsp, 16
	mov	DWORD PTR -4[rbp], edi
	mov	DWORD PTR -8[rbp], esi
	cmp	DWORD PTR -4[rbp], 1
	jne	.L289
	cmp	DWORD PTR -8[rbp], 65535
	jne	.L289
	lea	rax, _ZStL8__ioinit[rip]
	mov	rdi, rax
	call	_ZNSt8ios_base4InitC1Ev@PLT
	lea	rax, __dso_handle[rip]
	mov	rdx, rax
	lea	rax, _ZStL8__ioinit[rip]
	mov	rsi, rax
	mov	rax, QWORD PTR _ZNSt8ios_base4InitD1Ev@GOTPCREL[rip]
	mov	rdi, rax
	call	__cxa_atexit@PLT
.L289:
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2736:
	.size	_Z41__static_initialization_and_destruction_0ii, .-_Z41__static_initialization_and_destruction_0ii
	.type	_GLOBAL__sub_I_main, @function
_GLOBAL__sub_I_main:
.LFB2737:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsp
	.cfi_def_cfa_register 6
	mov	esi, 65535
	mov	edi, 1
	call	_Z41__static_initialization_and_destruction_0ii
	pop	rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2737:
	.size	_GLOBAL__sub_I_main, .-_GLOBAL__sub_I_main
	.section	.init_array,"aw"
	.align 8
	.quad	_GLOBAL__sub_I_main
	.hidden	DW.ref.__gxx_personality_v0
	.weak	DW.ref.__gxx_personality_v0
	.section	.data.rel.local.DW.ref.__gxx_personality_v0,"awG",@progbits,DW.ref.__gxx_personality_v0,comdat
	.align 8
	.type	DW.ref.__gxx_personality_v0, @object
	.size	DW.ref.__gxx_personality_v0, 8
DW.ref.__gxx_personality_v0:
	.quad	__gxx_personality_v0
	.hidden	__dso_handle
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
