	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 14, 0
	.globl	_fac                            ; -- Begin function fac
	.p2align	2
_fac:                                   ; @fac
	.cfi_startproc
; %bb.0:                                ; %entry
	mov	x8, x0
	mov	w0, #1                          ; =0x1
	mov	w9, #2                          ; =0x2
LBB0_1:                                 ; =>This Inner Loop Header: Depth=1
	mul	x0, x0, x9
	add	x9, x9, #1
	cmp	x9, x8
	b.lt	LBB0_1
; %bb.2:
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_fac_rec                        ; -- Begin function fac_rec
	.p2align	2
_fac_rec:                               ; @fac_rec
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x20, x19, [sp, #-32]!           ; 16-byte Folded Spill
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 32
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	mov	x19, x0
	subs	x0, x0, #1
	b.le	LBB1_2
; %bb.1:
	bl	_fac
	mul	x0, x19, x0
	b	LBB1_3
LBB1_2:
	mov	w0, #1                          ; =0x1
LBB1_3:                                 ; %common.ret
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp], #32             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x29, x30, [sp, #-16]!           ; 16-byte Folded Spill
	.cfi_def_cfa_offset 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	mov	w0, #10                         ; =0xa
	bl	_fac
	ldp	x29, x30, [sp], #16             ; 16-byte Folded Reload
	ret
	.cfi_endproc
                                        ; -- End function
.subsections_via_symbols
