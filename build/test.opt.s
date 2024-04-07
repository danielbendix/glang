	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 14, 0
	.globl	_fac                            ; -- Begin function fac
	.p2align	2
_fac:                                   ; @fac
; %bb.0:                                ; %entry
	cbz	x0, LBB0_4
; %bb.1:                                ; %tailrecurse.preheader
	mov	x8, x0
	mov	w0, #1                          ; =0x1
LBB0_2:                                 ; %tailrecurse
                                        ; =>This Inner Loop Header: Depth=1
	subs	x9, x8, #1
	mul	x0, x8, x0
	mov	x8, x9
	b.ne	LBB0_2
; %bb.3:                                ; %common.ret
	ret
LBB0_4:
	mov	w0, #1                          ; =0x1
	ret
                                        ; -- End function
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
; %bb.0:                                ; %entry
	mov	w0, #24                         ; =0x18
	ret
                                        ; -- End function
.subsections_via_symbols
