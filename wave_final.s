	
;;; Phase 3 of the final project
;;; MASSIVE version (old ver: wave3old.s)
;;; (c) calvin chung / josh kang
	.requ	pc,r1
	.requ	ir,r2
	.requ	dest,r3
	.requ	src1,r4
	.requ	base,r4
	.requ	mode,r5
	.requ	src2,r6
	.requ	exp,r6
	.requ	src3,r7
	.requ	shReg,r7
	.requ	shCnt,r7
	.requ	value,r8
	.requ	temp,r9
	.requ	src1Num,r10
	.requ	stmUpd,r11
	.requ	ldmUpd,r11

;;; Constants
	.equ	ones24,0xFFFFFF
	.equ	ones9,0x1FF
	
	lea	warm,r0
	trap	$SysOverlay

loop:	
	mov	wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and	$ones24,pc
	mov	warm(pc),ir
	shr	$29,ir
	mov	condjmp(ir),rip	; jump to condition data table
	
instdec:					; start decoding instruction (bits 23 to 0)
	mov	warm(pc),r0
	trap	$SysPLA
	mov	opjmp(r0),rip

;;; Conditional Checks (bits 31 to 28)
nv:	add     $1,wpc		; condition never
	mov     wpc,pc          ; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
eq:	mov	wccr,ccr	; condition equal
	je	instdec
	add	$1,wpc
	mov     wpc,pc          	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

ne:	mov	wccr,ccr	; condition not equal
	jne	instdec
	add	$1,wpc
	mov     wpc,pc          	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
lt:	mov	wccr,ccr	; condition less than
	jl	instdec
	add	$1,wpc
	mov     wpc,pc          	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
ge:	mov     wccr,ccr	; condition greater than or equal
	jge	instdec
	add	$1,wpc
	mov     wpc,pc          	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
gt:	mov	wccr,ccr	; condition greater than
	jg	instdec
	add	$1,wpc
	mov     wpc,pc          	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
le:	mov     wccr,ccr	; condition less than or equal to
	jle	instdec
	add	$1,wpc
	mov     wpc,pc          	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
;;; INSTRUCTION EXECUTION AREA

;;; Branching
bl:	mov	wpc,wlr		; branch and link
	add	$1,wlr		; increment wlr then continue to regular branch

b:	add	warm(pc),wpc	; branch
	and	$ones24,wpc
	mov     wpc,pc          	; TOP OF INSTRUCTION FETCH LOOP
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

;;; LDM -- load multiple registers
ldm:	mov	wregs(dest),ldmUpd
	and	$ones24,ldmUpd
	and	$0xFFFF,value
ldm0:	test    $0x1,value
	je	ldm1
	mov	warm(ldmUpd),wr0
	add	$1,ldmUpd
ldm1:	test	$0x2,value
	je	ldm2
	mov	warm(ldmUpd),wr1
	add	$1,ldmUpd
ldm2:	test	$0x4,value
	je      ldm3
	mov	warm(ldmUpd),wr2
	add	$1,ldmUpd
ldm3:	test	$0x8,value
	je 	ldm4
	mov	warm(ldmUpd),wr3
	add	$1,ldmUpd
ldm4:	test	$0x10,value
	je      ldm5
	mov	warm(ldmUpd),wr4
	add	$1,ldmUpd
ldm5:	test    $0x20,value
	je	ldm6
	mov	warm(ldmUpd),wr5
	add	$1,ldmUpd
ldm6:	test    $0x40,value
	je	ldm7
	mov	warm(ldmUpd),wr6
	add	$1,ldmUpd
ldm7:	cmp	$0x80,value
	jl	ldmend
	test    $0x80,value
	je	ldm8
	mov	warm(ldmUpd),wr7
	add	$1,ldmUpd
ldm8:	test    $0x100,value
	je	ldm9
	mov	warm(ldmUpd),wr8
	add	$1,ldmUpd
ldm9:	test    $0x200,value
	je	ldm10
	mov	warm(ldmUpd),wr9
	add	$1,ldmUpd
ldm10:	test    $0x400,value
	je	ldm11
	mov	warm(ldmUpd),wr10
	add	$1,ldmUpd
ldm11:	test    $0x800,value
	je	ldm12
	mov	warm(ldmUpd),wr11
	add	$1,ldmUpd
ldm12:	test    $0x1000,value
	je	ldm13
	mov	warm(ldmUpd),wr12
	add	$1,ldmUpd
ldm13:	test    $0x2000,value
	je	ldm14
	mov	warm(ldmUpd),wr13
	add	$1,ldmUpd
ldm14:	test    $0x4000,value
	je	ldm15
	mov	warm(ldmUpd),wr14
	add	$1,ldmUpd
ldm15:	test    $0x8000,value
	je	ldmend
	mov	warm(ldmUpd),wpc
	and	$ones24,wpc
	mov	warm(ldmUpd),wccr
	shr	$28,wccr
	add	$1,ldmUpd
ldmend:	mov	ldmUpd,wregs(dest)
	mov     wpc,pc		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
;;; STM -- store multiple registers
stm:	mov	wregs(dest),stmUpd	; initial dest reg value (mem adr)
	and	$ones24,stmUpd
	and	$0xFFFF,value		; get bits 15 to 0 that correspond to regs
	cmp	$0x80,value
	jl	stm7
	test	$0x8000,value
	je	stm14
stm15:	sub     $1,stmUpd
	mov	wccr,warm(stmUpd)
	shl	$28,warm(stmUpd)
	or	wr15,warm(stmUpd)
stm14:	test	$0x4000,value
	je	stm13
	sub     $1,stmUpd
	mov	wr14,warm(stmUpd)
stm13:	test	$0x2000,value
	je	stm12
	sub     $1,stmUpd
	mov	wr13,warm(stmUpd)
stm12:	test	$0x1000,value
	je      stm11
	sub     $1,stmUpd
	mov     wr12,warm(stmUpd)
stm11:	test	$0x800,value
	je      stm10
	sub     $1,stmUpd
	mov     wr11,warm(stmUpd)
stm10:	test	$0x400,value
	je      stm9
	sub     $1,stmUpd
	mov     wr10,warm(stmUpd)
stm9:	test	$0x200,value
	je      stm8
	sub     $1,stmUpd
	mov     wr9,warm(stmUpd)
stm8:	test	$0x100,value
	je      stm7
	sub     $1,stmUpd
	mov     wr8,warm(stmUpd)
stm7:	test	$0x80,value
	je      stm6
	sub     $1,stmUpd
	mov     wr7,warm(stmUpd)
stm6:	test	$0x40,value
	je      stm5
	sub     $1,stmUpd
	mov     wr6,warm(stmUpd)
stm5:	test	$0x20,value
	je      stm4
	sub     $1,stmUpd
	mov     wr5,warm(stmUpd)
stm4:	test	$0x10,value
	je      stm3
	sub     $1,stmUpd
	mov     wr4,warm(stmUpd)
stm3:	test	$0x8,value
	je      stm2
	sub     $1,stmUpd
	mov     wr3,warm(stmUpd)
stm2:	test	$0x4,value
	je      stm1
	sub     $1,stmUpd
	mov     wr2,warm(stmUpd)
stm1:	test	$0x2,value
	je      stm0
	sub     $1,stmUpd
	mov     wr1,warm(stmUpd)
stm0:	test	$0x1,value
	je	stmEnd
	sub     $1,stmUpd
	mov     wr0,warm(stmUpd)
stmEnd:	mov	stmUpd,wregs(dest) 	; final address stored in dest reg
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table   
;;; MLA--only uses mode #4
mla:
	mov     warm(pc),dest		; Decoding 1st Half
	shr     $19,dest 		; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1		; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1 	; Value stored in src1 -> src1
	mov     warm(pc),src3		; MLA--get src3
	and     $0xF,src3     		; third src in src3
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2       	; src reg 2 in src
	add	$1,wpc
	mov     wregs(src2),value 	; memory stored in src reg2 to value
	mul	wregs(src3),value 	; right-hand source is src2 * src3
	add     value,src1	 	; src1 + src2 * src3
	mov     src1,wregs(dest) 	; update WARM dest reg
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table
	
mlas:
	mov     warm(pc),dest   	; Decoding 1st Half
	shr     $19,dest 		; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 		; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1 	; Value stored in src1 -> src1
	mov     warm(pc),src3 		; MLAS--get src3
	and     $0xF,src3 		; third src in src3
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 		; src reg 2 in src
	add	$1,wpc
	mov     wregs(src2),value 	; memory stored in src reg2 to value
	mul     wregs(src3),value 	; right-hand source is src2 * src3
	add     value,src1 		; src1 + src2 * src3
	mov     ccr,wccr
	mov     src1,wregs(dest) ; update WARM dest reg
	mov     wpc,pc			 ; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
;;; SWI -- can optimize since x take dest nor src 1 // directly jump to swi SWISWI
swi:
	mov	wr0,r0		; SWI by some value
	mov     warm(pc),value 	; compute imm value
	and     $ones9,value    
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp 
	shl     exp,value 	
	trap	value
	mov	r0,wr0
	add	$1,wpc
	mov     wpc,pc	    ; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
;;; SWIS -- same as swi but updates condition bits SWISSWIS
swis:
	mov     wr0,r0          ; SWI by some value
	mov     warm(pc),value ; compute imm value
	and     $ones9,value
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp
	shl     exp,value
	trap    value
	mov	r0,wr0
	or	$0,wr0
	mov	ccr,wccr
	add	$1,wpc
	mov     wpc,pc          	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
;;; VARYING CODE BASED on OPJMP ;;;

;;; ADD ;;;
addIM:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest		; Dest Reg -> dest 
	and     $0xF,dest 
	mov     warm(pc),src1	; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov	wregs(src1),src1

	mov     warm(pc),value
	and     $ones9,value 
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp
	add	$1,wpc
	shl     exp,value 

	lea		0(value,src1),wregs(dest) 	; ADD
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
addsIM:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
	
	mov     warm(pc),value
	and     $ones9,value
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp
	add     $1,wpc
	shl     exp,value

	add     value,src1 		; ADDSIM
	mov     ccr,wccr		; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

addSClsl:
	mov     warm(pc),dest		     ; Decoding 1st Half
	shr     $19,dest	     ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
	
	mov     warm(pc),shCnt          ; LSL Immediate
	and     $0x3F,shCnt	;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2	; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
		
	lea	0(value,src1),wregs(dest) 	; ADD
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
addsSClsl:
	mov     warm(pc),dest	     ; Decoding 1st Half
	shr     $19,dest     ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
	
	mov     warm(pc),shCnt	     ; LSL Immediate
	and     $0x3F,shCnt  ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2    ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value	     ; lsl that value by shift count
		
	add     value,src1 			; ADDS
	mov     ccr,wccr			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

addSClsr:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt	 ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
			
	lea		0(value,src1),wregs(dest) 	; ADD
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

addsSClsr:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
	
	mov     warm(pc),shCnt	; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	add     value,src1 		; ADDS
	mov     ccr,wccr		; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

addSCasr:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
	
	mov     warm(pc),shCnt	; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value  

	lea		0(value,src1),wregs(dest) 	; ADD
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

addsSCasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt	     ; AR immediate
	and     $0x3F,shCnt  ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2    ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value  ; asr src2 by shift count, stored in value
	
	add     value,src1 		; ADDS
	mov     ccr,wccr		; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

addSCror:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt	 ; ROR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value ; right source after ror, stored in value
	
	lea		0(value,src1),wregs(dest) 	; ADD
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

addsSCror:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value ; right source after ror, stored in value
	
	add     value,src1 		; ADDS
	mov     ccr,wccr		; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

addSRlsl:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg	 ; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value
	
	lea		0(value,src1),wregs(dest) 	; ADD
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

addsSRlsl:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value
	
	add     value,src1 		; ADDS
	mov     ccr,wccr		; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

addSRlsr:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg  ; LSR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value
	
	lea		0(value,src1),wregs(dest) 	; ADD
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

addsSRlsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value

	add     value,src1 		; ADDS
	mov     ccr,wccr		; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

addSRasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
	
	mov     warm(pc),shReg		 ; ASR register
	and     $0xF,shReg	 ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value
	
	lea		0(value,src1),wregs(dest) 	; ADD
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

addsSRasr:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value
	
	add     value,src1 		; ADDS
	mov     ccr,wccr		; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

addSRror:
	mov     warm(pc),dest ; Decoding 1st Half
	shr     $19,dest      ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg  ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value
		
	lea		0(value,src1),wregs(dest) 	; ADD
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

addsSRror: mov     warm(pc),dest ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value

	add     value,src1 			; ADDS
	mov     ccr,wccr			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
;;; ADC ADCADCADCADC 
adcIM:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),value
	and     $ones9,value
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp
	add     $1,wpc
	shl     exp,value
	
	mov		wccr,temp		; get warm condition bits
	shr		$1,temp			; shr so that C bit is in 1's
	and		$1,temp			; mask to isolate C bit
	add		temp,src1		; add C bit to src1
	lea		0(value,src1),wregs(dest)
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table
	
adcsIM:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),value
	and     $ones9,value
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp
	add     $1,wpc
	shl     exp,value
	
	mov     wccr,temp 		; get warm condition bits
	shr     $1,temp			; shr so that C bit is in 1's
	and     $1,temp			; mask to isolate C bit
	add     temp,src1 		; add C bit to src1
	add     value,src1		; add value to src1 + C
	mov     ccr,wccr 		; update condition bits
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

adcSClsl:
	mov     warm(pc),dest	     ; Decoding 1st Half
	shr     $19,dest     ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt  ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2    ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value  ; lsl that value by shift count
		
	mov		wccr,temp		; get warm condition bits
	shr		$1,temp			; shr so that C bit is in 1's
	and		$1,temp			; mask to isolate C bit
	add		temp,src1		; add C bit to src1
	lea		0(value,src1),wregs(dest)
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table
	
adcsSClsl:
	mov     warm(pc),dest	     ; Decoding 1st Half
	shr     $19,dest     ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt  ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2    ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value  ; lsl that value by shift count
	
	mov     wccr,temp 		; get warm condition bits
	shr     $1,temp			; shr so that C bit is in 1's
	and     $1,temp			; mask to isolate C bit
	add     temp,src1 		; add C bit to src1
	add     value,src1		; add value to src1 + C
	mov     ccr,wccr 		; update condition bits
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

adcSClsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	mov		wccr,temp		; get warm condition bits
	shr		$1,temp			; shr so that C bit is in 1's
	and		$1,temp			; mask to isolate C bit
	add		temp,src1		; add C bit to src1
	lea		0(value,src1),wregs(dest)
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

adcsSClsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	mov     wccr,temp 		; get warm condition bits
	shr     $1,temp			; shr so that C bit is in 1's
	and     $1,temp			; mask to isolate C bit
	add     temp,src1 		; add C bit to src1
	add     value,src1		; add value to src1 + C
	mov     ccr,wccr 		; update condition bits
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

adcSCasr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value
	
	mov		wccr,temp		; get warm condition bits
	shr		$1,temp			; shr so that C bit is in 1's
	and		$1,temp			; mask to isolate C bit
	add		temp,src1		; add C bit to src1
	lea		0(value,src1),wregs(dest)
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

adcsSCasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value
	
	mov     wccr,temp 		; get warm condition bits
	shr     $1,temp			; shr so that C bit is in 1's
	and     $1,temp			; mask to isolate C bit
	add     temp,src1 		; add C bit to src1
	add     value,src1		; add value to src1 + C
	mov     ccr,wccr 		; update condition bits
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

adcSCror:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value ; right source after ror, stored in value
	
	mov		wccr,temp		; get warm condition bits
	shr		$1,temp			; shr so that C bit is in 1's
	and		$1,temp			; mask to isolate C bit
	add		temp,src1		; add C bit to src1
	lea		0(value,src1),wregs(dest)
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

adcsSCror:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value ; right source after ror, stored in value
	
	mov     wccr,temp 		; get warm condition bits
	shr     $1,temp			; shr so that C bit is in 1's
	and     $1,temp			; mask to isolate C bit
	add     temp,src1 		; add C bit to src1
	add     value,src1		; add value to src1 + C
	mov     ccr,wccr 		; update condition bits
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

adcSRlsl:
	mov     warm(pc),dest    	; Decoding 1st Half
	shr     $19,dest	; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg	; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value
	
	mov		wccr,temp		; get warm condition bits
	shr		$1,temp			; shr so that C bit is in 1's
	and		$1,temp			; mask to isolate C bit
	add		temp,src1		; add C bit to src1
	lea		0(value,src1),wregs(dest)
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

adcsSRlsl:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg	; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value
	
	mov     wccr,temp 		; get warm condition bits
	shr     $1,temp			; shr so that C bit is in 1's
	and     $1,temp			; mask to isolate C bit
	add     temp,src1 		; add C bit to src1
	add     value,src1		; add value to src1 + C
	mov     ccr,wccr 		; update condition bits
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

adcSRlsr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg     ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value
	
	mov		wccr,temp		; get warm condition bits
	shr		$1,temp			; shr so that C bit is in 1's
	and		$1,temp			; mask to isolate C bit
	add		temp,src1		; add C bit to src1
	lea		0(value,src1),wregs(dest)
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

adcsSRlsr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg     ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value
	

adcSRasr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value
	
	mov		wccr,temp		; get warm condition bits
	shr		$1,temp			; shr so that C bit is in 1's
	and		$1,temp			; mask to isolate C bit
	add		temp,src1		; add C bit to src1
	lea		0(value,src1),wregs(dest)
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

adcsSRasr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value
	
	mov     wccr,temp 		; get warm condition bits
	shr     $1,temp			; shr so that C bit is in 1's
	and     $1,temp			; mask to isolate C bit
	add     temp,src1 		; add C bit to src1
	add     value,src1		; add value to src1 + C
	mov     ccr,wccr 		; update condition bits
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

adcSRror:
	mov     warm(pc),dest 	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value

	mov		wccr,temp		; get warm condition bits
	shr		$1,temp			; shr so that C bit is in 1's
	and		$1,temp			; mask to isolate C bit
	add		temp,src1		; add C bit to src1
	lea		0(value,src1),wregs(dest)
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

adcsSRror:
	mov     warm(pc),dest 	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value

	mov     wccr,temp 		; get warm condition bits
	shr     $1,temp			; shr so that C bit is in 1's
	and     $1,temp			; mask to isolate C bit
	add     temp,src1 		; add C bit to src1
	add     value,src1		; add value to src1 + C
	mov     ccr,wccr 		; update condition bits
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table
	
;;; SUB SUBSUBSUB
subIM:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),value
	and     $ones9,value
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp
	add     $1,wpc
	shl     exp,value
	
	sub     value,src1 		; SUB
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

subsIM:
	mov     warm(pc),dest   	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),value
	and     $ones9,value
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp
	add     $1,wpc
	shl     exp,value
	
	sub     value,src1 			; SUBS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

subSClsl:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
	
	sub     value,src1 			; SUB
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

subsSClsl:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count

	sub     value,src1 			; SUBS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

subSClsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	sub     value,src1 			; SUB
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

subsSClsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	sub     value,src1 			; SUBS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

subSCasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value

	sub     value,src1 			; SUB
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

subsSCasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value

	sub     value,src1 			; SUBS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

subSCror:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value ; right source after ror, stored in value
	
	sub     value,src1 			; SUB
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

subsSCror:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value ; right source after ror, stored in value

	sub     value,src1 			; SUBS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

subSRlsl:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
	
	mov     warm(pc),shReg	; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value
	
	sub     value,src1 			; SUB
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

subsSRlsl:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg	; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value

	sub     value,src1 			; SUBS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

subSRlsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg     ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value

	sub     value,src1 			; SUB
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

subsSRlsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg     ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value

	sub     value,src1 			; SUBS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

subSRasr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value
	
	sub     value,src1 			; SUB
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

subsSRasr:
	mov     warm(pc),dest    	; Decoding 1st Half
	shr     $19,dest	; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value
	
	sub     value,src1 			; SUBS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

subSRror:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value

	sub     value,src1 			; SUB
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

subsSRror:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value

	sub     value,src1 			; SUBS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table
	
;;; CMPCMPCMPCMP
cmpIM:
	mov     warm(pc),src1Num        ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),value		; IMM value
	and     $ones9,value 		; value in r5
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp 		; exponent in r6
	add		$1,wpc
	shl     exp,value 		; immediate value in value

	cmp     value,src1 		; CMP 
	mov		ccr,wccr		; might need to mask ccr with 0xF if ccr's high bits aren't 0
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

cmpSClsl:
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1

	mov     warm(pc),shCnt	; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
	
	cmp     value,src1 		; CMP 
	mov		ccr,wccr		; might need to mask ccr with 0xF if ccr's high bits aren't 0
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

cmpSClsr:
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shCnt	; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	cmp     value,src1 		; CMP 
	mov		ccr,wccr		; might need to mask ccr with 0xF if ccr's high bits aren't 0
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

cmpSCasr:
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value
	
	cmp     value,src1 		; CMP 
	mov		ccr,wccr		; might need to mask ccr with 0xF if ccr's high bits aren't 0
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

cmpSCror:
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1

	mov     warm(pc),shCnt  ; ROR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value ; right source after ror, stored in value
	
	cmp     value,src1 		; CMP 
	mov		ccr,wccr		; might need to mask ccr with 0xF if ccr's high bits aren't 0
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

cmpSRlsl:
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
		
	mov     warm(pc),shReg   ; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value
	
	cmp     value,src1 		; CMP 
	mov		ccr,wccr		; might need to mask ccr with 0xF if ccr's high bits aren't 0
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

cmpSRlsr:
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1

	mov     warm(pc),shReg  ; LSR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value
	
	cmp     value,src1 		; CMP 
	mov		ccr,wccr		; might need to mask ccr with 0xF if ccr's high bits aren't 0
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

cmpSRasr:
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value
	
	cmp     value,src1 		; CMP 
	mov		ccr,wccr		; might need to mask ccr with 0xF if ccr's high bits aren't 0
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

cmpSRror:
	mov     warm(pc),src1	; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1 ; Value stored in src1 -> src1
	
	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value
	
	cmp     value,src1 		; CMP 
	mov		ccr,wccr		; might need to mask ccr with 0xF if ccr's high bits aren't 0
	mov     wpc,pc			; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

;;; EOR EOREOREOREOR

eorIM:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),value
	and     $ones9,value
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp
	add     $1,wpc
	shl     exp,value
	
	xor     value,src1 			; EOR
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

eorsIM:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),value
	and     $ones9,value
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp
	add     $1,wpc
	shl     exp,value
	
	xor     value,src1 			; EORS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

eorSClsl:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
	xor     value,src1 			; EOR
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

eorsSClsl:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count

	xor     value,src1 			; EORS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

eorSClsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	xor     value,src1 			; EOR
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

eorsSClsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	xor     value,src1 			; EORS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

eorSCasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value

	xor     value,src1 			; EOR
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

eorsSCasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value

	xor     value,src1 			; EORS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

eorSCror:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value 	; right source after ror, stored in value
	
	xor     value,src1 			; EOR
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

eorsSCror:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value 	; right source after ror, stored in value    

	xor     value,src1 			; EORS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

eorSRlsl:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg   ; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value
	
	xor     value,src1 			; EOR
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

eorsSRlsl:
	mov     warm(pc),dest    	; Decoding 1st Half
	shr     $19,dest	; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg	; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value
	
	xor     value,src1 			; EORS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

eorSRlsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg     ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value

	xor     value,src1 			; EOR
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

eorsSRlsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg     ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value
	
	xor     value,src1 			; EORS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

eorSRasr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value
	
	xor     value,src1 			; EOR
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

eorsSRasr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value

	xor     value,src1 			; EORS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

eorSRror:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value

	xor     value,src1 			; EOR
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

eorsSRror:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value

	xor     value,src1 			; EORS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

;;; ORR ORRORRORR
orrIM:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),value
	and     $ones9,value
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp
	add     $1,wpc
	shl     exp,value
	
	or    	value,src1	 		; ORR
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

orrsIM:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),value
	and     $ones9,value
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp
	add     $1,wpc
	shl     exp,value
	
	or      value,src1 			; ORRS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

orrSClsl:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count

	or    	value,src1	 		; ORR
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

orrsSClsl:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count

	or      value,src1 			; ORRS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

orrSClsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value

	or    	value,src1	 		; ORR
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

orrsSClsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value

	or      value,src1 			; ORRS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

orrSCasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value

	or    	value,src1	 		; ORR
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

orrsSCasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value

	or      value,src1 			; ORRS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

orrSCror:
	mov     warm(pc),dest    	; Decoding 1st Half
	shr     $19,dest	; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value 	; right source after ror, stored in value

	or    	value,src1	 		; ORR
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

orrsSCror:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value	 ; right source after ror, stored in value

	or      value,src1 			; ORRS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

orrSRlsl:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg   ; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value

	or    	value,src1	 		; ORR
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

orrsSRlsl:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg   ; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value

	or      value,src1 			; ORRS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

orrSRlsr:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg  ; LSR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value

	or    	value,src1	 		; ORR
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

orrsSRlsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg	 ; LSR register
	and     $0xF,shReg	 ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value

	or      value,src1 			; ORRS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

orrSRasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value

	or    	value,src1	 		; ORR
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

orrsSRasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value

	or      value,src1 			; ORRS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

orrSRror:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value

	or    	value,src1	 		; ORR
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

orrsSRror:mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value

	or      value,src1 			; ORRS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table
	
;;; AND andandandand
andIM:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),value
	and     $ones9,value
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp
	add     $1,wpc
	shl     exp,value
	
	and     value,src1	 			; AND
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

andsIM:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),value
	and     $ones9,value
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp
	add     $1,wpc
	shl     exp,value

	and     value,src1 				; ANDS
	mov     ccr,wccr   				; update ccr
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc			 		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

andSClsl:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count

	and     value,src1	 			; AND
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

andsSClsl:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count

	and     value,src1 				; ANDS
	mov     ccr,wccr   				; update ccr
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc			 		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

andSClsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	and     value,src1	 			; AND
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

andsSClsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value

	and     value,src1 				; ANDS
	mov     ccr,wccr   				; update ccr
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc			 		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

andSCasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value

	and     value,src1	 			; AND
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

andsSCasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value

	and     value,src1 				; ANDS
	mov     ccr,wccr   				; update ccr
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc			 		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

andSCror:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value	 ; right source after ror, stored in value

	and     value,src1	 			; AND
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

andsSCror:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value	 ; right source after ror, stored in value

	and     value,src1 				; ANDS
	mov     ccr,wccr   				; update ccr
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc			 		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

andSRlsl:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg   ; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value

	and     value,src1	 			; AND
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

andsSRlsl:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg   ; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value

	and     value,src1 				; ANDS
	mov     ccr,wccr   				; update ccr
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc			 		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

andSRlsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg     ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value

	and     value,src1	 			; AND
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

andsSRlsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg     ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value

	and     value,src1 				; ANDS
	mov     ccr,wccr   				; update ccr
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc			 		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

andSRasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value

	and     value,src1	 			; AND
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

andsSRasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value

	and     value,src1 				; ANDS
	mov     ccr,wccr   				; update ccr
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc			 		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

andSRror:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value

	and     value,src1	 			; AND
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

andsSRror:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value

	and     value,src1 				; ANDS
	mov     ccr,wccr   				; update ccr
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc			 		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

;;; TST TSTTSTTST
tstIM:
	mov     warm(pc),src1	; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1 ; Value stored in src1 -> src1
	
	mov     warm(pc),value		; IMM value
	and     $ones9,value 		; value in r5
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp 		; exponent in r6
	add		$1,wpc
	shl     exp,value 		; immediate value in value

	and     value,src1			; TST
	mov		ccr,wccr			; possibly mask here
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

tstSClsl:
	mov     warm(pc),src1	     ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1 ; Value stored in src1 -> src1

	mov     warm(pc),shCnt	; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
	
	and     value,src1			; TST
	mov		ccr,wccr			; possibly mask here
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

tstSClsr:
	mov     warm(pc),src1	; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1 ; Value stored in src1 -> src1
	
	mov     warm(pc),shCnt	; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	and     value,src1			; TST
	mov		ccr,wccr			; possibly mask here
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

tstSCasr:
	mov     warm(pc),src1		; Only Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt  ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2    ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value  ; asr src2 by shift count, stored in value

	and     value,src1			; TST
	mov		ccr,wccr			; possibly mask here
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

tstSCror:
	mov     warm(pc),src1	; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1 ; Value stored in src1 -> src1
	
	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value	 ; right source after ror, stored in value
	
	and     value,src1			; TST
	mov		ccr,wccr			; possibly mask here
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

tstSRlsl:
	mov     warm(pc),src1	; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1 ; Value stored in src1 -> src1
	
	mov     warm(pc),shReg	; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value

	and     value,src1			; TST
	mov		ccr,wccr			; possibly mask here
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

tstSRlsr:
	mov     warm(pc),src1	; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1 ; Value stored in src1 -> src1
		
	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg     ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value
	
	and     value,src1			; TST
	mov		ccr,wccr			; possibly mask here
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

tstSRasr:
	mov     warm(pc),src1	; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1 ; Value stored in src1 -> src1
	
	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value
	
	and     value,src1			; TST
	mov		ccr,wccr			; possibly mask here
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

tstSRror:
	mov     warm(pc),src1	; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1 ; Value stored in src1 -> src1
	
	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value

	and     value,src1			; TST
	mov		ccr,wccr			; possibly mask here
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

;;; MUL MULMULMUL
mulIM:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),value
	and     $ones9,value
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp
	add     $1,wpc
	shl     exp,value
	
	mul     value,src1	 			; MUL
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

mulsIM:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),value
	and     $ones9,value
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp
	add     $1,wpc
	shl     exp,value

	mul     value,src1 				; MULS
	mov     ccr,wccr   				; update ccr
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc			 		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

mulSClsl:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count

	mul     value,src1	 			; MUL
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

mulsSClsl:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count

	mul     value,src1 				; MULS
	mov     ccr,wccr   				; update ccr
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc			 		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

mulSClsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value

	mul     value,src1	 			; MUL
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

mulsSClsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	mul     value,src1 				; MULS
	mov     ccr,wccr   				; update ccr
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc			 		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

mulSCasr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value

	mul     value,src1	 			; MUL
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

mulsSCasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value

	mul     value,src1 				; MULS
	mov     ccr,wccr   				; update ccr
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc			 		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

mulSCror:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value	 ; right source after ror, stored in value

	mul     value,src1	 			; MUL
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

mulsSCror:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value	 ; right source after ror, stored in value

	mul     value,src1 				; MULS
	mov     ccr,wccr   				; update ccr
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc			 		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

mulSRlsl:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg   ; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value

	mul     value,src1	 			; MUL
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

mulsSRlsl:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg   ; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value

	mul     value,src1 				; MULS
	mov     ccr,wccr   				; update ccr
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc			 		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

mulSRlsr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg     ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value

	mul     value,src1	 			; MUL
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

mulsSRlsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg     ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value

	mul     value,src1 				; MULS
	mov     ccr,wccr   				; update ccr
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc			 		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

mulSRasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value

	mul     value,src1	 			; MUL
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

mulsSRasr:
	mov     warm(pc),dest    	; Decoding 1st Half
	shr     $19,dest	; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value
	
	mul     value,src1 				; MULS
	mov     ccr,wccr   				; update ccr
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc			 		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

mulSRror:
	mov     warm(pc),dest 	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value

	mul     value,src1	 			; MUL
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

mulsSRror:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value

	mul     value,src1 				; MULS
	mov     ccr,wccr   				; update ccr
	mov     src1,wregs(dest) 		; update WARM reg
	mov     wpc,pc			 		; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

;;; DIV DIVDIVDIV
divIM:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),value
	and     $ones9,value
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp
	add     $1,wpc
	shl     exp,value
	
	div     value,src1	 		; DIV
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	;jump to condition data table

divsIM:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),value
	and     $ones9,value
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp
	add     $1,wpc
	shl     exp,value
	
	div     value,src1 			; DIVS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

divSClsl:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count

	div     value,src1	 		; DIV
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	;jump to condition data table

divsSClsl:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count

	div     value,src1 			; DIVS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

divSClsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value

	div     value,src1	 		; DIV
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	;jump to condition data table

divsSClsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value

	div     value,src1 			; DIVS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

divSCasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value

	div     value,src1	 		; DIV
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	;jump to condition data table

divsSCasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value

	div     value,src1 			; DIVS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

divSCror:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value	 ; right source after ror, stored in value    

	div     value,src1	 		; DIV
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	;jump to condition data table

divsSCror:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value	 ; right source after ror, stored in value
	
	div     value,src1 			; DIVS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

divSRlsl:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg   ; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value

	div     value,src1	 		; DIV
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	;jump to condition data table

divsSRlsl:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg   ; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value

	div     value,src1 			; DIVS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

divSRlsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg     ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value

	div     value,src1	 		; DIV
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	;jump to condition data table

divsSRlsr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg     ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value

	div     value,src1 			; DIVS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

divSRasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value

	div     value,src1	 		; DIV
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	;jump to condition data table

divsSRasr:
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value

	div     value,src1 			; DIVS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

divSRror:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value

	div     value,src1	 		; DIV
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	;jump to condition data table

divsSRror:
	mov     warm(pc),dest	 ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value

	div     value,src1 			; DIVS
	mov     ccr,wccr   			; update ccr
	mov     src1,wregs(dest) 	; update WARM reg
	mov     wpc,pc			 	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table
	
;;; MOV -- no src reg MOVMOVMOV
movIM:
	mov     warm(pc),dest	; Decoding 1st Half for MOV/MOVN
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest

	mov     warm(pc),value	; IMM value
	and     $ones9,value ; value in r5
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp ; exponent in r6
	add     $1,wpc
	shl     exp,value ; immediate value in value
	
	mov     value,wregs(dest) 		; MOV value to destination
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
movsIM:
	mov     warm(pc),dest	; Decoding 1st Half for MOV/MOVN
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest

	mov     warm(pc),value	; IMM value
	and     $ones9,value ; value in r5
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp ; exponent in r6
	add     $1,wpc
	shl     exp,value ; immediate value in value
	
	or		$0,value		; sets condition code 
	mov		ccr,wccr
	mov		value,wregs(dest)
	mov     wpc,pc          	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

movSClsl:
	mov     warm(pc),dest	; Decoding 1st Half for MOV/MOVN
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	
	mov     warm(pc),shCnt	; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
	

	mov     value,wregs(dest) 		; MOV value to destination
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
movsSClsl:
	mov     warm(pc),dest   ; Decoding 1st Half for MOV/MOVN
	shr     $19,dest 	; Dest Reg -> dest
	and     $0xF,dest

	mov     warm(pc),shCnt  ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value 	; lsl that value by shift count
	
	or		$0,value		; sets condition code 
	mov		ccr,wccr
	mov		value,wregs(dest)
	mov     wpc,pc          	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

movSClsr:
	mov     warm(pc),dest	; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest		; Dest Reg -> dest
	and     $0xF,dest

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	
	mov     value,wregs(dest) 		; MOV value to destination
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
movsSClsr:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
		
	or		$0,value		; sets condition code 
	mov		ccr,wccr
	mov		value,wregs(dest)
	mov     wpc,pc          	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

movSCasr:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
		
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value
	

	mov     value,wregs(dest) 		; MOV value to destination
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
movsSCasr:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value
	

	or		$0,value		; sets condition code 
	mov		ccr,wccr
	mov		value,wregs(dest)
	mov     wpc,pc          	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

movSCror:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
		
	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value 	; right source after ror, stored in value
		
	mov     value,wregs(dest) 		; MOV value to destination
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
movsSCror:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
		
	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value 	; right source after ror, stored in value

	or		$0,value		; sets condition code 
	mov		ccr,wccr
	mov		value,wregs(dest)
	mov     wpc,pc          	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

movSRlsl:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest

	mov     warm(pc),shReg   ; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value

	mov     value,wregs(dest) 		; MOV value to destination
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
movsSRlsl:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	
	mov     warm(pc),shReg   ; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value

	or		$0,value		; sets condition code 
	mov		ccr,wccr
	mov		value,wregs(dest)
	mov     wpc,pc          	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

movSRlsr:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
		
	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg     ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value
	
	mov     value,wregs(dest) 		; MOV value to destination
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
movsSRlsr:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	
	mov     warm(pc),dest    ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg     ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value
	
	or		$0,value		; sets condition code 
	mov		ccr,wccr
	mov		value,wregs(dest)
	mov     wpc,pc          	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

movSRasr:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	
	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value      

	mov     value,wregs(dest) 		; MOV value to destination
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
movsSRasr:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
		
	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value
	
	or		$0,value		; sets condition code 
	mov		ccr,wccr
	mov		value,wregs(dest)
	mov     wpc,pc          	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

movSRror:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	
	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value

	mov     value,wregs(dest) 		; MOV value to destination
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
movsSRror:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	
	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value

	or		$0,value		; sets condition code 
	mov		ccr,wccr
	mov		value,wregs(dest)
	mov     wpc,pc          	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

;;; MOVN -- no src reg MVNMVN
mvnIM:
	mov     warm(pc),dest	     ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest     ; Dest Reg -> dest
	and     $0xF,dest

	mov     warm(pc),value	; IMM value
	and     $ones9,value ; value in r5
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp ; exponent in r6
	add     $1,wpc
	shl     exp,value ; immediate value in value
	
	xor		$0xffffffff,value	; MVN
	mov     value,wregs(dest) 	; MOV value to destination
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
mvnsIM:	
	mov     warm(pc),dest	; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest

	mov     warm(pc),value ; IMM value
	and     $ones9,value   ; value in r5
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp ; exponent in r6
	add     $1,wpc
	shl     exp,value ; immediate value in value
	
	xor     $0xffffffff,value
	mov		ccr,wccr
	mov     value,wregs(dest) ; MOV value to destination
	mov     wpc,pc			  ; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

mvnSClsl:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest 	; Dest Reg -> dest
	and     $0xF,dest
		
	mov     warm(pc),shCnt	; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
	
	xor		$0xffffffff,value	; MVN
	mov     value,wregs(dest) 	; MOV value to destination
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
mvnsSClsl:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest        ; Dest Reg -> dest
	and     $0xF,dest

	mov     warm(pc),shCnt  ; LSL Immediate
	and     $0x3F,shCnt 	;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
	
	xor     $0xffffffff,value
	mov		ccr,wccr
	mov     value,wregs(dest) ; MOV value to destination
	mov     wpc,pc			  ; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

mvnSClsr:
	mov     warm(pc),dest	; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	xor	$0xffffffff,value	; MVN
	mov     value,wregs(dest) 	; MOV value to destination
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
mvnsSClsr:
	mov     warm(pc),dest	; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	
	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt  ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2    ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value  ; lsl src2 by shift count, stored in value
		
	xor     $0xffffffff,value
	mov		ccr,wccr
	mov     value,wregs(dest) ; MOV value to destination
	mov     wpc,pc			  ; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

mvnSCasr:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
			
	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value
	
	xor		$0xffffffff,value	; MVN
	mov     value,wregs(dest) 	; MOV value to destination
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
mvnsSCasr:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
		
	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value

	xor     $0xffffffff,value
	mov		ccr,wccr
	mov     value,wregs(dest) ; MOV value to destination
	mov     wpc,pc			  ; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

mvnSCror:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
		
	mov     warm(pc),shCnt	  ; ROR immediate
	and     $0x3F,shCnt	  ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2	  ; src reg 2 in src
	mov     wregs(src2),temp  ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value		  ; right source after ror, stored in value
	
	xor		$0xffffffff,value	; MVN
	mov     value,wregs(dest) 	; MOV value to destination
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
mvnsSCror:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
		
	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value	 ; right source after ror, stored in value
	
	xor     $0xffffffff,value
	mov		ccr,wccr
	mov     value,wregs(dest) ; MOV value to destination
	mov     wpc,pc			  ; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

mvnSRlsl:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	
	mov     warm(pc),shReg   ; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value


	xor		$0xffffffff,value	; MVN
	mov     value,wregs(dest) 	; MOV value to destination
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
mvnsSRlsl:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	
	mov     warm(pc),shReg   ; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value

	xor     $0xffffffff,value
	mov		ccr,wccr
	mov     value,wregs(dest) ; MOV value to destination
	mov     wpc,pc			  ; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

mvnSRlsr:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
		
	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg     ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value
	
	xor		$0xffffffff,value	; MVN
	mov     value,wregs(dest) 	; MOV value to destination
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
mvnsSRlsr:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
		
	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg     ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value
	
	xor     $0xffffffff,value
	mov		ccr,wccr
	mov     value,wregs(dest) ; MOV value to destination
	mov     wpc,pc			  ; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

mvnSRasr:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	
	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value
		

	xor		$0xffffffff,value	; MVN
	mov     value,wregs(dest) 	; MOV value to destination
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
mvnsSRasr:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	
	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value
	
	xor     $0xffffffff,value
	mov		ccr,wccr
	mov     value,wregs(dest) ; MOV value to destination
	mov     wpc,pc			  ; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

mvnSRror:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	
	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value
	
	xor		$0xffffffff,value	; MVN
	mov     value,wregs(dest) 	; MOV value to destination
	mov     wpc,pc				; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
mvnsSRror:
	mov     warm(pc),dest   ; Only decode Dest Reg (MOV/MOVN)
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	
	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value
	
	xor     $0xffffffff,value
	mov		ccr,wccr
	mov     value,wregs(dest) ; MOV value to destination
	mov     wpc,pc			  ; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table

;;; LDR -- load register LDRLDRLDR
ldrIM:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
			
	mov     warm(pc),value          ; IMM signed offset from register
	sal     $18,value
	add     $1,wpc
	sar     $18,value      ; extend sign bit
	
	add     value,base				; LDR
	and     $ones24,base 			; mask effective address
	mov     warm(base),wregs(dest) 	; mov address to dest
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
ldrsIM:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
		
	mov     warm(pc),value ; IMM signed offset from register
	sal     $18,value
	add     $1,wpc
	sar     $18,value ; extend sign bit
	
	add     value,base				; LDRS
	and     $ones24,base 			; mask effective address
	or		$0,warm(base)
	mov		ccr,wccr				; condition code bits set
	mov     warm(base),wregs(dest) 	; mov address to dest
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

ldrSClsl:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
		
	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
	
	add     value,base				; LDR
	and     $ones24,base 			; mask effective address
	mov     warm(base),wregs(dest) 	; mov address to dest
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table
	
ldrsSClsl:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
		
	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count

	add     value,base				; LDRS
	and     $ones24,base 			; mask effective address
	or		$0,warm(base)
	mov		ccr,wccr				; condition code bits set
	mov     warm(base),wregs(dest) 	; mov address to dest
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

ldrSClsr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
		
	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	add     value,base				; LDR
	and     $ones24,base 			; mask effective address
	mov     warm(base),wregs(dest) 	; mov address to dest
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table
	
ldrsSClsr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
		
	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	add     value,base				; LDRS
	and     $ones24,base 			; mask effective address
	or		$0,warm(base)
	mov		ccr,wccr				; condition code bits set
	mov     warm(base),wregs(dest) 	; mov address to dest
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

ldrSCasr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
		
	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value
	

	add     value,base				; LDR
	and     $ones24,base 			; mask effective address
	mov     warm(base),wregs(dest) 	; mov address to dest
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table
	
ldrsSCasr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
		
	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value
	
	add     value,base				; LDRS
	and     $ones24,base 			; mask effective address
	or		$0,warm(base)
	mov		ccr,wccr				; condition code bits set
	mov     warm(base),wregs(dest) 	; mov address to dest
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

ldrSCror:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
			
	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value 	; right source after ror, stored in value    

	add     value,base				; LDR
	and     $ones24,base 			; mask effective address
	mov     warm(base),wregs(dest) 	; mov address to dest
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table
	
ldrsSCror:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
		
	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value 	; right source after ror, stored in value    

	add     value,base				; LDRS
	and     $ones24,base 			; mask effective address
	or		$0,warm(base)
	mov		ccr,wccr				; condition code bits set
	mov     warm(base),wregs(dest) 	; mov address to dest
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
;;; STR -- store register - STRSTRSTR
strIM:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
		
	mov     warm(pc),value	; IMM signed offset from register
	sal     $18,value
	add     $1,wpc
	sar     $18,value ; extend sign bit
	
	add     value,base				; STR
	and     $ones24,base 			; mask effective address -- might be able to optimizing using reg. ind. with index
	mov     wregs(dest),warm(base)	; store dest reg in memory loc
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
strsIM:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
	
	mov     warm(pc),value ; IMM signed offset from register
	sal     $18,value
	add     $1,wpc
	sar     $18,value ; extend sign bit
	
	add     value,base				; STRS
	and     $ones24,base 			; mask effective address -- might be able to optimizing using reg. ind. with index
	or		$0,wregs(dest)
	mov		ccr,wccr	       		; condition code bits set
	mov		wregs(dest),warm(base) 	; store dest reg in memory loc
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

strSClsl:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
		
	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
	
	add     value,base				; STR
	and     $ones24,base 			; mask effective address -- might be able to optimizing using reg. ind. with index
	mov     wregs(dest),warm(base)	; store dest reg in memory loc
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
strsSClsl:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
		
	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
	
	add     value,base				; STRS
	and     $ones24,base 			; mask effective address -- might be able to optimizing using reg. ind. with index
	or	$0,wregs(dest)
	mov	ccr,wccr	       		; condition code bits set
	mov	wregs(dest),warm(base) 	; store dest reg in memory loc
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

strSClsr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
		
	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	add     value,base				; STR
	and     $ones24,base 			; mask effective address -- might be able to optimizing using reg. ind. with index
	mov     wregs(dest),warm(base)	; store dest reg in memory loc
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
strsSClsr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
		
	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	add     value,base				; STRS
	and     $ones24,base 			; mask effective address -- might be able to optimizing using reg. ind. with index
	or		$0,wregs(dest)
	mov		ccr,wccr	       		; condition code bits set
	mov		wregs(dest),warm(base) 	; store dest reg in memory loc
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

strSCasr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
		
	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value
	
	add     value,base				; STR
	and     $ones24,base 			; mask effective address -- might be able to optimizing using reg. ind. with index
	mov     wregs(dest),warm(base)	; store dest reg in memory loc
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
strsSCasr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
	
	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value
	
	add     value,base				; STRS
	and     $ones24,base 			; mask effective address -- might be able to optimizing using reg. ind. with index
	or		$0,wregs(dest)
	mov		ccr,wccr	       		; condition code bits set
	mov		wregs(dest),warm(base) 	; store dest reg in memory loc
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

strSCror:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value 	; right source after ror, stored in value
	
	add     value,base				; STR
	and     $ones24,base 			; mask effective address -- might be able to optimizing using reg. ind. with index
	mov     wregs(dest),warm(base)	; store dest reg in memory loc
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
strsSCror:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value 	; right source after ror, stored in value    

	add     value,base				; STRS
	and     $ones24,base 			; mask effective address -- might be able to optimizing using reg. ind. with index
	or		$0,wregs(dest)
	mov		ccr,wccr	       		; condition code bits set
	mov		wregs(dest),warm(base) 	; store dest reg in memory loc
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
;;; LDU -- load/pop register ldulduldu
lduIM:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1

	mov     warm(pc),value	; IMM signed offset from register
	sal     $18,value
	add     $1,wpc
	sar     $18,value ; extend sign bit
	
	jge     lduIMPos				; LDU
	add     value,base				; Offset is -
	and     $ones24,base			; effective address in base
	mov     warm(base),wregs(dest)	; value at eff addr loaded into dest register
	mov     base,wregs(src1Num)		; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
lduIMPos:
	and     $ones24,base 			; Offset is +. effective address in base
	mov     warm(base),wregs(dest) 	; value at eff addr loaded into dest register
	add		value,base				; base  += offset value
	mov     base,wregs(src1Num)		; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
ldusIM:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1

	mov     warm(pc),value	; IMM signed offset from register
	sal     $18,value
	add     $1,wpc
	sar     $18,value ; extend sign bit
	
	jge     ldusIMPos					; LDUS, Offset is Positive
	add     value,base
	and     $ones24,base 			; effective address in base
	or		$0,warm(base)
	mov		ccr,wccr	       		; condition code bits set
	mov     warm(base),wregs(dest) 	; value at eff addr loaded into dest register
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc			    	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
ldusIMPos:
	and     $ones24,base			; Offset + . effective address in base
	or		$0,warm(base)
	mov		ccr,wccr	       		; condition code bits set
	mov     warm(base),wregs(dest) 	; value at eff addr loaded into dest register
	add		value,base     			; base  += offset value
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
lduSClsl:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
	
	jge     lduSClslPos					; LDU
	add     value,base				; Offset is -
	and     $ones24,base			; effective address in base
	mov     warm(base),wregs(dest)	; value at eff addr loaded into dest register
	mov     base,wregs(src1Num)		; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
lduSClslPos:
	and     $ones24,base 			; Offset is +. effective address in base
	mov     warm(base),wregs(dest) 	; value at eff addr loaded into dest register
	add		value,base				; base  += offset value
	mov     base,wregs(src1Num)		; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
ldusSClsl:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
	

	jge     ldusSClslPos			; LDUS, Offset is Positive
	add     value,base
	and     $ones24,base 			; effective address in base
	or		$0,warm(base)
	mov		ccr,wccr	       		; condition code bits set
	mov     warm(base),wregs(dest) 	; value at eff addr loaded into dest register
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc			    	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
ldusSClslPos:
	and     $ones24,base			; Offset + . effective address in base
	or		$0,warm(base)
	mov		ccr,wccr	       		; condition code bits set
	mov     warm(base),wregs(dest) 	; value at eff addr loaded into dest register
	add		value,base     			; base  += offset value
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

lduSClsr:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	jge     lduSClsrPos					; LDU
	add     value,base				; Offset is -
	and     $ones24,base			; effective address in base
	mov     warm(base),wregs(dest)	; value at eff addr loaded into dest register
	mov     base,wregs(src1Num)		; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
lduSClsrPos:
	and     $ones24,base 			; Offset is +. effective address in base
	mov     warm(base),wregs(dest) 	; value at eff addr loaded into dest register
	add		value,base				; base  += offset value
	mov     base,wregs(src1Num)		; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
ldusSClsr:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	jge     ldusSClsrPos			; LDUS, Offset is Positive
	add     value,base
	and     $ones24,base 			; effective address in base
	or		$0,warm(base)
	mov		ccr,wccr	       		; condition code bits set
	mov     warm(base),wregs(dest) 	; value at eff addr loaded into dest register
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc			    	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
ldusSClsrPos:
	and     $ones24,base			; Offset + . effective address in base
	or		$0,warm(base)
	mov		ccr,wccr	       		; condition code bits set
	mov     warm(base),wregs(dest) 	; value at eff addr loaded into dest register
	add		value,base     			; base  += offset value
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

lduSCasr:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value
	

	jge     lduSCasrPos					; LDU
	add     value,base				; Offset is -
	and     $ones24,base			; effective address in base
	mov     warm(base),wregs(dest)	; value at eff addr loaded into dest register
	mov     base,wregs(src1Num)		; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
lduSCasrPos:
	and     $ones24,base 			; Offset is +. effective address in base
	mov     warm(base),wregs(dest) 	; value at eff addr loaded into dest register
	add		value,base				; base  += offset value
	mov     base,wregs(src1Num)		; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
ldusSCasr:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value
	
	jge     ldusSCasrPos					; LDUS, Offset is Positive
	add     value,base
	and     $ones24,base 			; effective address in base
	or		$0,warm(base)
	mov		ccr,wccr	       		; condition code bits set
	mov     warm(base),wregs(dest) 	; value at eff addr loaded into dest register
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc			    	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
ldusSCasrPos:
	and     $ones24,base			; Offset + . effective address in base
	or		$0,warm(base)
	mov		ccr,wccr	       		; condition code bits set
	mov     warm(base),wregs(dest) 	; value at eff addr loaded into dest register
	add		value,base     			; base  += offset value
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

lduSCror:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value 	; right source after ror, stored in value    

	jge     lduSCrorPos					; LDU
	add     value,base				; Offset is -
	and     $ones24,base			; effective address in base
	mov     warm(base),wregs(dest)	; value at eff addr loaded into dest register
	mov     base,wregs(src1Num)		; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
lduSCrorPos:
	and     $ones24,base 			; Offset is +. effective address in base
	mov     warm(base),wregs(dest) 	; value at eff addr loaded into dest register
	add		value,base				; base  += offset value
	mov     base,wregs(src1Num)		; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
ldusSCror:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value 	; right source after ror, stored in value
	
	jge     ldusSCrorPos					; LDUS, Offset is Positive
	add     value,base
	and     $ones24,base 			; effective address in base
	or		$0,warm(base)
	mov		ccr,wccr	       		; condition code bits set
	mov     warm(base),wregs(dest) 	; value at eff addr loaded into dest register
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc			    	; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
ldusSCrorPos:
	and     $ones24,base			; Offset + . effective address in base
	or		$0,warm(base)
	mov		ccr,wccr	       		; condition code bits set
	mov     warm(base),wregs(dest) 	; value at eff addr loaded into dest register
	add		value,base     			; base  += offset value
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

;;; STU -- store/push register STUSTUSTU
stuIM:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1

	mov     warm(pc),value	; IMM signed offset from register
	sal     $18,value
	add     $1,wpc
	sar     $18,value ; extend sign bit
	
	jge     stuIMPos					; STU
	add     value,base				; Offset is Negative
	and     $ones24,base 			; effective address in base
	mov     wregs(dest),warm(base) 	; value in dest register stored at eff address
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
stuIMPos:
	and     $ones24,base 			; Offset is Positive
	mov     wregs(dest),warm(base) 	; value of dest register stored at eff. address
	add     value,base     		;	 base  += offset value
	mov     base,wregs(src1Num)		; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
stusIM:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest 	; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1

	mov     warm(pc),value ; IMM signed offset from register
	sal     $18,value
	add     $1,wpc
	sar     $18,value ; extend sign bit
	
	jge     stusIMPos	  			; STUS
	add     value,base			; Offset Neg
	and     $ones24,base 		; effective address in base
	or 		$0,wregs(dest)
	mov		ccr,wccr			; condition code set
	mov     wregs(dest),warm(base) 	; value in dest register stored at eff address
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table
stusIMPos:
	and     $ones24,base 		; Offset +; effective address in base
	or		$0,wregs(dest)
	mov		ccr,wccr
	mov     wregs(dest),warm(base) 	; value of dest register stored at eff. address
	add     value,base     			; base  += offset value
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

stuSClsl:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest        ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
	
	jge     stuSClslPos					; STU
	add     value,base				; Offset is Negative
	and     $ones24,base 			; effective address in base
	mov     wregs(dest),warm(base) 	; value in dest register stored at eff address
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
stuSClslPos:
	and     $ones24,base 			; Offset is Positive
	mov     wregs(dest),warm(base) 	; value of dest register stored at eff. address
	add     value,base     		;	 base  += offset value
	mov     base,wregs(src1Num)		; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
stusSClsl:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest        ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
	
	jge     stusSClslPos	  			; STUS
	add     value,base			; Offset Neg
	and     $ones24,base 		; effective address in base
	or 		$0,wregs(dest)
	mov		ccr,wccr			; condition code set
	mov     wregs(dest),warm(base) 	; value in dest register stored at eff address
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table
stusSClslPos:
	and     $ones24,base 		; Offset +; effective address in base
	or		$0,wregs(dest)
	mov		ccr,wccr
	mov     wregs(dest),warm(base) 	; value of dest register stored at eff. address
	add     value,base     			; base  += offset value
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

stuSClsr:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest        ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	jge     stuSClsrPos					; STU
	add     value,base				; Offset is Negative
	and     $ones24,base 			; effective address in base
	mov     wregs(dest),warm(base) 	; value in dest register stored at eff address
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
stuSClsrPos:
	and     $ones24,base 			; Offset is Positive
	mov     wregs(dest),warm(base) 	; value of dest register stored at eff. address
	add     value,base     		;	 base  += offset value
	mov     base,wregs(src1Num)		; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
stusSClsr:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest        ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	jge     stusSClsrPos	  			; STUS
	add     value,base			; Offset Neg
	and     $ones24,base 		; effective address in base
	or 	$0,wregs(dest)
	mov	ccr,wccr			; condition code set
	mov     wregs(dest),warm(base) 	; value in dest register stored at eff address
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table
stusSClsrPos:
	and     $ones24,base 		; Offset +; effective address in base
	or		$0,wregs(dest)
	mov		ccr,wccr
	mov     wregs(dest),warm(base) 	; value of dest register stored at eff. address
	add     value,base     			; base  += offset value
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

stuSCasr:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest        ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value
	
	jge     stuSCasrPos					; STU
	add     value,base				; Offset is Negative
	and     $ones24,base 			; effective address in base
	mov     wregs(dest),warm(base) 	; value in dest register stored at eff address
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
stuSCasrPos:
	and     $ones24,base 			; Offset is Positive
	mov     wregs(dest),warm(base) 	; value of dest register stored at eff. address
	add     value,base     		;	 base  += offset value
	mov     base,wregs(src1Num)		; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
stusSCasr:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest        ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value
	
	jge     stusSCasrPos	  			; STUS
	add     value,base			; Offset Neg
	and     $ones24,base 		; effective address in base
	or 		$0,wregs(dest)
	mov		ccr,wccr			; condition code set
	mov     wregs(dest),warm(base) 	; value in dest register stored at eff address
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table
stusSCasrPos:
	and     $ones24,base 		; Offset +; effective address in base
	or		$0,wregs(dest)
	mov		ccr,wccr
	mov     wregs(dest),warm(base) 	; value of dest register stored at eff. address
	add     value,base     			; base  += offset value
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table

stuSCror:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest        ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value 	; right source after ror, stored in value    

	jge     stuSCrorPos					; STU
	add     value,base				; Offset is Negative
	and     $ones24,base 			; effective address in base
	mov     wregs(dest),warm(base) 	; value in dest register stored at eff address
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
stuSCrorPos:
	and     $ones24,base 			; Offset is Positive
	mov     wregs(dest),warm(base) 	; value of dest register stored at eff. address
	add     value,base     		;	 base  += offset value
	mov     base,wregs(src1Num)		; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip ; jump to condition data table
	
stusSCror:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest        ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1Num ; Source Reg -> src1Num
	shr     $15,src1Num
	and     $0xF,src1Num
	mov     wregs(src1Num),src1 ; Value stored in src1Num -> src1
	
	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value 	; right source after ror, stored in value    

	jge     stusSCrorPos	  			; STUS
	add     value,base			; Offset Neg
	and     $ones24,base 		; effective address in base
	or 		$0,wregs(dest)
	mov		ccr,wccr			; condition code set
	mov     wregs(dest),warm(base) 	; value in dest register stored at eff address
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table
stusSCrorPos:
	and     $ones24,base 		; Offset +; effective address in base
	or		$0,wregs(dest)
	mov		ccr,wccr
	mov     wregs(dest),warm(base) 	; value of dest register stored at eff. address
	add     value,base     			; base  += offset value
	mov     base,wregs(src1Num) 	; eff addr stored in base register
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 	; jump to condition data table
	
;;; ADR -- form address ADRADRADR
adrIM:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
		
	mov     warm(pc),value	    ; IMM signed offset from register
	sal     $18,value
	add     $1,wpc
	sar     $18,value    ; extend sign bit
	
	lea	0(value,base),wregs(dest) 	; ADR: base = base reg + value computed
	and	$ones24,wregs(dest)			; mask effective address
	mov     wpc,pc						; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
adrsIM:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
	
	mov     warm(pc),value ; IMM signed offset from register
	sal     $18,value
	add     $1,wpc
	sar     $18,value ; extend sign bit
	
	add		value,base				; ADRS:  base = base reg + value computed
	and		$ones24,base 			; masked effective address
	mov		ccr,wccr				; store condition code bits
	mov     base,wregs(dest) 		; mov address to dest
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

adrSClsl:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
	
	lea	0(value,base),wregs(dest) 	; ADR: base = base reg + value computed
	and	$ones24,wregs(dest)			; mask effective address
	mov     wpc,pc						; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
adrsSClsl:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
	
	add		value,base				; ADRS:  base = base reg + value computed
	and		$ones24,base 			; masked effective address
	mov		ccr,wccr				; store condition code bits
	mov     base,wregs(dest) 		; mov address to dest
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

adrSClsr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	lea		0(value,base),wregs(dest) 	; ADR: base = base reg + value computed
	and		$ones24,wregs(dest)			; mask effective address
	mov     wpc,pc						; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
adrsSClsr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	add		value,base				; ADRS:  base = base reg + value computed
	and		$ones24,base 			; masked effective address
	mov		ccr,wccr				; store condition code bits
	mov     base,wregs(dest) 		; mov address to dest
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

adrSCasr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value
	
	lea		0(value,base),wregs(dest) 	; ADR: base = base reg + value computed
	and		$ones24,wregs(dest)			; mask effective address
	mov     wpc,pc						; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
adrsSCasr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value
	
	add		value,base				; ADRS:  base = base reg + value computed
	and		$ones24,base 			; masked effective address
	mov		ccr,wccr				; store condition code bits
	mov     base,wregs(dest) 		; mov address to dest
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

adrSCror:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value 	; right source after ror, stored in value    

	lea		0(value,base),wregs(dest) 	; ADR: base = base reg + value computed
	and		$ones24,wregs(dest)			; mask effective address
	mov     wpc,pc						; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table
	
adrsSCror:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value 	; right source after ror, stored in value    

	add		value,base				; ADRS:  base = base reg + value computed
	and		$ones24,base 			; masked effective address
	mov		ccr,wccr				; store condition code bits
	mov     base,wregs(dest) 		; mov address to dest
	mov     wpc,pc					; TOP OF INSTRUCTION FETCH LOOP
	and     $ones24,pc
	mov     warm(pc),ir
	shr     $29,ir
	mov     condjmp(ir),rip 		; jump to condition data table

;;; LDM -- load multiple registers ldmldm
ldmIM:	
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),value		; IMM value
	and     $ones9,value 		; value in r5
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp 		; exponent in r6
	add		$1,wpc
	shl     exp,value 		; immediate value in value

	jmp		ldm

ldmSClsl:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
	
	jmp		ldm

ldmSClsr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	jmp		ldm

ldmSCasr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value
	
	jmp		ldm

ldmSCror:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value 	; right source after ror, stored in value    

	jmp		ldm

ldmSRlsl:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg	; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value
	
	jmp		ldm

ldmSRlsr:
	mov     warm(pc),dest   ; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value
		
	jmp		ldm

ldmSRasr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value
		
	jmp 	ldm
	
	
ldmSRror:
	mov     warm(pc),dest 	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value
	
	jmp	ldm

;;; STM -- store multiple registers stmstm
stmIM:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1
	
	mov     warm(pc),value		; IMM value
	and     $ones9,value 		; value in r5
	mov     warm(pc),exp
	shr     $9,exp
	and     $0x1F,exp 		; exponent in r6
	add		$1,wpc
	shl     exp,value 		; immediate value in value

	jmp		stm

stmSClsl:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSL Immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value ; memory stored in src reg2 to value
	add     $1,wpc
	shl     shCnt,value ; lsl that value by shift count
	
	jmp		stm

stmSClsr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; LSR immediate
	and     $0x3F,shCnt ; shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	shr     shCnt,value ; lsl src2 by shift count, stored in value
	
	jmp		stm

stmSCasr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; AR immediate
	and     $0x3F,shCnt ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),value
	add     $1,wpc
	sar     shCnt,value ; asr src2 by shift count, stored in value
	

	jmp		stm

stmSCror:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shCnt ; ROR immediate
	and     $0x3F,shCnt    ;shift count in shCnt
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift count
	mov     wregs(src2),value
	shl     shCnt,temp
	shr     shCnt,value
	add     $1,wpc
	add     temp,value 	; right source after ror, stored in value    

	jmp		stm

stmSRlsl:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg	; LSL register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shl     wregs(shReg),value ; lsl src2 by shift reg, stored in value
	
	jmp		stm

stmSRlsr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; LSR register
	and     $0xF,shReg     ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	shr     wregs(shReg),value ; lsr src2 by shift reg, stored in value
	
	jmp		stm

stmSRasr:
	mov     warm(pc),dest	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ASR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ;src reg 2 in src2
	mov     wregs(src2),value
	add     $1,wpc
	sar     wregs(shReg),value ; asr src2 by shift reg, stored in value
	
	jmp 	stm

stmSRror:
	mov     warm(pc),dest 	; Decoding 1st Half
	shr     $19,dest ; Dest Reg -> dest
	and     $0xF,dest
	mov     warm(pc),src1 ; Source Reg -> src1
	shr     $15,src1
	and     $0xF,src1
	mov     wregs(src1),src1

	mov     warm(pc),shReg ; ROR register
	and     $0xF,shReg ;shift reg in shReg
	mov     warm(pc),src2
	shr     $6,src2
	and     $0xF,src2 ; src reg 2 in src
	mov     wregs(src2),temp ; ror src2 by shift reg, stored in value
	mov     wregs(src2),value
	shl     wregs(shReg),temp
	shr     wregs(shReg),value
	add     $1,wpc
	add     temp,value
	
	jmp	stm

;;; HALT
halt:	trap	$SysHalt	

;;; DATA
;;; determines whether to run code based on condition bits, jumps based on bits 31-29
condjmp:
	.data	instdec,nv,eq,ne,lt,le,ge,gt
	
;;; jmp table for 11 bits (1 's' flag + 5 opcode and 5 mode code)
;;; Assume SysPLA put 5 mode shift codes on high 5 bits;
;;; OPJMPTABLE
opjmp:
	;;; Mode 00000 ~ 01111 (total, 16) 
	.data   addIM,adcIM,subIM,cmpIM,eorIM,orrIM,andIM,tstIM ; 1
	.data   mulIM,mla,divIM,movIM,mvnIM,swi,ldmIM,stmIM
	.data   ldrIM,strIM,lduIM,stuIM,adrIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsIM,adcsIM,subsIM,cmpIM,eorsIM,orrsIM,andsIM,tstIM
	.data   mulsIM,mlas,divsIM,movsIM,mvnsIM,swis,ldmIM,stmIM
	.data   ldrsIM,strsIM,ldusIM,stusIM,adrsIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	.data   addIM,adcIM,subIM,cmpIM,eorIM,orrIM,andIM,tstIM ; 2
	.data   mulIM,mla,divIM,movIM,mvnIM,swi,ldmIM,stmIM
	.data   ldrIM,strIM,lduIM,stuIM,adrIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsIM,adcsIM,subsIM,cmpIM,eorsIM,orrsIM,andsIM,tstIM
	.data   mulsIM,mlas,divsIM,movsIM,mvnsIM,swis,ldmIM,stmIM
	.data   ldrsIM,strsIM,ldusIM,stusIM,adrsIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	.data   addIM,adcIM,subIM,cmpIM,eorIM,orrIM,andIM,tstIM ;3
	.data   mulIM,mla,divIM,movIM,mvnIM,swi,ldmIM,stmIM
	.data   ldrIM,strIM,lduIM,stuIM,adrIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsIM,adcsIM,subsIM,cmpIM,eorsIM,orrsIM,andsIM,tstIM
	.data   mulsIM,mlas,divsIM,movsIM,mvnsIM,swis,ldmIM,stmIM
	.data   ldrsIM,strsIM,ldusIM,stusIM,adrsIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	.data   addIM,adcIM,subIM,cmpIM,eorIM,orrIM,andIM,tstIM ; 4
	.data   mulIM,mla,divIM,movIM,mvnIM,swi,ldmIM,stmIM
	.data   ldrIM,strIM,lduIM,stuIM,adrIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsIM,adcsIM,subsIM,cmpIM,eorsIM,orrsIM,andsIM,tstIM
	.data   mulsIM,mlas,divsIM,movsIM,mvnsIM,swis,ldmIM,stmIM
	.data   ldrsIM,strsIM,ldusIM,stusIM,adrsIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	.data   addIM,adcIM,subIM,cmpIM,eorIM,orrIM,andIM,tstIM ; 5
	.data   mulIM,mla,divIM,movIM,mvnIM,swi,ldmIM,stmIM
	.data   ldrIM,strIM,lduIM,stuIM,adrIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsIM,adcsIM,subsIM,cmpIM,eorsIM,orrsIM,andsIM,tstIM
	.data   mulsIM,mlas,divsIM,movsIM,mvnsIM,swis,ldmIM,stmIM
	.data   ldrsIM,strsIM,ldusIM,stusIM,adrsIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	.data   addIM,adcIM,subIM,cmpIM,eorIM,orrIM,andIM,tstIM ; 6
	.data   mulIM,mla,divIM,movIM,mvnIM,swi,ldmIM,stmIM
	.data   ldrIM,strIM,lduIM,stuIM,adrIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsIM,adcsIM,subsIM,cmpIM,eorsIM,orrsIM,andsIM,tstIM
	.data   mulsIM,mlas,divsIM,movsIM,mvnsIM,swis,ldmIM,stmIM
	.data   ldrsIM,strsIM,ldusIM,stusIM,adrsIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	.data   addIM,adcIM,subIM,cmpIM,eorIM,orrIM,andIM,tstIM ; 7
	.data   mulIM,mla,divIM,movIM,mvnIM,swi,ldmIM,stmIM
	.data   ldrIM,strIM,lduIM,stuIM,adrIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsIM,adcsIM,subsIM,cmpIM,eorsIM,orrsIM,andsIM,tstIM
	.data   mulsIM,mlas,divsIM,movsIM,mvnsIM,swis,ldmIM,stmIM
	.data   ldrsIM,strsIM,ldusIM,stusIM,adrsIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	.data   addIM,adcIM,subIM,cmpIM,eorIM,orrIM,andIM,tstIM ; 8
	.data   mulIM,mla,divIM,movIM,mvnIM,swi,ldmIM,stmIM
	.data   ldrIM,strIM,lduIM,stuIM,adrIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsIM,adcsIM,subsIM,cmpIM,eorsIM,orrsIM,andsIM,tstIM
	.data   mulsIM,mlas,divsIM,movsIM,mvnsIM,swis,ldmIM,stmIM
	.data   ldrsIM,strsIM,ldusIM,stusIM,adrsIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	.data   addIM,adcIM,subIM,cmpIM,eorIM,orrIM,andIM,tstIM ; 9
	.data   mulIM,mla,divIM,movIM,mvnIM,swi,ldmIM,stmIM
	.data   ldrIM,strIM,lduIM,stuIM,adrIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsIM,adcsIM,subsIM,cmpIM,eorsIM,orrsIM,andsIM,tstIM
	.data   mulsIM,mlas,divsIM,movsIM,mvnsIM,swis,ldmIM,stmIM
	.data   ldrsIM,strsIM,ldusIM,stusIM,adrsIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	.data   addIM,adcIM,subIM,cmpIM,eorIM,orrIM,andIM,tstIM ;10
	.data   mulIM,mla,divIM,movIM,mvnIM,swi,ldmIM,stmIM
	.data   ldrIM,strIM,lduIM,stuIM,adrIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsIM,adcsIM,subsIM,cmpIM,eorsIM,orrsIM,andsIM,tstIM
	.data   mulsIM,mlas,divsIM,movsIM,mvnsIM,swis,ldmIM,stmIM
	.data   ldrsIM,strsIM,ldusIM,stusIM,adrsIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	.data   addIM,adcIM,subIM,cmpIM,eorIM,orrIM,andIM,tstIM ; 11
	.data   mulIM,mla,divIM,movIM,mvnIM,swi,ldmIM,stmIM
	.data   ldrIM,strIM,lduIM,stuIM,adrIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsIM,adcsIM,subsIM,cmpIM,eorsIM,orrsIM,andsIM,tstIM
	.data   mulsIM,mlas,divsIM,movsIM,mvnsIM,swis,ldmIM,stmIM
	.data   ldrsIM,strsIM,ldusIM,stusIM,adrsIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	.data   addIM,adcIM,subIM,cmpIM,eorIM,orrIM,andIM,tstIM ;12
	.data   mulIM,mla,divIM,movIM,mvnIM,swi,ldmIM,stmIM
	.data   ldrIM,strIM,lduIM,stuIM,adrIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsIM,adcsIM,subsIM,cmpIM,eorsIM,orrsIM,andsIM,tstIM
	.data   mulsIM,mlas,divsIM,movsIM,mvnsIM,swis,ldmIM,stmIM
	.data   ldrsIM,strsIM,ldusIM,stusIM,adrsIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	.data   addIM,adcIM,subIM,cmpIM,eorIM,orrIM,andIM,tstIM;13
	.data   mulIM,mla,divIM,movIM,mvnIM,swi,ldmIM,stmIM
	.data   ldrIM,strIM,lduIM,stuIM,adrIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsIM,adcsIM,subsIM,cmpIM,eorsIM,orrsIM,andsIM,tstIM
	.data   mulsIM,mlas,divsIM,movsIM,mvnsIM,swis,ldmIM,stmIM
	.data   ldrsIM,strsIM,ldusIM,stusIM,adrsIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	
	.data   addIM,adcIM,subIM,cmpIM,eorIM,orrIM,andIM,tstIM	;14
	.data   mulIM,mla,divIM,movIM,mvnIM,swi,ldmIM,stmIM
	.data   ldrIM,strIM,lduIM,stuIM,adrIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsIM,adcsIM,subsIM,cmpIM,eorsIM,orrsIM,andsIM,tstIM
	.data   mulsIM,mlas,divsIM,movsIM,mvnsIM,swis,ldmIM,stmIM
	.data   ldrsIM,strsIM,ldusIM,stusIM,adrsIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	;15
	.data   addIM,adcIM,subIM,cmpIM,eorIM,orrIM,andIM,tstIM
	.data   mulIM,mla,divIM,movIM,mvnIM,swi,ldmIM,stmIM
	.data   ldrIM,strIM,lduIM,stuIM,adrIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsIM,adcsIM,subsIM,cmpIM,eorsIM,orrsIM,andsIM,tstIM
	.data   mulsIM,mlas,divsIM,movsIM,mvnsIM,swis,ldmIM,stmIM
	.data   ldrsIM,strsIM,ldusIM,stusIM,adrsIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	;16
	.data   addIM,adcIM,subIM,cmpIM,eorIM,orrIM,andIM,tstIM
	.data   mulIM,mla,divIM,movIM,mvnIM,swi,ldmIM,stmIM
	.data   ldrIM,strIM,lduIM,stuIM,adrIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsIM,adcsIM,subsIM,cmpIM,eorsIM,orrsIM,andsIM,tstIM
	.data   mulsIM,mlas,divsIM,movsIM,mvnsIM,swis,ldmIM,stmIM
	.data   ldrsIM,strsIM,ldusIM,stusIM,adrsIM,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	
	;;; shift by immediate value - lsl 
	.data   addSClsl,adcSClsl,subSClsl,cmpSClsl,eorSClsl,orrSClsl,andSClsl,tstSClsl
	.data   mulSClsl,mla,divSClsl,movSClsl,mvnSClsl,swi,ldmSClsl,stmSClsl
	.data   ldrSClsl,strSClsl,lduSClsl,stuSClsl,adrSClsl,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsSClsl,adcsSClsl,subsSClsl,cmpSClsl,eorsSClsl,orrsSClsl,andsSClsl,tstSClsl
	.data   mulsSClsl,mlas,divsSClsl,movsSClsl,mvnsSClsl,swis,ldmSClsl,stmSClsl
	.data   ldrsSClsl,strsSClsl,ldusSClsl,stusSClsl,adrsSClsl,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	;;; shift by immediate value - lsr 
	.data   addSClsr,adcSClsr,subSClsr,cmpSClsr,eorSClsr,orrSClsr,andSClsr,tstSClsr
	.data   mulSClsr,mla,divSClsr,movSClsr,mvnSClsr,swi,ldmSClsr,stmSClsr
	.data   ldrSClsr,strSClsr,lduSClsr,stuSClsr,adrSClsr,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsSClsr,adcsSClsr,subsSClsr,cmpSClsr,eorsSClsr,orrsSClsr,andsSClsr,tstSClsr
	.data   mulsSClsr,mlas,divsSClsr,movsSClsr,mvnsSClsr,swis,ldmSClsr,stmSClsr
	.data   ldrsSClsr,strsSClsr,ldusSClsr,stusSClsr,adrsSClsr,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	;;; shift by immediate value - asr
	.data   addSCasr,adcSCasr,subSCasr,cmpSCasr,eorSCasr,orrSCasr,andSCasr,tstSCasr
	.data   mulSCasr,mla,divSCasr,movSCasr,mvnSCasr,swi,ldmSCasr,stmSCasr
	.data   ldrSCasr,strSCasr,lduSCasr,stuSCasr,adrSCasr,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsSCasr,adcsSCasr,subsSCasr,cmpSCasr,eorsSCasr,orrsSCasr,andsSCasr,tstSCasr
	.data   mulsSCasr,mlas,divsSCasr,movsSCasr,mvnsSCasr,swis,ldmSCasr,stmSCasr
	.data   ldrsSCasr,strsSCasr,ldusSCasr,stusSCasr,adrsSCasr,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	;;; shift by immediate value - asr 
	.data   addSCror,adcSCror,subSCror,cmpSCror,eorSCror,orrSCror,andSCror,tstSCror
	.data   mulSCror,mla,divSCror,movSCror,mvnSCror,swi,ldmSCror,stmSCror
	.data   ldrSCror,strSCror,lduSCror,stuSCror,adrSCror,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsSCror,adcsSCror,subsSCror,cmpSCror,eorsSCror,orrsSCror,andsSCror,tstSCror
	.data   mulsSCror,mlas,divsSCror,movsSCror,mvnsSCror,swis,ldmSCror,stmSCror
	.data   ldrsSCror,strsSCror,ldusSCror,stusSCror,adrsSCror,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	;;; shift by register value -lsl
	.data   addSRlsl,adcSRlsl,subSRlsl,cmpSRlsl,eorSRlsl,orrSRlsl,andSRlsl,tstSRlsl
	.data   mulSRlsl,mla,divSRlsl,movSRlsl,mvnSRlsl,swi,ldmSRlsl,stmSRlsl
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsSRlsl,adcsSRlsl,subsSRlsl,cmpSRlsl,eorsSRlsl,orrsSRlsl,andsSRlsl,tstSRlsl
	.data   mulsSRlsl,mlas,divsSRlsl,movsSRlsl,mvnsSRlsl,swis,ldmSRlsl,stmSRlsl
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	;;; shift by register value -lsr
	.data   addSRlsr,adcSRlsr,subSRlsr,cmpSRlsr,eorSRlsr,orrSRlsr,andSRlsr,tstSRlsr
	.data   mulSRlsr,mla,divSRlsr,movSRlsr,mvnSRlsr,swi,ldmSRlsr,stmSRlsr
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsSRlsr,adcsSRlsr,subsSRlsr,cmpSRlsr,eorsSRlsr,orrsSRlsr,andsSRlsr,tstSRlsr
	.data   mulsSRlsr,mlas,divsSRlsr,movsSRlsr,mvnsSRlsr,swis,ldmSRlsr,stmSRlsr
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	;;; shift by register value -asr
	.data   addSRasr,adcSRasr,subSRasr,cmpSRasr,eorSRasr,orrSRasr,andSRasr,tstSRasr
	.data   mulSRasr,mla,divSRasr,movSRasr,mvnSRasr,swi,ldmSRasr,stmSRasr
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsSRasr,adcsSRasr,subsSRasr,cmpSRasr,eorsSRasr,orrsSRasr,andsSRasr,tstSRasr
	.data   mulsSRasr,mlas,divsSRasr,movsSRasr,mvnsSRasr,swis,ldmSRasr,stmSRasr
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	;;; shift by register value -ror
	.data   addSRror,adcSRror,subSRror,cmpSRror,eorSRror,orrSRror,andSRror,tstSRror
	.data   mulSRror,mla,divSRror,movSRror,mvnSRror,swi,ldmSRror,stmSRror
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   addsSRror,adcsSRror,subsSRror,cmpSRror,eorsSRror,orrsSRror,andsSRror,tstSRror
	.data   mulsSRror,mlas,divsSRror,movsSRror,mvnsSRror,swis,ldmSRror,stmSRror
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	;;; only mla and branching - 4 ea
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,mla,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,mlas,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,mla,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,mlas,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,mla,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,mlas,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,mla,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,mlas,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	;;; Final four -- only branching
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   halt,halt,halt,halt,halt,halt,halt,halt
	.data   b,b,bl,bl,halt,halt,halt,halt

;;; DON'T WRITE ANY CODE BELOW THIS LINE
wregs:
wr0:	.data	0
wr1:	.data	0
wr2:	.data	0
wr3:	.data	0
wr4:	.data	0
wr5:	.data   0
wr6:	.data   0
wr7:	.data   0
wr8:	.data	0
wr9:	.data   0
wr10:	.data   0
wr11:	.data   0
wr12:	.data	0
wsp:	
wr13:	.data   0x00ffffff
wlr:	
wr14:	.data   0
wr15:	
wpc:	.data	0
wccr:	.data	0		; ccr in warm (low 4 bits are N,Z,C,V)
	
warm:

	

