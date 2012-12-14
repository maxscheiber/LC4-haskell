;;; USER Code

;;;  These constants define the dimensions of the square
ROW1 .CONST 40
ROW2 .CONST 100
COL1 .CONST 30
COL2 .CONST 120

;;;  Define some colors
	
RED   .UCONST x7C00	; 0 11111 00000 00000
GREEN .UCONST x03E0	; 0 00000 11111 00000
BLUE  .UCONST x001F	; 0 00000 00000 11111
WHITE .UCONST x7FFF	; 0 11111 11111 11111
	
	.CODE
	.ADDR x0000

;;;  Output to the Console with the PUTC Trap
	CONST R0, x43
	TRAP x01

	CONST R0, x4A
	TRAP x01

	CONST R0, x0A		; Output a newline character
	TRAP x01

	
	CONST R0, x43
	TRAP x01

	CONST R0, x4A
	TRAP x01

	
;;; Draw some lines with the DRAW_H_LINE TRAP

	LC R0, ROW1
	LC R1, COL1
	LC R2, COL2
	LC R3, RED

	TRAP x02		; DRAW H LINE

	
	LC R0, ROW2
	LC R1, COL2
	LC R2, COL1
	LC R3, GREEN

	TRAP x02		; DRAW H LINE


END_PROGRAM