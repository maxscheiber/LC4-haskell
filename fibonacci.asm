;; Fibonacci numbers program
;; Calculates the first 20 Fibonacci numbers and stores them in addresses 0x4000
;;   through 0x4013
;; R0 = pointer to the current address
;; R1 = i, counter for the while loop

	.DATA
	.ADDR 0x4000		; start filling data at address 0x4000

	.FILL #0		; fills 0x4000 with 0
	.FILL #1		; fills 0x4001 with 1
	
	.CODE
	.ADDR 0x0000 		; start filling instructions at address 0
	CONST R0, 0x00		; initialize R0 to the first fibonacci number's pointer in memory
	HICONST R0, 0x40	; second half of initializing R0
	CONST R1, #2 		; initialize i to 2

LOOP

	CMPI R1, #20 		; compare i to 20
	BRzp END		; if i >= 20, jump to end

	LDR R2, R0, #0	 	; load F_i-2 into R2
	LDR R3, R0, #1	 	; load F_i-1 into R3
	ADD R4, R2, R3		; place F_i (F_i-2 + F_i-1) into R4
	STR R4, R0, #2 		; store F_i at current address R0
	ADD R0, R0, #1		; increment current address
	ADD R1, R1, #1		; increment counter
	BRnzp LOOP		; go to the start of the while loop

END