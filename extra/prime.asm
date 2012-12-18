;; Prime numbers program
;; Tests whether an integer A is prime or not
;; R0 = A
;; R1 = 1 if A is prime, 0 if A is not prime
;; R2 = B, potential divisor of A

	.CODE
	.ADDR 0x0000 		; start instructions at address 0
	CONST R0, #67		; number to check
	CONST R1 #0 		; initialize R1, the prime flag, to 0
	CONST R2, #2 		; initialize B pre-emptively to 2

INCALCULABLE

	CMPI R0, #1 		; compare A to 1
	BRnz END 		; if A <= 1, jump to the end
	ADD R1, R1, #1 		; otherwise, turn the prime flag to true

LOOP_B_sqrd_LE_A

	MUL R3, R2, R2 		; put the square of B in R3
	CMP R3, R0		; compare B^2 to A
	BRp END 		; if B^2 > A, jump to the end

IF_A_mod_B_is_0

	MOD R3, R0, R2		; store A mod B in R3
	BRnp ELSE_A_mod_B_not_0	; if A mod B != 0, jump to implicit ELSE clause
	ADD R1, R1, #-1 	; turn the prime flag to false
	BRnzp END 		; we've found a divisor, so A is not prime

ELSE_A_mod_B_not_0

	ADD R2, R2, #1		; B++
	BRnzp LOOP_B_sqrd_LE_A 	; jump to start of loop

END