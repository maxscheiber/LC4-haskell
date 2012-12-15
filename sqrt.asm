;; Square root program
;; B = sqrt(A); the largest integer such that B*B <= A
;; R0 = A, R1 = B

	.CODE
	.ADDR 0x0000 		; start filling instructions at address 0

	CONST R1, #0		; initialize B to 0

A_NOT_NEG

	CMPI R0, #0		; compare A to 0
	BRn END			; if A < 0, jump to END

LOOP

	MUL R2, R1, R1		; store B*B in R2
	CMP R2, R0		; compare B*B to A
	BRp END			; if B*B > A, jump to END
	ADD R1, R1, #1		; B++
	BRnzp LOOP		; start the while loop

END

	ADD R1, R1, #-1		; B--

INFINITE

	BRnzp INFINITE		; infinitely loop once done