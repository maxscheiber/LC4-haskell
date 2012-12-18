;; Square Root Code
;; A = B*B, Given A Find B
;; R0 = A, R1 = B, R2 = B*B

	.CODE
	.ADDR x0000

	CONST R1 #0 ; set B to 0
	CONST R0 #9
IF
	CMPI R0 #0 ; compare A to 0
	BRn RETURN
LOOP
	MUL R2 R1 R1
	CMP R2 R0
	BRp RETURN
	ADD R1 R1 #1 
	BRnzp LOOP

RETURN
	ADD R1 R1 #-1
	END
	
