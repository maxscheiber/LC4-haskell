;;; HW 6 part 2, Max Scheiber, scheiber@wharton.upenn.edu, 10.29.2012
	
	.DATA
	.ADDR x4000

;;; Stores my name in memory starting at address x4000
	text_array
	.STRINGZ "Max Scheiber\nStart Typing > "

	.CODE
	.ADDR x0000
	CONST R0, x00		; set the R0 address to x4000 for TRAP_PUTS
	HICONST R0, x40

;;; Prints my name and "Start Typing > "
	TRAP x08		; calls TRAP_PUTS to put the text on the screen

;;; Waits for a character before printing it to the screen
;;; Halts on a newline character
	
GET_CHAR_LOOP
	TRAP x00 		; TRAP_GETC; places character in R5
	CMPI R5, x0A		; is the char in R5 equal to a newline char?
	BRz END_PROGRAM		; terminates program on a newline char
	CONST R0, #0		; sets R0 to 0
	ADD R0, R5, #0		; moves char from R5 into R0
	TRAP x01		; TRAP_PUTC; puts character in R0 onto the console
	BRnzp GET_CHAR_LOOP	; loops all over again

END_PROGRAM