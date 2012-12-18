;;; HW 6 part 3, Max Scheiber, scheiber@wharton.upenn.edu, 10.29.2012

;;;  Define some colors
RED    .UCONST x7C00	; 0 11111 00000 00000
YELLOW .UCONST x7FE0	; 0 11111 11111 00000
GREEN  .UCONST x03E0	; 0 00000 11111 00000

	.CODE
	.ADDR x0000

;;; Store the points for the red line from (20, 20) to (20, 100) in (col, row) form
	CONST R1, x00		; starting at x4000 in memory
	HICONST R1, x40
	CONST R2, #20 		; col starts at 20
	CONST R3, #20		; row starts at 20
	CONST R4, #100		; store 100 in R4
	
RED_LINE_LOOP
	CMP R3, R4		; compare the row to 100
	BRp DRAW_RED_LINE	; if our row is greater than 100, we're done loading points
	STR R2, R1, #0		; store the col at the address at R1
	STR R3, R1, #1		; store the row at the address at R1 + 1
	ADD R1, R1, #2		; bump the address up by two
	ADD R3, R3, #1		; bump the row up by one
	BRnzp RED_LINE_LOOP
	
DRAW_RED_LINE
	CONST R0, #81		; we are drawing 81 points total
	CONST R1, x00		; starting at x4000 in memory
	HICONST R1, x40
	LC R2, RED		; the line should be red
	TRAP x0A		; TRAP_DRAW_PTS

;;; Store the points for the yellow line from (20, 20) to (200, 200) in (col, row) form
	CONST R1, x00		; starting at x4000 in memory
	HICONST R1, x40
	CONST R2, #20		; col starts at 20
	CONST R3, #20		; row starts at 20
	CONST R4, #200		; store 200 in R4

YELLOW_LINE_LOOP
	CMP R3, R4		; compare the row to 200
	BRp DRAW_YELLOW_LINE	; if our row is greater than 200, we're done loading points
	STR R2, R1, #0		; store the col at the address at R1
	STR R3, R1, #1		; store the row at the address at R1 + 1
	ADD R1, R1, #2		; bump the address up by two
	ADD R2, R2, #1		; bump the col up by one
	ADD R3, R3, #1		; bump the row up by one
	BRnzp YELLOW_LINE_LOOP
	
DRAW_YELLOW_LINE
	CONST R0, #181		; we are drawing 181 points total
	CONST R1, x00		; starting at x4000 in memory
	HICONST R1, x40
	LC R2, YELLOW		; the line should be yellow
	TRAP x0A		; TRAP_DRAW_PTS

;;; Store the points for the green circle of radius 10 centered at (64, 120) in (col, row) form
	CONST R1, x00		; starting at x4000 in memory
	HICONST R1, x40
	CONST R2, #64 		; R2 - col centered at 64
	CONST R3, #120		; R3 - row centered at 120
	CONST R4, #10		; R4 - radius of 10
	CONST R6, #0
	SUB R6, R6, R4		; R6 = -radius (y)

CIRCLE_OUTER_LOOP
	CMP R6, R4		; compare R6 to radius
	BRp DRAW_GREEN_CIRCLE	; we're done when R6 > radius
	CONST R5, #0
	SUB R5, R5, R4		; R5 = -radius (x)

CIRCLE_INNER_LOOP
	ADD R6, R6, #1		; y++ pre-emptively
	CMP R5, R4		; compare R5 to radius
	BRp CIRCLE_OUTER_LOOP	; inner loop is over when R5 > radius
	ADD R6, R6, #-1		; y--

IF_X2_PLUS_Y2_LE_R2
	CONST R7, #0
	MUL R7, R5, R5		; R7 = x^2
	CONST R0, #0
	MUL R0, R6, R6		; R0 = y^2
	ADD R0, R0, R7		; R0 = x^2 + y^2
	MUL R7, R4, R4		; R7 = r^2

	CMP R0, R7		; compare x^2 + y^2 to r^2
	BRp AFTER_IF_X2_PLUS_Y2_LE_R2  ; skip condition when x^2 + y^2 > r^2
	
	ADD R2, R2, R6		; R2 = cols + y
	STR R2, R1, #0
	SUB R2, R2, R6		; R2 = cols

	ADD R3, R3, R5		; R3 = rows + x
	STR R3, R1, #1
	SUB R3, R3, R5		; R3 = rows

	ADD R1, R1, #2

AFTER_IF_X2_PLUS_Y2_LE_R2
	ADD R5, R5, #1		; x++
	BRnzp CIRCLE_INNER_LOOP
	
DRAW_GREEN_CIRCLE
	CONST R0, x3D		; we have 317 points, or x013D in hex. We know this from the following Haskell script:
	HICONST R0, x01		; length [x*x + y*y | x <- [-10..10], y <- [-10..10], x*x + y*y <= 100]
	CONST R1, x00		; starting at x4000 in memory
	HICONST R1, x40
	LC R2, GREEN		; the circle should be green
	TRAP x0A		; TRAP_DRAW_PTS

END_PROGRAM