;; Fibonacci Counter
;; Stores the first 20 Fibonacci numbers in memory
;; R0 = mem address, R1 = Fibonacci #1, R2 = Fibonacci #2, R3 = Counter

;; Data section
  .DATA
  .ADDR x4000 ; start data at address x4000

;; Code Section
  .CODE
  .ADDR x0000

INIT
  CONST R0, 0      ;load low byte
  HICONST R0 x40     ; load high byte
  CONST R1, 0      ; first fib is 0
  CONST R2, 1      ; second fib is 1
  CONST R3 2       ; counter is 2
  STR R1 R0 0      ; store the first fib
  ADD R0 R0 1      ; increment mem address
  STR R2 R0 0      ; store the second fib
  ADD R0 R0 1      ; increment mem address

LOOP
  CMPI R3 #20      ; compare counter to 20
  BRzp END
  ADD R2 R2 R1     ; find new fib number
  STR R2 R0 0      ; store in memory
  ADD R0 R0 1      ; increment mem
  LDR R1 R0 #-2    ; update first fib
  LDR R2 R0 #-1   ; update second fib
  ADD R3 R3 1     ; inc counter
  BRnzp LOOP
  END
  

