{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind  #-}

-- | Author: Max Scheiber, University of Pennsylvania '15, maxnscheiber@gmail.com
-- | Version: 0.2
-- | Specific parsers designed to parse LC4 .asm files

module LC4.LC4Parser 
  (
    lineP
  ) where

import Parser.Parser
import Parser.Base
import LC4.Types
import Data.Char(isSpace)

-- | Parse one of all eight possible registers
regP :: Parser String Reg
regP = unlist $ map (\r -> string (show r) >> return r) [R0, R1, R2, R3, R4, R5, R6, R7]

-- | Parse a label as a string
labelP :: Parser String String
labelP = ws . rest $ satisfy (not . isSpace)

-- | Convert an Instruction or Directive into a Line
lineP :: Parser String Line
lineP = (instrP >>= \i -> return (Instr i))   <|>
        (directiveP >>= \d -> return (Dir d)) <|>
        commentP                              <|>
        labelIP

-- | Parse a Comment
commentP :: Parser String Line
commentP = do
  ws $ char ';'
  cmnt <- ws $ many . satisfy $ not . (== '\n')
  return $ Comment cmnt

labelIP :: Parser String Line
labelIP = do
  lbl <- ws $ rest . satisfy $ not . (== '\n')
  return $ Label lbl

-- | Parse an Instruction from one of its constructors
instrP :: Parser String Instruction
instrP = unlist . concat $ 
  [
   [retP], 
   map loneP [NOP, RTI],
   map oneRegP [JSRR, JMPR],
   map oneStrP [BRnzp, BRnz, BRnp, BRn, BRzp, BRz, BRp, JSR, JMP],
   map oneValP [BRnzp, BRnz, BRnp, BRn, BRzp, BRz, BRp, JSR, JMP, TRAP],
   map oneRegOneStrP [LEA, LC],
   map oneRegOneValP [CMPI, CMPIU, CONST, HICONST],
   map twoRegP [CMP, CMPU, NOT], 
   map twoRegOneValP [ADD, AND, LDR, STR, SLL, SRA, SRL],
   map threeRegP [ADD, MUL, SUB, DIV, MOD, AND, OR, XOR]
  ]

-- | Turns the RET pseudoinstruction into JMPR R7. Parses space to differentiate
-- | between a label RETURN and an instruction RET.
retP :: Parser String Instruction
retP = ws $ string "RET" >> space >> (return $ OneReg JMPR R7)

-- | Parses an Operator with no arguments, like NOP or RTI.
loneP :: Operator -> Parser String Instruction
loneP op = ws (string $ show op) >> return (Lone op)

-- | Parses an Operator with a single label argument, like a branch or JMP.
-- | NOTE: These are not valid instructions in assembly, but will be kept as is
-- | for the purposes of simpler simulation in Haskell.
oneStrP :: Operator -> Parser String Instruction
oneStrP op = do
  ws . string $ show op
  s <- ws labelP
  return $ OneStr op s

-- | Parses an Operator with a single value argument, like a branch or JMP.
oneValP :: Operator -> Parser String Instruction
oneValP op = do
  ws . string $ show op
  val <- ws int
  return $ OneVal op (IMM16 val)

-- | Parses an Operator that takes one register, like JSRR or JMPR.
oneRegP :: Operator -> Parser String Instruction
oneRegP op = do
  ws . string $ show op
  reg <- ws regP
  return $ OneReg op reg

-- | Parses an Operator that takes one register and one label, like LEA or LC.
oneRegOneStrP :: Operator -> Parser String Instruction
oneRegOneStrP op = do
  ws . string $ show op
  reg <- ws regP
  (ws $ string ",") <|> (ws $ string "")
  s <- ws labelP
  return $ OneRegOneStr op reg s

-- | Parses an Operator that takes one register and one value, like CMPI or 
-- | CONST.
oneRegOneValP :: Operator -> Parser String Instruction
oneRegOneValP op = do
  ws . string $ show op
  reg <- ws regP
  (ws $ string ",") <|> (ws $ string "")
  val <- ws int
  return $ OneRegOneVal op reg (IMM16 val)

-- | Parses an Operator that takes two registers, like NOT or CMP.
twoRegP :: Operator -> Parser String Instruction
twoRegP op = do
  ws . string $ show op
  rd <- ws regP
  (ws $ string ",") <|> (ws $ string "")
  rs <- ws regP
  return $ TwoReg op rd rs

-- | Parses an Operator takes two registers and a value, like ADDIM or STR.
twoRegOneValP :: Operator -> Parser String Instruction
twoRegOneValP op = do
  ws . string $ show op
  rd <- ws regP
  (ws $ string ",") <|> (ws $ string "")
  rs <- ws regP
  (ws $ string ",") <|> (ws $ string "")
  val <- ws int
  return $ TwoRegOneVal op rd rs (IMM16 val)

-- | Parses an Operator that takes three registers, like MUL or XOR.
threeRegP :: Operator -> Parser String Instruction
threeRegP op = do
  ws . string $ show op
  rd <- ws regP
  (ws $ string ",") <|> (ws $ string "")
  rs <- ws regP
  (ws $ string ",") <|> (ws $ string "")
  rt <- ws regP
  return $ ThreeReg op rd rs rt

-- | Combines all assembly directive parsers.
directiveP :: Parser String Directive
directiveP = unlist [dataP, codeP, addrP, falignP, fillP, blkwP, dconstP, 
  uconstP]

-- | Individual parsers for assembly directives.
dataP, codeP, addrP, falignP, fillP, blkwP, dconstP, uconstP :: Parser String Directive
dataP   = ws $ string ".DATA"   >> return DATA
codeP   = ws $ string ".CODE"   >> return CODE
falignP = ws $ string ".FALIGN" >> return FALIGN
addrP   = do 
  ws . string $ ".ADDR"
  i <- ws int
  return $ ADDR (UIMM16 i)
fillP = do
  ws . string $ ".FILL"
  i <- ws int
  return $ FILL (IMM16 i)
blkwP = do
  ws . string $ ".BLKW"
  i <- ws int
  return $ BLKW (UIMM16 i)
dconstP = do
  s <- ws labelP
  ws . string $ ".CONST"
  i <- ws int
  return $ DCONST s (IMM16 i)
uconstP = do
  s <- ws $ labelP
  ws . string $ ".UCONST"
  i <- ws int
  return $ UCONST s (UIMM16 i)

{- addrP   = ws $ string ".ADDR"   >> (ws $ int >>= \i -> return (ADDR $ UIMM16 i))
fillP   = ws $ string ".FILL"   >> (ws $ int >>= \i -> return (FILL $ IMM16 i))
blkwP   = ws $ string ".BLKW"   >> (ws $ int >>= \i -> return (BLKW $ UIMM16 i))
dconstP = ws $ string ".CONST"  >> (ws $ int >>= \i -> return (DCONST $ IMM16 i))
uconstP = ws $ string ".UCONST" >> (ws $ int >>= \i -> return (UCONST $ UIMM16 i))

directP op = do
  s <- ws $ labelP
  string $ '.' : show op
  i <- ws int
  return $ op s (IMM16 i) -}
