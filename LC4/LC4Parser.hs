{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind  #-}

-- | Author: Max Scheiber, University of Pennsylvania '15, maxnscheiber@gmail.com
-- | Version: 0.1
-- | Specific parsers designed to parse LC4 .asm files
-- | KNOWN ISSUES: Does not correctly distinguish between hex / dec / binary.

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
labelP = ws . many $ satisfy (not . isSpace)

-- | Convert an Instruction or Directive into a Line
lineP :: Parser String Line
lineP = (instrP >>= \i -> return (Instr i)) <|>
        (directiveP >>= \d -> return (Dir d))

-- | Parse an Instruction from one of its constructors
instrP :: Parser String Instruction
instrP = unlist . concat $ 
  [
   map loneP [NOP, RTI],
   [retP], 
   map oneStrP [BRn, BRnz, BRnp, BRz, BRzp, BRp, BRnzp, JSR, JMP],
   map oneValP [BRn, BRnz, BRnp, BRz, BRzp, BRp, BRnzp, JSR, JMP],
   map oneRegP [JSRR, JMPR, TRAP],
   map oneRegOneStrP [LEA, LC],
   map oneRegOneValP [CMPI, CMPIU, CONST, HICONST],
   map twoRegP [CMP, CMPU, NOT], 
   map twoRegOneValP [ADDIM, ANDIM, LDR, STR, SLL, SRA, SRL],
   map threeRegP [ADD, MUL, SUB, DIV, MOD, AND, OR, XOR]
  ]

-- | Parses an Operator with no arguments, like NOP or RTI
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
  ws $ char ','
  s <- ws labelP
  return $ OneRegOneStr op reg s

-- | Parses an Operator that takes one register and one value, like CMPI or 
-- | CONST.
oneRegOneValP :: Operator -> Parser String Instruction
oneRegOneValP op = do
  ws . string $ show op
  reg <- ws regP
  ws $ char ','
  val <- ws int
  return $ OneRegOneVal op reg (IMM16 val)

-- | Parses an Operator that takes two registers, like NOT or CMP.
twoRegP :: Operator -> Parser String Instruction
twoRegP op = do
  ws . string $ show op
  rd <- ws regP
  ws $ char ','
  rs <- ws regP
  return $ TwoReg op rd rs

-- | Parses an Operator takes two registers and a value, like ADDIM or STR.
twoRegOneValP :: Operator -> Parser String Instruction
twoRegOneValP op = do
  ws . string $ show op
  rd <- ws regP
  ws $ char ','
  rs <- ws regP
  ws $ char ','
  val <- ws int
  return $ TwoRegOneVal op rd rs (IMM16 val)

-- | Parses an Operator that takes three registers, like MUL or XOR.
threeRegP :: Operator -> Parser String Instruction
threeRegP op = do
  ws . string $ show op
  rd <- ws regP
  ws $ char ','
  rs <- ws regP
  ws $ char ','
  rt <- ws regP
  return $ ThreeReg op rd rs rt

-- | Turns the RET pseudoinstruction into JMPR R7.
retP :: Parser String Instruction
retP = do
  ws $ string "RET"
  return $ OneReg JMPR R7

-- | Combines all assembly directive parsers.
directiveP :: Parser String Directive
directiveP = unlist [dataP, codeP, addrP, falignP, fillP, blkwP, dconstP, 
  uconstP]

-- | Individual parsers for assembly directives.
dataP, codeP, addrP, falignP, fillP, blkwP, dconstP, uconstP :: Parser String Directive
dataP = ws $ string ".DATA" >> return DATA
codeP = ws $ string ".CODE" >> return CODE
addrP = ws $ string ".ADDR" >> (ws $ int >>= \i -> return (ADDR $ UIMM16 i))
falignP = ws $ string ".FALIGN" >> return FALIGN
fillP = ws $ string ".FILL" >> (ws $ int >>= \i -> return (FILL $ IMM16 i))
blkwP = ws $ string ".BLKW" >> (ws $ int >>= \i -> return (BLKW $ UIMM16 i))
dconstP = ws $ string ".CONST" >> (ws $ int >>= \i -> return (DCONST $ IMM16 i))
uconstP = ws $ string ".UCONST" >> (ws $ int >>= \i -> return (UCONST $ UIMM16 i))
