{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -XFlexibleInstances #-}

module LC4.Types where
import Prelude
import Data.Bits()
import Data.Map
import Data.IntMap
import Data.Word()

data Reg =
  R0 |
  R1 |
  R2 |
  R3 |
  R4 |
  R5 |
  R6 |
  R7
  deriving (Show, Eq, Ord, Read)

data Value = 
  UIMM4  Int |
  IMM5   Int |
  IMM6   Int |
  IMM7   Int |
  UIMM8  Int |
  IMM9   Int |
  IMM11  Int |
  IMM16  Int |
  UIMM16 Int
  deriving (Show, Eq)

data Directive = 
  DATA                |
  CODE                |
  ADDR Value          |
  FALIGN              |
  FILL Value          |
  BLKW Value          |
  DCONST String Value |
  UCONST String Value
  deriving (Show, Eq)

data Operator = 
  NOP     |
  BRn     |
  BRnz    |
  BRnp    |
  BRnzp   |
  BRz     |
  BRzp    |
  BRp     |
  ADD     |
  MUL     |
  SUB     |
  DIV     |
  MOD     |
  CMP     |
  CMPU    |
  CMPI    |
  CMPIU   |
  JSR     |
  JSRR    |
  JMPR    |
  JMP     |
  RTI     |
  TRAP    |
  AND     |
  NOT     |
  OR      |
  XOR     |
  LDR     |
  STR     |
  CONST   |
  HICONST |
  SLL     |
  SRA     |
  SRL     |
  LEA     |
  LC      |
  BINARY |
  END
  deriving (Show, Eq)

data Instruction =
  Lone Operator                       |
  OneStr Operator String              |
  OneVal Operator Value               |
  OneReg Operator Reg                 |
  OneRegOneStr Operator Reg String    |
  OneRegOneVal Operator Reg Value     |
  TwoReg Operator Reg Reg             |
  TwoRegOneVal Operator Reg Reg Value |
  ThreeReg Operator Reg Reg Reg
  deriving (Show, Eq)

data Line =
  Dir Directive | Instr Instruction | Comment String | Label String
  deriving (Show, Eq)

data Machine = Machine { pc :: Int
                       , psr :: Int
                       , regs :: Map Reg Int
                       , memory :: IntMap Instruction
                       , labels :: Map String Int
                       } deriving (Show, Eq)