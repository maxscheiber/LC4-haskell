{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -XScopedTypeVariables  #-}

-- | Author: Max Scheiber, University of Pennsylvania '15, maxnscheiber@gmail.com
-- | Version: 0.2
-- | Assembly and simulation of the LC4 ISA.
-- | KNOWN BUGS: no error checking, merely preprocesses and does not run

module LC4.LC4 where
import Prelude
import Parser.Parser (Parser, parse)
import Parser.Base
import LC4.LC4Parser
import LC4.Types
import System.IO
import Data.Map as Map hiding (filter)
import Data.IntMap as IntMap hiding (filter)
import Control.Monad.State
import Numeric (readHex)
import Data.Bits
import Data.Word

-- | UNSAFE METHOD, only use if positive hex is in right format
hexToInt :: String -> Int
hexToInt = fst . head . readHex

intToWord :: Int -> Word
intToWord s = fromIntegral s

wordToInt :: Word -> Int
wordToInt s = fromIntegral s

userCode, userData, osCode, osData :: Int
userCode = 0
userData = hexToInt "4000"
osCode   = hexToInt "8000"
osData   = hexToInt "A000"

-- | Current Machine state.
machine :: Machine
machine = Machine 
            { pc = hexToInt "8200"
            , psr = hexToInt "8000" -- PSR[15] = 1
            , regs = Map.empty
            , memory = IntMap.empty
            , labels = Map.empty
            }

-- | The standard parser for LC4 .asm files.
asmParser :: Parser String [Line]
asmParser = many lineP

-- | Properly parse a .asm file and maybe return an error.
parseASM :: Parser String [Line] -> String -> Either String [Line]
parseASM p str = case parse p str of
  []       -> Left "No possible parse"
  [([],_)] -> Left "No possible parse"
  [(a,_)]  -> Right a
  _        -> Left "Multiple possible parses"

-- | Read in the .asm file itself and parse it.
readASM :: Parser String [Line] -> String -> IO (Either String [Line])
readASM p name = do
  f <- openFile name ReadMode
  str <- hGetContents f
  return $ parseASM p str

-- | Simulate the .asm file inputted.
runASM :: Parser String [Line] -> String -> IO (Either String Machine)
runASM p name = do
  f <- openFile name ReadMode
  str <- hGetContents f
  case parseASM p str of
    Left errorS  -> return $ Left errorS
    Right parsed -> return . Right $ execState (preprocess parsed) machine

preprocess :: [Line] -> State Machine ()
preprocess []     = do
  m <- get
  put $ m { pc = 0, psr = 0 }
  return ()
preprocess (Instr i:ls) = do
  m <- get
  put $ m { memory = IntMap.insert (pc m) i (memory m), 
            pc = 1 + pc m }
  preprocess ls
  return ()
preprocess (Dir (ADDR (UIMM16 v)):ls) = do
  m <- get
  put $ m { pc = v }
  preprocess ls
  return ()
preprocess (Dir FALIGN:ls) = do
  m <- get
  case (pc m `mod` 16) of
    0 -> do preprocess ls
            return ()
    x -> do put $ m { pc = (pc m) + 16 - x }
            preprocess ls
            return ()
preprocess (Dir (FILL v):ls) = do
  m <- get
  put $ m { memory = IntMap.insert (pc m) (OneVal BINARY v) (memory m),
            pc = 1 + pc m }
  preprocess ls
  return ()
preprocess (Dir (BLKW (UIMM16 v)):ls) = do
  m <- get
  put $ m { pc = v + pc m }
  preprocess ls
  return ()
preprocess (Dir (DCONST lbl (IMM16 v)):ls) = do
  m <- get -- EVENTUALLY CHECK FOR DUPLICATE LABELS
  put $ m { labels = Map.insert lbl v (labels m) }
  preprocess ls
  return ()
preprocess (Dir (UCONST lbl (IMM16 v)):ls) = do
  m <- get -- EVENTUALLY CHECK FOR DUPLICATE LABELS
  put $ m { labels = Map.insert lbl v (labels m) }
  preprocess ls
  return ()
preprocess (Dir _:ls)     = preprocess ls
preprocess (Comment _:ls) = preprocess ls
preprocess (Label lbl:ls) = do
  m <- get
  put $ m { labels = Map.insert lbl (pc m) (labels m) }
  preprocess ls
  return ()

process :: Instruction -> State Machine ()
process (Lone END) = return ()
process (ThreeReg op rd rs rt) = do
  m <- get
  let rsval = Map.findWithDefault 0 rs (regs m)
  let rtval = Map.findWithDefault 0 rt (regs m)
  case op of
    ADD -> put $ m { regs = Map.insert rd (rsval+rtval) (regs m), pc = (pc m) +1 }
    SUB -> put $ m { regs = Map.insert rd (rsval-rtval) (regs m), pc = (pc m) +1 }
    MUL -> put $ m { regs = Map.insert rd (rsval*rtval) (regs m), pc = (pc m) +1 }
    DIV -> put $ m { regs = Map.insert rd (rsval `div` rtval) (regs m), pc  = (pc m) +1}
    AND -> put $ m { regs = Map.insert rd (rsval .&. rtval) (regs m), pc = (pc m) + 1 }
    OR -> put $ m {regs = Map.insert rd (rsval .|. rtval) (regs m), pc = (pc m) + 1}
    XOR -> put $ m {regs = Map.insert rd (rsval `xor` rtval) (regs m), pc = (pc m) + 1}
    MOD -> put $ m {regs = Map.insert rd (rsval `mod` rtval) (regs m), pc = (pc m) + 1}
    _ -> return ()
  let next_instr = IntMap.findWithDefault (Lone END) (pc m) (memory m)
  process next_instr
  return ()
process (TwoRegOneVal op rd rs (IMM16 imm)) = do
  m <- get
  let rsval = Map.findWithDefault 0 rs (regs m)
  case op of
    ADD -> put $ m {regs = Map.insert rd (rsval+imm) (regs m), pc = (pc m) + 1 }
    AND -> put $ m {regs = Map.insert rd (rsval .&. imm) (regs m), pc = (pc m) + 1}
    LDR -> let memval = IntMap.findWithDefault (OneVal BINARY (IMM16 0)) (rsval+imm) (memory m) in
      case memval of
        OneVal BINARY (IMM16 i) -> put $ m {regs = Map.insert rd i (regs m), pc = (pc m) + 1}
        _ -> return ()
    STR -> let rtval = Map.findWithDefault 0 rd (regs m) in
      put $ m {memory = IntMap.insert (rsval + imm) (OneVal BINARY (IMM16 rtval)) (memory m), pc = (pc m) + 1}
    SLL -> put $ m {regs = Map.insert rd (rsval `shift` imm) (regs m), pc = (pc m) + 1 }
    SRA -> put $ m {regs = Map.insert rd (rsval `shiftR` imm) (regs m), pc = (pc m) + 1 }
    SRL -> let newshift = shiftR (intToWord rsval) imm in
           put $ m {regs = Map.insert rd (wordToInt newshift) (regs m), pc = (pc m) + 1}
    _ -> return ()  
  let next_instr = IntMap.findWithDefault (Lone END) (pc m) (memory m)
  process next_instr
  return ()
process (TwoReg op rs rt) = do
  m <- get
  let rsval = Map.findWithDefault 0 rs (regs m)
  let rtval = Map.findWithDefault 0 rt (regs m)
  case op of
    CMP -> let sub = rsval - rtval in
          if (sub < 0) then
            put $ m { psr = (setBit (clearBit (clearBit (psr m) 0) 1) 2), pc = (pc m) + 1 }
          else if (sub == 0) then
            put $ m {psr = (setBit (clearBit (clearBit (psr m) 0) 2) 1), pc = pc m + 1 }
          else
            put $ m {psr = (setBit (clearBit (clearBit (psr m) 1) 2) 0), pc = pc m + 1 }
    CMPU -> let unsigned1 = intToWord rsval in
           let unsigned2 = intToWord rtval in
           let sub = wordToInt (unsigned1 - unsigned2) in
           if (sub < 0) then
            put $ m { psr = (setBit (clearBit (clearBit (psr m) 0) 1) 2), pc = (pc             m) + 1 }
          else if (sub == 0) then
            put $ m {psr = (setBit (clearBit (clearBit (psr m) 0) 2) 1), pc = pc m              + 1 }
          else
            put $ m {psr = (setBit (clearBit (clearBit (psr m) 1) 2) 0), pc = pc m              + 1 }
    NOT -> put $ m { regs = Map.insert rs (complement rtval) (regs m), pc = pc m + 1}
    _ -> return ()
  let next_instr = IntMap.findWithDefault (Lone END) (pc m) (memory m)
  process next_instr
  return ()
process (OneRegOneVal op rs (IMM16 imm)) = do
  m <- get
  let rsval = Map.findWithDefault 0 rs (regs m)
  case op of
    CMPI -> let sub = rsval - imm in
            if (sub < 0) then
            put $ m { psr = (setBit (clearBit (clearBit (psr m) 0) 1) 2), pc = (pc m) + 1 }
          else if (sub == 0) then
            put $ m {psr = (setBit (clearBit (clearBit (psr m) 0) 2) 1), pc = pc m + 1 }
          else
            put $ m {psr = (setBit (clearBit (clearBit (psr m) 1) 2) 0), pc = pc m + 1 }
    CMPIU -> let unsigned1 = intToWord rsval in
             let unsigned2 = intToWord imm in
             let sub = wordToInt (unsigned1 - unsigned2) in
             if (sub < 0) then
              put $ m { psr = (setBit (clearBit (clearBit (psr m) 0) 1) 2), pc = (pc m) + 1 }
             else if (sub == 0) then
               put $ m {psr = (setBit (clearBit (clearBit (psr m) 0) 2) 1), pc = pc m + 1 }
                  else
                   put $ m {psr = (setBit (clearBit (clearBit (psr m) 1) 2) 0), pc = pc m + 1 }
    CONST -> put $ m {regs = Map.insert rs imm (regs m), pc = pc m + 1}
    HICONST -> let newval = ((rsval .&. 255) .|. (imm `shiftL` 8)) in
               put $ m {regs = Map.insert rs newval (regs m), pc = pc m + 1}
    _ -> return ()
  let next_instr = IntMap.findWithDefault (Lone END) (pc m) (memory m)
  process next_instr
  return ()
process (OneRegOneStr op rd label) = do
  m <- get
  -- STILL NEED TO DO LC
  case op of
    LEA -> put $ m { regs = Map.insert rd (Map.findWithDefault 0 label (labels m)) (regs m), pc = pc m + 1 }
    _ -> return ()
  let next_instr = IntMap.findWithDefault (Lone END) (pc m) (memory m)
  process next_instr
  return ()
process (OneReg op rs) = do
  m <- get
  let rsval = Map.findWithDefault 0 rs (regs m)
  case op of
    JSRR -> put $ m { regs = Map.insert R7 (pc m + 1) (regs m), pc = rsval }
    JMPR -> put $ m {pc = rsval }
    _ -> return ()
  let next_instr = IntMap.findWithDefault (Lone END) (pc m) (memory m)
  process next_instr
  return ()
process (OneVal op (IMM16 imm)) = do
  m <- get
  return ()
process (OneStr op label) = undefined
process (Lone op) = do
  m <- get
  case op of
    NOP -> put $ m
    RTI -> put $ m {psr = (psr m) `clearBit` 15, pc = Map.findWithDefault 0 R7 (regs m) }
    RET -> put $ m {pc = Map.findWithDefault 0 R7 (regs m) }
    _ -> return ()
  let next_instr = IntMap.findWithDefault (Lone END) (pc m) (memory m)
  process next_instr
  return ()
  