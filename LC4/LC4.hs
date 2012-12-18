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

writeToReg :: Reg -> Int -> State Machine ()
writeToReg rd val = do
  m <- get
  let regs' = Map.insert rd val (regs m)
  let nzp' = if val > 0 then 1 else if val == 0 then 2 else 4
  put (m { psr = (hexToInt "8000" .&. psr m) .|. nzp', regs = regs' })
  return ()

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
    Right parsed -> 
      do let m = execState (preprocess parsed) machine
         return . Right $ execState (process $ IntMap.findWithDefault (Lone END) (pc m) 
           (memory m)) m
         --return $ Right m

preprocess :: [Line] -> State Machine ()
preprocess []     = do
  m <- get
  put $ m { memory = IntMap.insert (pc m) (Lone END) (memory m), pc = 0, psr = 0 }
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
process instr@(ThreeReg op rd rs rt) = do
  m <- get
  let rsval = Map.findWithDefault (error $ "uninitialized register " ++ show rs) rs (regs m)
  let rtval = Map.findWithDefault (error $ "uninitialized register " ++ show rt) rt (regs m)
  case op of
    ADD -> do writeToReg rd (rsval + rtval)
              m' <- get
              put $ m' { pc = (pc m') + 1 }
    SUB -> do writeToReg rd (rsval - rtval)
              m' <- get
              put $ m' { pc = (pc m') + 1 }
    MUL -> do writeToReg rd (rsval * rtval)
              m' <- get
              put $ m' { pc = (pc m') + 1 }
    DIV -> do writeToReg rd (rsval `div` rtval)
              m' <- get
              put $ m' { pc = (pc m') + 1 }
    AND -> do writeToReg rd (rsval .&. rtval)
              m' <- get
              put $ m' { pc = (pc m') + 1 }
    OR ->  do writeToReg rd (rsval .|. rtval)
              m' <- get
              put $ m' { pc = (pc m') + 1 }
    XOR -> do writeToReg rd (rsval `xor` rtval)
              m' <- get
              put $ m' { pc = (pc m') + 1 }
    MOD -> do writeToReg rd (rsval `mod` rtval)
              m' <- get
              put $ m' { pc = (pc m') + 1 }
    _ -> error $ "Invalid instruction " ++ show instr
  m' <- get
  let next_instr = IntMap.findWithDefault (Lone END) (pc m') (memory m')
  process next_instr
  return ()
process instr@(TwoRegOneVal op rd rs (IMM16 imm)) = do
  m <- get
  let rsval = Map.findWithDefault (error $ "uninitialized register " ++ show rs) rs (regs m)
  case op of
    ADD -> do writeToReg rd (rsval + imm)
              m' <- get
              put $ m' { pc = (pc m') + 1 }
    AND -> do writeToReg rd (rsval .&. imm)
              m' <- get
              put $ m' { pc = (pc m') + 1 }
    LDR -> let memval = IntMap.findWithDefault (OneVal BINARY (IMM16 0)) (rsval+imm) (memory m) in
      case memval of
        OneVal BINARY (IMM16 i) -> put $ m {regs = Map.insert rd i (regs m), pc = (pc m) + 1}
        _ -> error $ "Invalid load " ++ show instr
    STR -> let rtval = Map.findWithDefault (error $ "uninitialized register " ++ show rd) rd (regs m) in
      put $ m {memory = IntMap.insert (rsval + imm) (OneVal BINARY (IMM16 rtval)) (memory m), pc = (pc m) + 1}
    SLL -> do writeToReg rd (rsval `shift` imm)
              m' <- get
              put $ m' { pc = (pc m') + 1 }
    SRA -> do writeToReg rd (rsval `shiftR` imm)
              m' <- get
              put $ m' { pc = (pc m') + 1 }
    SRL -> do let newshift = shiftR (intToWord rsval) imm
              writeToReg rd $ wordToInt newshift
              m' <- get
              put $ m' { pc = (pc m') + 1 }
    _ -> error $ "Invalid instruction " ++ show instr 
  m' <- get
  let next_instr = IntMap.findWithDefault (Lone END) (pc m') (memory m')
  process next_instr
  return ()
process instr@(TwoReg op rs rt) = do
  m <- get
  let rsval = Map.findWithDefault (error $ "uninitialized register " ++ show rs) rs (regs m)
  let rtval = Map.findWithDefault (error $ "uninitialized register " ++ show rt) rt (regs m)
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
            put $ m { psr = (setBit (clearBit (clearBit (psr m) 0) 1) 2), pc = (pc m) + 1 }
          else if (sub == 0) then
            put $ m {psr = (setBit (clearBit (clearBit (psr m) 0) 2) 1), pc = pc m + 1 }
          else
            put $ m {psr = (setBit (clearBit (clearBit (psr m) 1) 2) 0), pc = pc m + 1 }
    NOT -> put $ m { regs = Map.insert rs (complement rtval) (regs m), pc = pc m + 1}
    _ -> error $ "Invalid instruction " ++ show instr
  m' <- get
  let next_instr = IntMap.findWithDefault (Lone END) (pc m') (memory m')
  process next_instr
  return ()
process instr@(OneRegOneVal op rs (IMM16 imm)) = do
  m <- get
  let rsval = Map.findWithDefault (error $ "undefined register " ++ show rs) rs (regs m)
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
    CONST -> do writeToReg rs imm
                m' <- get
                put $ m' { pc = (pc m') + 1 }
    HICONST -> do let newval = ((rsval .&. 255) .|. (imm `shiftL` 8))
                  writeToReg rs newval
                  m' <- get
                  put $ m' { pc = (pc m') + 1 }
    _ -> error $ "Invalid instruction " ++ show instr
  m' <- get
  let next_instr = IntMap.findWithDefault (Lone END) (pc m') (memory m')
  process next_instr
  return ()
process (OneRegOneStr op rd label) = do
  m <- get
  case op of
    LEA -> do writeToReg rd (Map.findWithDefault (error $ "Cannot find label " ++ label) label (labels m))
              m' <- get
              put $ m' { pc = (pc m') + 1 }
    LC ->  do writeToReg rd (Map.findWithDefault (error $ "Cannot find label " ++ label) label (labels m))
              m' <- get
              put $ m' { pc = (pc m') + 1 }
    _ -> return ()
  m' <- get
  let next_instr = IntMap.findWithDefault (Lone END) (pc m') (memory m')
  process next_instr
  return ()
process (OneReg op rs) = do
  m <- get
  let rsval = Map.findWithDefault (error $ "Uninitialized register " ++ show rs) rs (regs m)
  case op of
    JSRR -> do writeToReg R7 (pc m + 1)
               m' <- get
               put $ m' { pc = rsval }
    JMPR -> put $ m { pc = rsval }
    _ -> return ()
  m' <- get
  let next_instr = IntMap.findWithDefault (Lone END) (pc m') (memory m')
  process next_instr
  return ()
process (OneVal op (IMM16 imm)) = do -- imm is ABSOLUTE ADDRESS to jump to
  m <- get
  let nzp = (psr m) .&. 7
  let next_pc i = if nzp .&. i /= 0 then imm else pc m + 1
  case op of
    BRn -> put $ m { pc = next_pc 4 }
    BRnz -> put $ m { pc = next_pc 6 }
    BRnp -> put $ m { pc = next_pc 5 }
    BRz -> put $ m { pc = next_pc 2 }
    BRzp -> put $ m { pc = next_pc 3 }
    BRp -> put $ m { pc = next_pc 1 }
    BRnzp -> put $ m { pc = imm }
    JSR -> do writeToReg R7 (pc m + 1)
              m' <- get
              put $ m' { pc = ((pc m) .&. 32768) .|. (imm `shift` 4) }
    JMP -> put $ m { pc = imm }
    TRAP -> do writeToReg R7 (pc m + 1)
               m' <- get
               put $ m' { pc = 32768 .|. imm, psr = (psr m) `setBit` 15 }
    _ -> return ()
  m' <- get
  let next_instr = IntMap.findWithDefault (Lone END) (pc m') (memory m')
  process next_instr
  return ()
process (OneStr op label) = do
  m <- get
  let imm = Map.findWithDefault (error $ "label " ++ label ++ " not found") label (labels m)
  process (OneVal op (IMM16 imm))
  return ()
process (Lone op) = do
  m <- get
  case op of
    NOP -> put $ m
    RTI -> put $ m {psr = (psr m) `clearBit` 15, pc = Map.findWithDefault (error "R7 not initialized") R7 (regs m) }
    _ -> return ()
  m' <- get
  let next_instr = IntMap.findWithDefault (Lone END) (pc m') (memory m')
  process next_instr
  return ()
process _ = return ()
  