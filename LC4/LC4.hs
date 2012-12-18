{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -XScopedTypeVariables  #-}

-- | Author: Max Scheiber, University of Pennsylvania '15, maxnscheiber@gmail.com
-- | Version: 0.2
-- | Assembly and simulation of the LC4 ISA.

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

-- | Some useful methods for changing state of simulation; also used for REPL
setReg :: Reg -> Int -> State Machine ()
setReg rd val = do
  m <- get
  put $ m { regs = Map.insert rd val (regs m) }
  setPSR val
  return ()

setPSR :: Int -> State Machine ()
setPSR val = do
  m <- get
  let nzp' = if val > 0 then 1 else if val == 0 then 2 else 4
  put $ m { psr = (hexToInt "8000" .&. psr m) .|. nzp' }
  return ()

setPC :: Int -> State Machine ()
setPC pc' = do
  m <- get
  put $ m { pc = pc' }
  return ()

-- | Some useful constants and related functions
userCode, userData, osCode, osData, vidMem, devices :: Int
userCode = 0
userData = hexToInt "4000"
osCode   = hexToInt "8000"
osData   = hexToInt "A000"
vidMem   = hexToInt "C000"
devices  = hexToInt "FE00"

kbsr, kbdr, adsr, addr, tsr, tir, vdcr :: Int
kbsr = hexToInt "FE00"
kbdr = hexToInt "FE02"
adsr = hexToInt "FE04"
addr = hexToInt "FE06"
tsr  = hexToInt "FE08"
tir  = hexToInt "FE0A"
vdcr = hexToInt "FE0C"

-- | Some error checking and error helpers
inUserData, inOSData :: Int -> Bool
inUserData i = userData <= i && i < osCode
inOSData   i = osData   <= i && i < devices

regError :: forall t a. Show a => a -> t
regError r = error $ "undefined register " ++ show r

instrError :: forall t a. Show a => a -> t
instrError i = error $ "invalid instruction " ++ show i

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

{- loadOBJ :: [Line] -> Machine
loadOBJ parsed = execState (preprocess parsed) machine -}

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
  let rsval = Map.findWithDefault (regError rs) rs (regs m)
  let rtval = Map.findWithDefault (regError rt) rt (regs m)
  case op of
    ADD -> do setReg rd (rsval + rtval)
              m' <- get
              setPC $ 1 + pc m'
    SUB -> do setReg rd (rsval - rtval)
              m' <- get
              setPC $ 1 + pc m'
    MUL -> do setReg rd (rsval * rtval)
              m' <- get
              setPC $ 1 + pc m'
    DIV -> do setReg rd (rsval `div` rtval)
              m' <- get
              setPC $ 1 + pc m'
    AND -> do setReg rd (rsval .&. rtval)
              m' <- get
              setPC $ 1 + pc m'
    OR ->  do setReg rd (rsval .|. rtval)
              m' <- get
              setPC $ 1 + pc m'
    XOR -> do setReg rd (rsval `xor` rtval)
              m' <- get
              setPC $ 1 + pc m'
    MOD -> do setReg rd (rsval `mod` rtval)
              m' <- get
              setPC $ 1 + pc m'
    _ -> instrError instr
  m' <- get
  let next_instr = IntMap.findWithDefault (Lone END) (pc m') (memory m')
  process next_instr
  return ()

process instr@(TwoRegOneVal op rd rs (IMM16 imm)) = do
  m <- get
  let rsval = Map.findWithDefault (regError rs) rs (regs m)
  case op of
    ADD -> do setReg rd (rsval + imm)
              m' <- get
              setPC $ 1 + pc m'
    AND -> do setReg rd (rsval .&. imm)
              m' <- get
              setPC $ 1 + pc m'
    LDR -> let memval = IntMap.findWithDefault (OneVal BINARY (IMM16 0)) (rsval + imm) (memory m) in
      case memval of
        OneVal BINARY (IMM16 i) -> 
            let psr15 = psr m `testBit` 15 in
            if (inUserData (rsval + imm) || (psr15 && inOSData (rsval + imm))) then
              do setReg rd i
                 m' <- get
                 setPC $ 1 + pc m'
            else error $ "Cannot load from address " ++ show (rsval + imm) ++ 
              " with PSR[15] " ++ show psr15
        _ -> error $ "Invalid load " ++ show instr
    STR -> 
      let rtval = Map.findWithDefault (regError rd) rd (regs m) in
      let psr15 = psr m `testBit` 15 in
      if (inUserData (rsval + imm) || (psr15 && inOSData (rsval + imm))) then
        put $ m {memory = IntMap.insert (rsval + imm) (OneVal BINARY 
          (IMM16 rtval)) (memory m), pc = (pc m) + 1}
      else error $ "Cannot store to address " ++ show (rsval + imm) ++ 
        " with PSR[15] " ++ show psr15
    SLL -> do setReg rd (rsval `shift` imm)
              m' <- get
              setPC $ 1 + pc m'
    SRA -> do setReg rd (rsval `shiftR` imm)
              m' <- get
              setPC $ 1 + pc m'
    SRL -> do let newshift = shiftR (intToWord rsval) imm
              setReg rd $ wordToInt newshift
              m' <- get
              setPC $ 1 + pc m'
    _ ->  instrError instr
  m' <- get
  let next_instr = IntMap.findWithDefault (Lone END) (pc m') (memory m')
  process next_instr
  return ()

process instr@(TwoReg op rs rt) = do
  m <- get
  let rsval = Map.findWithDefault (regError rs) rs (regs m)
  let rtval = Map.findWithDefault (regError rt) rt (regs m)
  case op of
    CMP -> do setPSR $ rsval - rtval
              m' <- get
              setPC $ 1 + pc m'
    CMPU -> let unsigned1 = intToWord rsval in
            let unsigned2 = intToWord rtval in
            let sub = wordToInt (unsigned1 - unsigned2) in
            do setPSR sub
               m' <- get
               setPC $ 1 + pc m'
    NOT -> do setReg rs (complement rtval)
              m' <- get
              setPC $ 1 + pc m'
    _ ->  instrError instr
  m' <- get
  let next_instr = IntMap.findWithDefault (Lone END) (pc m') (memory m')
  process next_instr
  return ()

process instr@(OneRegOneVal op rs (IMM16 imm)) = do
  m <- get
  let rsval = Map.findWithDefault (regError rs) rs (regs m)
  case op of
    CMPI -> do setPSR $ rsval - imm
               m' <- get
               setPC $ 1 + pc m'
    CMPIU -> let unsigned1 = intToWord rsval in
             let unsigned2 = intToWord imm in
             let sub = wordToInt (unsigned1 - unsigned2) in
             do setPSR sub
                m' <- get
                setPC $ 1 + pc m'
    CONST -> do setReg rs imm
                m' <- get
                setPC $ 1 + pc m'
    HICONST -> do let newval = ((rsval .&. 255) .|. (imm `shiftL` 8))
                  setReg rs newval
                  m' <- get
                  setPC $ 1 + pc m'
    _ -> instrError instr
  m' <- get
  let next_instr = IntMap.findWithDefault (Lone END) (pc m') (memory m')
  process next_instr
  return ()

process instr@(OneRegOneStr op rd label) = do
  m <- get
  case op of
    LEA -> do setReg rd (Map.findWithDefault (error $ "Cannot find label " ++ label) label (labels m))
              m' <- get
              setPC $ 1 + pc m'
    LC ->  do setReg rd (Map.findWithDefault (error $ "Cannot find label " ++ label) label (labels m))
              m' <- get
              setPC $ 1 + pc m'
    _ -> instrError instr
  m' <- get
  let next_instr = IntMap.findWithDefault (Lone END) (pc m') (memory m')
  process next_instr
  return ()

process instr@(OneReg op rs) = do
  m <- get
  let rsval = Map.findWithDefault (regError rs) rs (regs m)
  case op of
    JSRR -> do setReg R7 (pc m + 1)
               setPC rsval
    JMPR -> setPC rsval
    _ -> instrError instr
  m' <- get
  let next_instr = IntMap.findWithDefault (Lone END) (pc m') (memory m')
  process next_instr
  return ()

process instr@(OneVal op (IMM16 imm)) = do -- imm is ABSOLUTE ADDRESS to jump to
  m <- get
  let nzp = (psr m) .&. 7
  let next_pc i = if nzp .&. i /= 0 then imm else pc m + 1
  case op of
    BRn ->   setPC $ next_pc 4
    BRnz ->  setPC $ next_pc 6
    BRnp ->  setPC $ next_pc 5
    BRz ->   setPC $ next_pc 2
    BRzp ->  setPC $ next_pc 3
    BRp ->   setPC $ next_pc 1
    BRnzp -> setPC $ imm
    JSR -> do setReg R7 (pc m + 1)
              m' <- get
              setPC $ ((pc m') .&. 32768) .|. (imm `shift` 4)
    JMP -> put $ m { pc = imm }
    TRAP -> do setReg R7 (pc m + 1)
               setPC $ 32768 .|. imm
               m' <- get
               put $ m' { psr = (psr m) `setBit` 15 }
    _ -> instrError instr
  m' <- get
  let next_instr = IntMap.findWithDefault (Lone END) (pc m') (memory m')
  process next_instr
  return ()

process (OneStr op label) = do
  m <- get
  let imm = Map.findWithDefault (error $ "label " ++ label ++ " not found") label (labels m)
  process (OneVal op (IMM16 imm))
  return ()

process instr@(Lone op) = do
  m <- get
  case op of
    NOP -> setPC $ 1 + pc m
    RTI -> do put $ m {psr = (psr m) `clearBit` 15 }
              m' <- get
              setPC $ Map.findWithDefault (regError R7) R7 (regs m')
    _ ->  instrError instr
  m' <- get
  let next_instr = IntMap.findWithDefault (Lone END) (pc m') (memory m')
  process next_instr
  return ()

process instr = instrError instr -- unknown instruction