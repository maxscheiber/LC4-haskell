{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

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

-- | UNSAFE METHOD, only use if positive hex is in right format
hexToInt :: String -> Int
hexToInt = fst . head . readHex

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