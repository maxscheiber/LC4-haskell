{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

-- | Author: Max Scheiber, University of Pennsylvania '15, maxnscheiber@gmail.com
-- | Version: 0.1
-- | Assembly and simulation of the LC4 ISA.

module LC4.LC4 where
import Prelude
import Parser.Parser
import Parser.Base
import LC4.LC4Parser
import LC4.Types
import Data.Map hiding (filter)
import System.IO

-- | Current Machine state.
m :: Machine
m = Machine { pc = 0
            , psr = 0
            , regs = empty :: Map Reg Int
            , labels = empty :: Map String Int
            }

asmParser :: Parser String [Line]
asmParser = many lineP

parseASM :: Parser String [Line] -> String -> Either String [Line]
parseASM p str = case parse p str of
  []       -> Left "No possible parse"
  [([],_)] -> Left "No possible parse"
  [(a,_)]  -> Right $ filter uncomment a
  _        -> Left "Multiple possible parses" 
  where
    uncomment :: Line -> Bool
    uncomment (Comment _) = False
    uncomment _           = True

readASM :: Parser String [Line] -> String -> IO (Either String [Line])
readASM p name = do
  f <- openFile name ReadMode
  str <- hGetContents f
  return $ parseASM p str