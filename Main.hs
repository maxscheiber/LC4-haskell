{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

-- | Author: Max Scheiber, University of Pennsylvania '15, maxnscheiber@gmail.com
-- | Version: 0.1
-- | Runner class for the LC4 simulation.

module Main where
import LC4.LC4

main = do
  res <- readASM (asmParser) "os.asm"
  case res of
    Right x -> putStr x
    Left _ -> return ()
  return ()