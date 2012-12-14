{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind  #-}

-- | Author: Max Scheiber, University of Pennsylvania '15, maxnscheiber@gmail.com
-- | Version: 0.1
-- | Mondaic parsing library, based off of Dr. Stephanie Weirich's version for
-- |   CIS 552, University of Pennsylvania.
module Parser.Base where

import Parser.Parser
import Data.Char
    
-- | Parsers for specific sorts of characters.
alpha, digit, upper, lower, space :: Parser String Char
alpha = satisfy isAlpha
digit = satisfy isDigit            
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isSpace
   
-- | Parses and returns the inputted a.
char :: Eq a => a -> Parser [a] a
char c = satisfy (c ==)   

-- | Parses and returns the specified [a].
string :: Eq a => [a] -> Parser [a] [a]
string = mapM char

-- | Parsers an integer.
int :: Parser String Int
int = do n <- string "-" <|> return []
         s <- rest digit  
         return $ (read (n ++ s) :: Int)

-- | Parses zero or more instances of p.
many   :: Parser t a -> Parser t [a]
many p = rest p <|> (return [])
                    
-- | Parses one or more instances of p.
rest :: Parser t a -> Parser t [a]
rest p = do x  <- p
            xs <- many p
            return (x:xs)
                   
-- | Combine all parsers in the list (sequentially)
unlist :: [Parser t a] -> Parser t a
unlist = foldr (<|>) (fail "Could not parse")

-- | Parses out whitespace from around something.
ws :: Parser String a -> Parser String a
ws p = do
  x <- p
  many space
  return x

