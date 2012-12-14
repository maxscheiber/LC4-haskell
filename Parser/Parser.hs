{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

-- | Author: Max Scheiber, University of Pennsylvania '15, maxnscheiber@gmail.com
-- | Version: 0.1
-- | Mondaic parsing library, based off of Dr. Stephanie Weirich's version for
-- |   CIS 552, University of Pennsylvania.

module Parser.Parser (
                Parser                 
              , get
              , (<|>)
              , satisfy
              , parse  
              ) where

newtype Parser t a = P (t -> [(a, t)])

parse :: Parser t a -> t -> [(a, t)] 
parse (P p) s = p s

-- | Grab the next quantum to be parsed.
get :: Parser [a] a
get = P (\cs -> case cs of 
                  (x:xs) -> [ (x,xs) ]
                  []     -> [])

-- | Grab the next quantum if it satisfies p.
satisfy :: (a -> Bool) -> Parser [a] a
satisfy p = do 
  c <- get
  if p c then return c else fail "Did not satisfy boolean predicate"

-- | Try p2 if parsing p1 fails.
(<|>) :: Parser t a -> Parser t a -> Parser t a
p1 <|> p2 = P $ \cs -> case parse p1 cs of
                          []   -> parse p2 cs
                          x:_ -> [x]

instance Functor (Parser t) where
  fmap f p = p >>= \x -> return (f x)

instance Monad (Parser t) where
  p1 >>= fp2 = P (\cs -> do (a,cs') <- parse p1 cs 
                            parse (fp2 a) cs') 

  return x   = P (\cs -> [ (x, cs) ])

  fail _     = P (\_ ->  [ ])
