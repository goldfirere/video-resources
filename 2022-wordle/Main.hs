module Main where

import Control.DeepSeq

import Words

main :: IO ()
main = do
  print "hello, world!"
  -- print (guessWords `deepseq` finalWords `deepseq` "hello, world!")

  -- print guessWords
  -- print finalWords

-- number of bytes absolutely necessary (# of chars): 64860
-- measured amount of memory (with lists):          4930232
-- measured amount of memory (with Vector Text):    2128568
-- measured baseline (no data storage):               52760
-- minimum possible: baseline + "number of bytes":   117620

-- total # of words: 12972
-- each cell of the vector stores a pointer. Each pointer is 8 bytes.
-- 8 * 12972 = 103776 bytes of pointers
-- 2 * 64860 = 129720 bytes of characters
-- 8 * 12972 = 103776 bytes of length
-- total:     337272
-- observed: 2128568