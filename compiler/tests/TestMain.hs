{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import System.Environment ( getArgs )
import Test.Framework
import qualified LexerTests
import qualified ParserTests


myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs

test_nonEmpty = do 
  assertEqual [1] (myReverse [1])
  assertEqual [3,2,1] (myReverse [1,2,3])                   

test_empty = assertEqual ([] :: [Int]) (myReverse [])

prop_reverse :: [Int] -> Bool
prop_reverse xs = xs == (myReverse (myReverse xs))

main = do args <- getArgs
          runTestWithArgs args 
            [ LexerTests.allHTFTests
            , ParserTests.allHTFTests
            ]
                    
