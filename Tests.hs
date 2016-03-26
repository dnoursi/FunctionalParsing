module Main where

import Parsing
import Data.List
import System.IO

test0 = primary "56 7 +" == 63
test1 = primary "(8) 6  4    + -" == -2
test2 = primary "66 3 / 5 +" == 27
test3 = primary "4! 6 + " == 30
tests :: [Bool]
tests=[test3,test2,test1,test0]

testAll :: Bool
testAll = and tests

testPasses =  (sum $ map fromEnum tests)

firstFailIndex = elemIndex False tests

main :: IO()
main = do
    if testAll then putStrLn("All "++ (show testPasses) ++  " tests have completed successfully") else putStrLn("naw")

