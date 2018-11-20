module Main where

import Test.Tasty (defaultMain, testGroup)
import Unit (hspecTestTree)

main :: IO ()
main = hspecTestTree >>= \unitTests ->
       let allTests = testGroup "Sheduler" [unitTests]
       in defaultMain allTests
