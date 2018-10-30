module Main where

import System.IO
import Control.Monad
import Parsers_and_convertors

import qualified Data.Text.IO as T_IO
import Data.Attoparsec.Text
import qualified Data.Text as T

import Control.Concurrent
import Control.Concurrent.MVar   
 
-- slowPut :: MVar String -> MVar String -> IO ()
-- slowPut mv1 mv2 = do 
--     threadDelay (2 * 10^6)
--     putStrLn "Put val 1"
--     putMVar mv1 "mv1 smth"
--     threadDelay (2 * 10^6)
--     putStrLn "Put val 2"
--     putMVar mv2 "mv2 smth"
    
-- main = do 
--     mv1 <- newEmptyMVar
--     mv2 <- newEmptyMVar  
--     forkIO $ putStrLn "Start slowPut" >> slowPut mv1 mv2
--     r1 <- takeMVar mv1
--     putStrLn $ "Take first: " ++ r1
--     r2 <- takeMVar mv2
--     putStrLn $ "Take second: " ++ r2

main :: IO ()
main = do
        content <- T_IO.readFile "examples/example_1.txt"
        let res = parseOnly projectParser content
        putStrLn $ show res
        let extract_text = (\res -> case res of
                                        Left mess  -> mess
                                        Right proj -> T.unpack $ projectToText proj)
        putStrLn $ extract_text res
        return ()
