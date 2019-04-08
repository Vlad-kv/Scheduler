module Main where

import System.IO
import Control.Monad

import Project_structure
import Parsers_and_convertors
import Work_with_graph
import Simple_console_executor

import qualified Data.Text.IO as T_IO
import Data.Attoparsec.Text
import qualified Data.Text as T

import qualified Data.Map as M

import Control.Concurrent
import Control.Concurrent.MVar

import System.Process
import System.Exit


main :: IO ()
main = runSimpleConsoleExecutor
