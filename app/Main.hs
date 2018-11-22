module Main where


import Parser
import qualified Data.Text as T
import Data.Text (Text)
import SchedulerV2
import ParsedTypes

import Control.Monad

import Text.Pretty.Simple

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    problem <- if (length args < 1)
        then do
            putStrLn "Usage: pgreedy <inputFilePath>\n"
            --error "No input file given"
            runParser $ "input/sample.txt"
        else
            runParser $ head args

    solutions <- mapM solveInstance (insts problem)
    mapM_ (putStrLn . formatSolution) solutions

    --print problem


    return ()
