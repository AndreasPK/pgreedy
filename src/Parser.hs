{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Debug.Trace

import ParsedTypes
import Data.Text
import Data.Attoparsec.Text as P

import Control.Applicative
import Control.Monad

import Data.Char (isSpace)
import Data.List
import qualified Data.Text.IO as T
import Data.Attoparsec.Combinator (lookAhead, (<?>))

traceInput :: Parser ()
traceInput = return ()
    --     do
    -- s <- lookAhead (P.take 5)
    -- traceM $ "5: " ++ show (unpack s)
    -- return ()

runParser :: FilePath -> IO Problem
runParser filePath = do
    f <- T.readFile filePath
    let result = parseOnly problem f
    case result of
        Right r -> return r
        failedResult -> error $ "Failed to parse Input:" ++ show failedResult



nDoubles :: Int -> Parser [Double]
nDoubles n = replicateM n (double <* skipSpace) <?> "Doubles"

linespace :: Parser Char
linespace = char ' ' <|> char '\t' <?> "linespace"

problem :: Parser Problem
problem = do
    num_inst <- decimal <* skipSpace <?> "instanceCount"
    instances <- replicateM num_inst inst <?> "instances"
    endOfInput
    return $ Problem instances

inst :: Parser Inst
inst = do
    num_jobs <- decimal <* skipSpace <?> "nj"

    job_ops <- nDoubles num_jobs <?> "job_work"

    num_machines <- decimal <* skipSpace

    num_states <- decimal <* skipSpace
    state_freqs <- nDoubles num_states :: Parser [Double]
    state_power <- nDoubles num_states :: Parser [Double]

    job_deps <- replicateM num_jobs pPreds <?> "Preds"

    deadline <- double <* skipSpace <?> "deadline"
    skipSpace

    let jobs = zipWith3 Job [1..] job_ops job_deps
    let states = zipWith3 PState [1..] state_freqs state_power

    return $ Inst jobs num_machines states deadline

mkJob :: Int -> Double -> [Int] -> Job
mkJob id work deps =
    Job id work deps

pPreds :: Parser [Int]
pPreds = do
    pred_list <- manyTill
        (decimal <* (many' linespace))
        endOfLine
    -- Predecessor of zero indicates no predecessor
    return (Prelude.filter (/= 0) pred_list)



