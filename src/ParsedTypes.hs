module ParsedTypes where

data Problem = Problem
    { insts :: [Inst] }
    deriving (Show, Eq, Ord)

data Inst = Inst
    { jobs :: [Job]
    , num_machines :: Int
    , states :: [PState]
    , deadline :: Double }
    deriving (Show, Eq, Ord)

data Job = Job
    { job_id :: Int,
      job_ops :: Double,
      preds :: [Int]
    }
    deriving (Show, Eq, Ord)

data PState = PState {
    state_id :: Int,
    state_freq :: Double,
    state_power :: Double
    }
    deriving (Show, Eq, Ord)

