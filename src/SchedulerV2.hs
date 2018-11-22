{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

-- {-# LANGUAGE Strict #-}




module SchedulerV2 (solveInstance, formatSolution, Solution(..)) where

import           ParsedTypes as PT hiding (jobs, states, job_id, deadline)
import qualified ParsedTypes as PT
import Util

import qualified Data.Text.Lazy as T

import Prelude as P
import Data.Maybe
import Data.List as L
import Data.Foldable as F

import Data.Vector as V hiding ((++))
import qualified Data.Vector.Unboxed as VU hiding ((++))

import Data.IntMap.Strict as M
import Data.IntSet as S
import Data.Set as SS
import Data.Set.Internal as SS

import Debug.Trace

import Debug.Pretty.Simple
import Text.Pretty.Simple
import Control.Monad as M

import System.Clock
import GHC.Exts (sortWith)
import GHC.Stack

import LogicStrict --Backtracking
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Applicative


type JobId = Int -- MkJobId { jobId :: Int } deriving (Eq, Show, Ord)
type Time = Double
type JobSet = IntSet
type JobVec = Vector

type MachineId = Int
type PStateId = Int

data JobInfo = JobInfo
    { -- jobCost :: Double
    --, jobTime :: Double
      jobStart :: !Double
    , jobEnd :: !Double
    , jobMachine :: !Int
    , jobState :: !Int
    } deriving (Eq, Show)

data ScheduleState = ScheduleState
    { openJobs :: !JobSet
    , doneJobs :: !JobSet
    , runningJobs :: !JobSet
    , time :: !Double
    , assignedTime :: !Double
    , assignedEnergy :: !Double
    , assignedWork :: !Double
    , jobInfo :: !(IntMap JobInfo)
    , machineUsed :: !(IntMap Double) -- There is a job running until the given time.
    , s_pstate :: !Int
    , jobSelector :: JobSet -> SM [JobId]
    , nextStop :: !Int -- ^ Branche after x jobs have finished
    } -- deriving Show

instance Show ScheduleState where
    show state =
        "open " ++ (show $ openJobs state) ++
        "\ndone " ++ (show $ doneJobs state) ++
        "\ninProgress " ++ (show $ runningJobs state) ++
        "\ntime " ++ (show $ time state)
        -- "open" ++ show openJobs ++
        -- "open" ++ show openJobs ++
        -- "open" ++ show openJobs
         ++ "\n"


data Solution = Solution
    { total_time :: Double
    , energy :: Double
    , jobSchedules :: [(JobId, MachineId, PStateId, Double, Double)]
    , jobCount :: Int
    }
    deriving (Eq, Show)

data InstanceInfo = InstanceInfo
    { deadline  :: {-# UNPACK #-} !Double
    , jobWork   :: !(VU.Vector Double)
    , states    :: !(Vector PState)
    , jobSuccs  :: !(JobVec JobSet) -- From job to list of dependant jobs
    , jobPreds  :: !(JobVec JobSet) -- From job to list of dependant jobs
    , m_count   :: {-# UNPACK #-} !Int
    , totalWork :: {-# UNPACK #-} !Double
    }

data GlobalState = GlobalState
    { lowerBound :: !Double
    , maxRuntime :: !TimeSpec
    } deriving (Eq, Show)

-- lower bound for the required energy
estimateEnergyBound :: InstanceInfo -> ScheduleState -> Double
estimateEnergyBound info state =
    assignedEnergy state + remainingEnergy
  where
    remainingWork = totalWork info - assignedWork state
    bestState = states info V.! 1 --lowest non idling job
    remainingTime = remainingWork / state_freq bestState
    remainingEnergy = remainingTime * state_power bestState

-- lowerbound for the required time
estimateTimeBound :: InstanceInfo -> ScheduleState -> Double
estimateTimeBound info state =
    time state + (remainingTime / (fromIntegral $ m_count info))
  where
    remainingWork = totalWork info - assignedWork state
    bestState = V.last $ states info --FULL POWER
    remainingTime = remainingWork / state_freq bestState

type LocalState = ScheduleState
-- type SM = ListT (State GlobalState)
type SM = LogicT (StateT GlobalState IO)

setLower :: Double -> SM ()
setLower l = do
    s <- lift $ get
    lift $ put s{lowerBound = l}

getLower :: SM Double
getLower = do
    lift (lowerBound <$> get)

--After that, print for each job (in one line) its job id, the machine it was allocated to, the
--p-state used for that job, the start time, and the completion time. Print a newline after each
--scheduling instance.
extractSolution :: -- HasCallStack =>
    InstanceInfo -> ScheduleState -> Solution
extractSolution info (ScheduleState {time = time, doneJobs = doneJobs,
        jobInfo = jobInfo, assignedEnergy = assignedEnergy, assignedTime = assignedTime, machineUsed = mUsed})
    = Solution
        { total_time = time
        , energy = assignedEnergy + idleEnergy
        , jobCount = S.size doneJobs
        , jobSchedules = schedules }
  where
    idleTime = time * (fromIntegral $ M.size mUsed) - assignedTime
    idleEnergy = idleTime * (state_power . V.head $ states info )
    jobInfos = M.toList jobInfo :: [(JobId,JobInfo)]
    getSchedule id jobInfo = (id, jobMachine jobInfo, jobState jobInfo, jobStart jobInfo, jobEnd jobInfo)
    schedules = fmap (uncurry getSchedule) jobInfos

formatSolution :: -- HasCallStack =>
    Solution -> String
formatSolution Solution
        { total_time = time
        , energy = energy
        , jobCount = jobCount
        , jobSchedules = schedules } = unlines $ [show jobCount, show time, show energy] ++ L.map showJob schedules
  where
    showJob (id,m,power,start,end) =
        unwords $ [show id, show m, show power, show start, show end]

{-# NOINLINE solveInstance #-}
solveInstance :: -- HasCallStack =>
    Inst -> IO Solution
solveInstance (Inst jobs m_count' states' deadline') = do
    let !machineUsed = M.fromList $ L.zip [1..m_count'] (repeat 0.0)
    let !instanceInfo = InstanceInfo
            { deadline = deadline'
            , jobSuccs = jobSuccs' -- From job to list of dependant jobs
            , jobPreds = jobPreds'
            , states = V.fromList states'
            , m_count = m_count'
            , jobWork = jobWork'
            , totalWork = VU.sum jobWork'
            }

    let !initialState = ScheduleState
            { openJobs = S.fromList jobIds
            , runningJobs = S.empty
            , doneJobs = S.empty
            , time = 0
            , assignedTime = 0
            , assignedEnergy = 0
            , assignedWork = 0
            , jobInfo = M.empty
            , machineUsed = machineUsed
            , s_pstate = 2
            , jobSelector = (return . S.toList :: JobSet -> SM [JobId])
            , nextStop = 0
            }

    let !jobCount = F.length jobs
    let decisionPoints = 0

    !maxRuntime' <- (+ TimeSpec {sec = 60, nsec = 0}) <$> getTime Monotonic
    let !lowerBound' = 1/0

    let startState = GlobalState { lowerBound = lowerBound', maxRuntime = maxRuntime'}

    finalSolutions <- (flip evalStateT) startState $ observeAllT $ do
    -- let finalSolutions = (flip evalState) (GlobalState { lowerBound = 1/0 }) $ runListT $ do
                useState <- return 5 --asum $ fmap return [2 .. L.length states'] :: SM Int
                initialState <- return $ initialState { s_pstate = useState }
                schedule instanceInfo initialState :: SM Solution

    let finalSolution =
            L.minimumBy (\s1 s2 ->
                        compare (energy s1)
                                (energy s2)) $
            L.filter (\s -> total_time s <= deadline') $
            finalSolutions

    return finalSolution

  where
    validSolution info sol = total_time sol <= deadline info
    jobIds = fmap PT.job_id jobs :: [JobId]
    succs = getSuccsFromJobs jobs :: IntMap (JobSet)
    jobPreds' = V.fromList $ L.map (S.fromList . PT.preds) jobs
    jobSuccs' = V.fromList $ L.map (\j -> fromMaybe S.empty (M.lookup j succs)) [1 .. L.length jobs]
    jobWork' = VU.fromList $ fmap job_ops jobs

getSuccsFromJobs :: -- HasCallStack =>
    [Job] -> IntMap (JobSet)
getSuccsFromJobs jobs =
    fromListWith (S.union) succsList
  where
    succsList :: [(Int,JobSet)]
    succsList = foldMap getSucc jobs
    getSucc (Job id _ops preds) =
        fmap (,S.singleton id) preds

{-# INLINE getPreds #-}
getPreds :: -- HasCallStack =>
    InstanceInfo -> JobId -> JobSet
getPreds info job =
    jobPreds info V.! (job - 1)

{-# INLINE getSuccs #-}
getSuccs :: -- HasCallStack =>
    InstanceInfo -> JobId -> JobSet
getSuccs info job =
    jobSuccs info V.! (job - 1)



{-
    Basic idea:
        * Schedule jobs greedily by the following order:
        * Job can be started.
        * Job has the highest outdegree
        * Break ties by work required for job in favour of longer job
        * Schedule the next job by this ordering at the next free slot.
-}

{- Iterativly schedule jobs:
    * Calculate a list of runable jobs
    * Sort them according to the priority given above
    * If there are n idling machines schedule the first n jobs.
    * Jump to the next point of interest. That is when the next job finishes.
    * If there are jobs left: Repeat
    * If there are no jobs left: We are done
-}

schedule :: -- HasCallStack =>
    InstanceInfo -> ScheduleState -> SM Solution
schedule info initialState =
    go initialState
  where
    go :: ScheduleState -> SM Solution
    go state
      | nextStop state <= 0
      , openJobCount <- S.size (openJobs state)
      , openJobCount >= 4
      = do
        let nextStop' = ceiling ((fromIntegral openJobCount) / 2)

        let rotDep :: Int -> JobSet -> SM [JobId]
            rotDep n js = if S.size js <= 1 then fail "Can't rotate" else (rotate n <$> depSelector js) :: SM [JobId]

        !jobSelector' <- choice $ [depLvlSel, return . S.toList, depSelector, depWorkSel, rotDep 1, revWorkSel]
        !pstate' <- choice $ [2.. V.length (states info)]
        --traceM $ show (openJobCount, nextStop')

        go state
            { nextStop = nextStop'
            , s_pstate = pstate'
            , jobSelector = jobSelector'
            }

      -- Finished
      | S.null (openJobs state)
      , S.null (runningJobs state) = do
        let solution = extractSolution info state
        !currentBound <- getLower
        let !energy' = energy solution
        if (energy' < currentBound && total_time solution <= deadline info)
            then do
                -- traceM $ "new bound - (energy, oldBound, time) - " ++ show (energy', currentBound, total_time solution)
                setLower energy'
            else do
                -- traceM $ ("Final solution not feasible: (energy, bound, time) - ") ++ show (energy', currentBound, total_time solution)
                fail "123"
        --trace "done" $
        return solution
      | S.null doableJobs = do
        --trace "NextPoint2" $
        go $ moveNextPoint state

      | otherwise = do
        let freeMachines = L.filter machineAvailable [1 .. M.size (machineUsed state)] :: [MachineId]
        nextJobs <- jobSelector state $ doableJobs :: SM [JobId]

        --place jobs
        !scheduled <- scheduleJobs info state nextJobs freeMachines
        --advance
        let !advanced = (moveNextPoint scheduled)
        --advance to next point in time

        --Trim search space
        let estimatedEnergy = estimateEnergyBound info state
        let estimatedTime = estimateTimeBound info state
        lowEnergyBound <- getLower
        maxTime' <- maxRuntime <$> lift get
        when (estimatedEnergy >= lowEnergyBound) $ do
            -- traceM $ "Worse than existing: " ++ show (estimatedEnergy)
            fail "Worse than existing solution"
        when (estimatedTime > deadline info) $ do
            -- traceM $ "Failed deadline - time - " ++ show estimatedTime
            fail "Deadline not held"

        currentTime <- liftIO $ getTime Monotonic
        when (currentTime > maxTime') $ do
            -- traceM "Abort - Timeout"
            fail "Timeout"


        go advanced
      where
        predsSatisfied :: JobId -> Bool
        predsSatisfied job = P.all (\j -> S.member j $ doneJobs state) (S.toList $ getPreds info job)
        doableJobs = S.filter (\j ->predsSatisfied j)  (openJobs state) :: JobSet
        machineAvailable :: MachineId -> Bool
        machineAvailable m =
            let takenTill = M.findWithDefault (error "Must exist") (m) (machineUsed state)
            in takenTill <= time state
        depSelector :: JobSet -> SM [JobId]
        depSelector doableJobs =
            return $ L.filter (\j -> S.member j doableJobs) orderedJobsByDeps :: SM [JobId]
        depLvlSel doableJobs = return $ L.filter (\j -> S.member j doableJobs) jobOrderByDepLevel
        depWorkSel doableJobs = return $ L.filter (\j -> S.member j doableJobs) jobOrderByDepWork
        revWorkSel doableJobs = return $ L.filter (\j -> S.member j doableJobs) revWorkOrder

    orderJobsBy :: (JobId -> JobId -> Ordering) -> [JobId]
    orderJobsBy f =
        sortBy f $ [1 .. VU.length (jobWork info)]
    orderedJobsByDeps = orderJobsBy jobOrderByDeps :: [JobId]
    orderedJobsByWork = orderJobsBy jobOrderByWork
    jobOrderByDepLevel = L.sortOn jobDepLevel orderedJobsByDeps
    jobOrderByDepWork = L.sortOn jobDepWork orderedJobsByDeps
    revWorkOrder = L.reverse orderedJobsByWork :: [JobId]


    varyTipOrder :: [JobId] -> SM [JobId]
    varyTipOrder (x1:x2:x3:x4:xs) = do
        !tip <- choice $ permutations [x1,x2,x3,x4]
        return $ tip ++ xs
    varyTipOrder xs = do
        choice $ permutations xs

    -- | Order two jobs, prefer the one which has more jobs
    -- depending on it. If they have the same number prefer the
    -- longer one.
    jobOrderByDeps :: -- HasCallStack =>
        JobId -> JobId -> Ordering
    jobOrderByDeps id1 id2
        | sats1 < sats2 = LT
        | sats1 > sats2 = GT
        | ops1 < ops2 = LT
        | ops1 > ops2 = GT
        | otherwise = compare id1 id2
      where
        sats1 = S.size (getSuccs info id1)
        sats2 = S.size (getSuccs info id2)
        ops1 = jobWork info VU.! (id1 - 1)
        ops2 = jobWork info VU.! (id2 - 1)
    -- | Order two jobs, prefer the one which has more work.
    jobOrderByWork :: -- HasCallStack =>
        JobId -> JobId -> Ordering
    jobOrderByWork id1 id2
        | ops1 < ops2 = LT
        | ops1 > ops2 = GT
        | sats1 < sats2 = LT
        | sats1 > sats2 = GT
        | otherwise = compare id1 id2
      where
        ops1 = jobWork info VU.! (id1 - 1)
        ops2 = jobWork info VU.! (id2 - 1)
        sats1 = S.size (getSuccs info id1)
        sats2 = S.size (getSuccs info id2)
    jobDepLevel :: JobId -> Int
    jobDepLevel job
      | S.null deps = 0
      | otherwise =
        1 + (F.maximum . fmap jobDepLevel $ S.toList deps)
      where
        deps = jobSuccs info V.! (job-1)
    jobDepWork :: JobId -> Double
    jobDepWork job =
        S.foldl' (\work j -> work + jobWork info VU.! (j-1)) work deps
      where
        work = jobWork info VU.! (job - 1)
        deps = jobSuccs info V.! (job - 1)


powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = L.map (x:) (powerset xs) ++ powerset xs

scheduleJobs :: -- HasCallStack =>
    InstanceInfo -> ScheduleState -> [JobId] -> [MachineId] -> SM ScheduleState
scheduleJobs info state jobs machines = do
    let jobs' = jobs

    -- !combination <- asum . fmap return $ combos
    return $ F.foldl' (\s (j,m) -> scheduleJob info s j m) state $ L.zip jobs' machines

scheduleJob :: -- HasCallStack =>
    InstanceInfo -> ScheduleState -> JobId -> MachineId -> ScheduleState
scheduleJob info state job_id m_id = do
    let openJobs' = S.delete job_id $ openJobs state
    let runningJobs' = S.insert job_id $ runningJobs state
    let p_state_id = s_pstate state
    -- !p_state_id <- asum $ fmap return [2..5]
    let p_state = states info V.! (p_state_id - 1)
    let jobWork' = jobWork info VU.! (job_id - 1)
    let job_time = jobWork' / state_freq p_state
    let start_time = time state
    let end_time = start_time + job_time
    let energy_cost = job_time * state_power p_state
    let job' = JobInfo
            { --jobCost = energy_cost
            --, jobTime = job_time
              jobStart   = start_time
            , jobEnd     = end_time
            , jobMachine = m_id
            , jobState   = state_id p_state }
    let assignedTime' = assignedTime state + job_time
    let assignedEnergy' = assignedEnergy state + energy_cost
    let assignedWork' = assignedWork state + jobWork'
    let jobInfo' =     M.insert (job_id) (job') (jobInfo state)
    let machineUsed' = M.insert (m_id) end_time      (machineUsed state)
    state
        { openJobs          = openJobs'
        , runningJobs       = runningJobs'
        , assignedTime      = assignedTime'
        , assignedEnergy    = assignedEnergy'
        , assignedWork      = assignedWork'
        , jobInfo           = jobInfo'
        , machineUsed       = machineUsed'
        }


-- Schedule an idle job on all idling machines which lasts until the next work job is done.
{- Move to the end of next finished job
* Calculate next time
* Update finished jobs
* The next time is always based on the next finished job,
  so we only have to check the job shedueld last.
-}
moveNextPoint :: -- HasCallStack =>
    ScheduleState -> ScheduleState
moveNextPoint state =
    -- traceM $ show state
    let nextEndTime = getNextEndTime (machineUsed state) (time state) :: Double
        done = doneJobs' nextEndTime
    in  state
        { time = nextEndTime
        , doneJobs = S.union (doneJobs state) done
        , runningJobs = S.foldl' (flip S.delete) (runningJobs state) done
        , nextStop = nextStop state - S.size done }
  where
    getNextEndTime :: IntMap Double -> Double -> Double
    getNextEndTime vm time =
        F.minimum $ M.filter (>time) vm
    jobDone :: Double -> JobId -> Bool
    jobDone time j =
        let info = M.findWithDefault (error "Must exist 4") (j) (jobInfo state)
            end = jobEnd info
        in  (end <= time)
    doneJobs' :: Double -> JobSet
    doneJobs' nextPoint =
        S.filter (\j -> jobDone nextPoint j) $ (runningJobs state)










