{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}


module Scheduler where

import           ParsedTypes as PT hiding (jobs, states, job_id)
import qualified ParsedTypes as PT

import qualified Data.Text.Lazy as T

import Prelude as P
import Data.Maybe
import Data.List as L
import Data.Foldable as F

import Data.Vector as V hiding ((++))
import Data.IntMap.Strict as M
import Data.IntSet as S

import Debug.Trace

import Debug.Pretty.Simple
import Text.Pretty.Simple

import GHC.Exts (sortWith)




type JobId = Int -- MkJobId { jobId :: Int } deriving (Eq, Show, Ord)
type Time = Double
type JobSet = IntSet
type JobMap = IntMap
type MachineId = Int

data ScheduleState = ScheduleState
    { machines :: Vector Machine
    , openJobs :: JobSet
    , doneJobs :: JobSet
--    , currentJobs :: JobSet
    , s_jobs :: IntMap Job
    , time :: Double
    , jobSuccs :: IntMap (JobSet) -- From job to list of dependant jobs
    , states :: Vector PState
    } deriving Show

data Machine = Machine
    { m_id :: Int
    , m_open :: Double --Machine will be free again at this time. (>= this time)
    , m_assigned :: [JobAssignment]
    } deriving (Show, Eq)

data JobAssignment
    = Assignment
        { job_power :: Int
        , job_id :: Int
        , job_start :: Double
        , job_end :: Double
        }
    | Idle
        { job_start :: Double
        , job_end :: Double
        }
    deriving (Eq, Show)

data Solution = Solution
    { total_time :: Double
    , energy :: Double
    , jobSchedules :: [(MachineId, JobAssignment)]
    , jobCount :: Int
    }
    deriving (Eq, Show)

--After that, print for each job (in one line) its job id, the machine it was allocated to, the
--p-state used for that job, the start time, and the completion time. Print a newline after each
--scheduling instance.
extractSolution :: ScheduleState -> Solution
extractSolution (ScheduleState {machines = machines, time = time, s_jobs = jobs, states = p_states})
    = Solution
        { total_time = time
        , energy = energy
        , jobCount = P.length jobs
        , jobSchedules = schedules }
  where
    assignments = foldMap (\m -> L.zip (repeat (m_id m)) (m_assigned m)) machines :: [(MachineId,JobAssignment)]
    workAssignments = P.filter (not . isIdle . snd) assignments
    energy = F.foldl' (\total (_,ass) -> total + assEnergy ass) 0 assignments
    assEnergy (Idle {job_start = start, job_end = end})
        = (end - start) * (state_power $ V.head $ p_states)
    assEnergy (Assignment {job_start = start, job_end = end, job_power = job_power})
        = (end - start) * (state_power $ p_states V.! job_power)
    schedules = sortWith (job_id . snd) workAssignments

formatSolution :: Solution -> String
formatSolution Solution
        { total_time = time
        , energy = energy
        , jobCount = jobCount
        , jobSchedules = schedules } = unlines $ [show jobCount, show time, show energy] ++ L.map showJob schedules
  where
    showJob (m, Assignment {job_id = id, job_start = start, job_end = end, job_power = power}) =
        unwords $ [show id, show m, show power, show start, show end]

solveInstance ::  Inst -> ScheduleState
solveInstance (Inst jobs m_count states' deadline) =
    schedule initialState
  where
    initialState = ScheduleState { machines = machines', openJobs = openJobs', doneJobs = S.empty
                         , s_jobs = jobs', time = 0, jobSuccs = succ', states = V.fromList states' }
    openJobs' = S.fromList $ fmap PT.job_id jobs
    jobs' = M.fromList $ fmap (\j -> (PT.job_id j, j)) jobs
    succ' = getSuccsFromJobs jobs
    machines' = V.fromList $ fmap (\m_id -> Machine m_id 0 []) [0..m_count]


isIdle :: JobAssignment -> Bool
isIdle Idle {} = True
isIdle _ = False

getSuccsFromJobs :: [Job] -> IntMap (JobSet)
getSuccsFromJobs jobs =
    fromListWith (S.union) succsList
  where
    succsList :: [(Int,JobSet)]
    succsList = foldMap getSucc jobs
    getSucc (Job id _ops preds) =
        fmap (,S.singleton id) preds

getJob :: ScheduleState -> JobId -> Job
getJob state job =
    fromMaybe (error "Job Should exist") (M.lookup job (s_jobs state))

jobPreds :: ScheduleState -> JobId -> [JobId]
jobPreds state job =
    PT.preds . getJob state $ job

getSuccs :: ScheduleState -> JobId -> [JobId]
getSuccs state job =
    S.toList $
        fromMaybe S.empty $
        M.lookup job (jobSuccs state)


-- | Order two jobs, prefer the one which has more jobs
-- depending on it. If they have the same number prefer the
-- longer one.
jobOrder :: ScheduleState -> Job -> Job -> Ordering
jobOrder state (Job id1 ops1 _preds1) (Job id2 ops2 _preds2)
    | sats1 < sats2 = LT
    | sats1 > sats2 = GT
    | ops1 < ops2 = LT
    | ops1 > ops2 = GT
    | otherwise = compare id1 id2
  where
    sats1 = P.length $ getSuccs state id1
    sats2 = P.length $ getSuccs state id2

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
schedule :: ScheduleState -> ScheduleState
schedule state
    -- Finished
    | M.size (s_jobs state) == S.size (doneJobs state) =
        --trace "done" $
        fillEnd state
    --advance to next point in time
    | S.null freeMachines =
        --trace "NextPoint1" $
        schedule (moveNextPoint state)
    -- schedule idle jobs
    | P.null doableJobs =
        --trace "NextPoint2" $
        schedule (moveNextPoint state)
    | otherwise =
        schedule (scheduleJobs state scheduleList $ S.toList freeMachines)
  where
    predsSatisfied job = let jobDone j = S.member j (doneJobs state)
        in P.all (jobDone) (jobPreds state job)
    doableJobs = P.filter predsSatisfied $ S.toList $ openJobs state :: [JobId]
    nextOrder = -- pTraceShowId $
        sortBy (jobOrder state) $ fmap (getJob state) doableJobs
    freeMachines =
        foldMap
            (\m -> if machineIdle m then S.singleton (m_id m) else S.empty )
            (machines state) :: IntSet
    getLastAssignment m = listToMaybe $ P.filter (not . isIdle) (m_assigned m)
    machineIdle m
        | Just (Assignment { job_end = end_time }) <- ass
        , end_time > time state
        = False
        | otherwise = True
      where
        ass = getLastAssignment m
    scheduleList = P.take (S.size freeMachines) nextOrder :: [Job]

fillEnd :: ScheduleState -> ScheduleState
fillEnd state
  | V.all (\w -> fmap job_end w == latestEnd) lastWorks
  = state
  | otherwise = fillEnd (moveNextPoint state)
  where
    getLastAssignment m = listToMaybe $ (m_assigned m) :: Maybe JobAssignment
    --work assignments not finished yet
    lastWorks = V.map getLastAssignment (machines state) :: Vector (Maybe JobAssignment)
    latestEnd = V.maximum . V.map (fmap job_end) $ lastWorks

scheduleJobs :: ScheduleState -> [Job] -> [MachineId] -> ScheduleState
scheduleJobs state jobs machines =
    F.foldl' (\s (j,m) -> scheduleJob s j m) state (L.zip jobs machines)

scheduleJob :: ScheduleState -> Job -> MachineId -> ScheduleState
scheduleJob state job@(Job {PT.job_id = job_id}) m_id =
    state'
  where
    p_state = V.last $ states state --FULL POWER
    job_time = job_ops job / state_freq p_state
    start_time = time state
    end_time = start_time + job_time
    assignment = Assignment (state_id p_state) job_id start_time end_time
    machine@Machine{m_assigned = assignments} = machines state V.! m_id
    machine' = machine {m_assigned = assignment:assignments, m_open = end_time}
    machines' = machines state  V.// [(m_id, machine')]
    state' =
        state
        { machines = machines'
        , openJobs = S.delete job_id (openJobs state)
        }

-- Schedule an idle job on all idling machines which lasts until the next work job is done.
{- Move to the end of next finished job
* Calculate next time
* Update finished jobs
* The next time is always based on the next finished job,
  so we only have to check the job shedueld last.
-}
moveNextPoint :: ScheduleState -> ScheduleState
moveNextPoint state =
    state
        { machines = V.map addIdle (machines state)
        , time = nextEndTime
        , doneJobs = done' }
  where
    getLastAssignment m = listToMaybe $ P.filter (not . isIdle) (m_assigned m) :: Maybe JobAssignment
    --work assignments not finished yet
    currentWork = V.filter (\j -> job_end j > time state) $
                    V.map fromJust . V.filter (isJust) $
                    V.map getLastAssignment (machines state)
    nextEndJob
        | V.null currentWork = error "No next point"
        | otherwise = V.minimumBy (\a1 a2 -> compare (job_end a1) (job_end a2)) currentWork
    nextEndTime = job_end nextEndJob :: Double
    ms = machines state
    addIdle :: Machine -> Machine
    addIdle m
      | currentEnd >= nextEndTime = m --already has an Idle slot scheduled
      | otherwise = m { m_assigned = idleJob : m_assigned m}
      where
        currentEnd = fromMaybe 0 (job_end <$> listToMaybe (m_assigned m))
        idleJob = Idle {job_start = currentEnd, job_end = nextEndTime}

    doneWork = V.filter (\j -> job_end j >= nextEndTime) $
                    V.map fromJust . V.filter (isJust) $
                    V.map getLastAssignment (machines state)
    finishedJobs = V.map job_id doneWork :: Vector JobId
    done' = V.foldl' (\d j -> S.insert j d) (doneJobs state) finishedJobs









