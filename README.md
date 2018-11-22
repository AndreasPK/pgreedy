# pgreedy

A greedy haskell implementation.

# Installation

## Using Docker

docker build -t dKlebinger .

docker run --rm -it --entrypoint /bin/sh dKlebinger

`cabal new-run exe:pgreedy -- input/sample.txt`

## Using Stack directly

* Install the build tool stack
`curl -sSL https://get.haskellstack.org/ | sh`

* Build the program, this will also set up the Haskell compiler
`stack build`

* To run the program then use:
`stack exec pgreedy .\input\sample.txt`

This will output any found solution to stdout.

## Using apt-get for Debian/Ubunto (untested)

* Depending on the distro there might be packages of the
  name haskell-stack to install the build tool:

`apt-get install haskell-stack`

* Then build and run as above

# Algorithm description

This implementation tries different heuristics and picks the best solution found.

## Basic approach:

In an iterative fashion we do the following:
* Check if we can shedule a job at the current starting time.
    * If so shedule a job at a P state to a machine.
      PState and Job are determined by an Heuristic
    * If not increase the starting time to the next time any job finishes.
    * Possibly change which heuristic is used.
* Estimate the best possible time/energy for this branch.
* If guaranteed to be worse than an existing solution stop this
  search path.
* If all jobs have been scheduled either dismiss the result or update the
  lower energy bounds.
* If search time has run out stop the search on this branch.
* Repeat

The algorithm will ALWAYS shedule a job if possible. This means if an optimal solution
depends on intermediate idling such a solution will never be found.

## Heuristics

A number of different heuristics are used, and the used heuristic can change
during execution.

### Heuristics used

* Longest path of depending jobs.
  * Ties by highest number of depending jobs.
* Lowest job id first
* Highest number of depending jobs.
  * With ties broken in favour of longer jobs.
* Highest total work depending directly on the job.
* Least amount of work first.
* Highest number of depending jobs, ignoring the first job.

### Heuristic selection

Decision points are points after which roughly `remainingJobs/2`
jobs have been placed. This way the size if the search space is linear
in the number of Jobs.
At each decision point we branch with each branch using a different Heuristic.
Heuristics are tried in order, as are PStates.

Given that we can expect the lowest PState to give the highest efficiency we
try lower PStates first. As we try higher pstates energy consumption increases.
But this is required for hard to hold deadlines.

