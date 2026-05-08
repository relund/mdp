# Generate a "random" HMDP stored in a set of binary files.

Generate a "random" HMDP stored in a set of binary files.

## Usage

``` r
randomHMDP(
  prefix = "",
  levels = 3,
  timeHorizon = c(Inf, 3, 4),
  states = c(2, 4, 5),
  actions = c(1, 2),
  childProcessPr = 0.5,
  externalProcessPr = 0,
  rewards = c(0, 100),
  durations = c(1, 10),
  rewardName = "Reward",
  durationName = "Duration"
)
```

## Arguments

- prefix:

  A character string with the prefix added to the file(s).

- levels:

  Maximum number of levels. Set `childProcessPr = 1` if want exact this
  number of levels.

- timeHorizon:

  The time horizon for each level (vector). For the founder the
  time-horizon can be Inf.

- states:

  Number of states at each stage at a given level (vector of length
  levels)

- actions:

  Min and max number of actions at a state.

- childProcessPr:

  Probability of creating a child process when define action.

- externalProcessPr:

  Probability of creating an external process given that we create a
  child process. Only works if levels\>2 and and currently does not
  generate external processes which include external processes.

- rewards:

  Min and max reward used.

- durations:

  Min and max duration used.

- rewardName:

  Weight name used for reward.

- durationName:

  Weight name used for duration.

## Value

The file prefix (character).
