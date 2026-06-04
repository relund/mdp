# Generate a "random" HMDP stored in a set of binary files.

Generate a "random" HMDP stored in a set of binary files.

## Usage

``` r
random_hmdp(
  prefix = "",
  levels = 3,
  time_horizon = c(Inf, 3, 4),
  states = c(2, 4, 5),
  actions = c(1, 2),
  child_process_pr = 0.5,
  external_process_pr = 0,
  rewards = c(0, 100),
  durations = c(1, 10),
  reward_name = "Reward",
  duration_name = "Duration"
)
```

## Arguments

- prefix:

  A character string with the prefix added to the file(s).

- levels:

  Maximum number of levels. Set `child_process_pr = 1` if want exact
  this number of levels.

- time_horizon:

  The time horizon for each level (vector). For the founder the
  time-horizon can be Inf.

- states:

  Number of states at each stage at a given level (vector of length
  levels)

- actions:

  Min and max number of actions at a state.

- child_process_pr:

  Probability of creating a child process when define action.

- external_process_pr:

  Probability of creating an external process given that we create a
  child process. Only works if levels\>2 and and currently does not
  generate external processes which include external processes.

- rewards:

  Min and max reward used.

- durations:

  Min and max duration used.

- reward_name:

  Weight name used for reward.

- duration_name:

  Weight name used for duration.

## Value

The file prefix (character).
