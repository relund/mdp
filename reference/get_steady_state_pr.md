# Calculate the steady state transition probabilities for the founder process (level 0).

Assume that we consider an ergodic/irreducible time-homogeneous Markov
chain specified using a policy in the MDP.

## Usage

``` r
get_steady_state_pr(mdp, get_log = FALSE)
```

## Arguments

- mdp:

  The MDP loaded using
  [`load_mdp()`](http://relund.github.io/mdp/reference/load_mdp.md).

- get_log:

  Output log text.

## Value

A vector with steady state probabilities for all the states at the
founder level.
