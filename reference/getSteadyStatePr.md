# Calculate the steady state transition probabilities for the founder process (level 0).

Assume that we consider an ergodic/irreducible time-homogeneous Markov
chain specified using a policy in the MDP.

## Usage

``` r
getSteadyStatePr(mdp, getLog = FALSE)
```

## Arguments

- mdp:

  The MDP loaded using
  [`loadMDP()`](http://relund.github.io/mdp/reference/loadMDP.md).

- getLog:

  Output log text.

## Value

A vector with steady state probabilities for all the states at the
founder level.
