# Perform policy iteration (average reward criterion) on the MDP.

The policy can afterwards be received using functions `getPolicy` and
`getPolicyW`.

## Usage

``` r
runPolicyIteAve(mdp, w, dur, maxIte = 100, getLog = TRUE)
```

## Arguments

- mdp:

  The MDP loaded using
  [`loadMDP()`](http://relund.github.io/mdp/reference/loadMDP.md).

- w:

  The label of the weight we optimize.

- dur:

  The label of the duration/time such that discount rates can be
  calculated.

- maxIte:

  Max number of iterations. If the model does not satisfy the unichain
  assumption the algorithm may loop.

- getLog:

  Output the log messages.

## Value

The optimal gain (g) calculated.

## See also

[`getPolicy()`](http://relund.github.io/mdp/reference/getPolicy.md).
