# Perform policy iteration using the average expected-weight Bellman operator on the MDP.

The policy can afterwards be received using functions `get_policy` and
`get_policy_w`.

## Usage

``` r
run_policy_ite_ave(
  mdp,
  w,
  dur,
  max_ite = 100,
  objective = c("max", "min"),
  get_log = TRUE
)
```

## Arguments

- mdp:

  The MDP loaded using
  [`load_mdp()`](http://relund.github.io/mdp/reference/load_mdp.md).

- w:

  The label of the weight we optimize.

- dur:

  The label of the duration/time such that discount rates can be
  calculated.

- max_ite:

  Max number of iterations. If the model does not satisfy the unichain
  assumption the algorithm may loop.

- objective:

  Optimize by maximizing (`"max"`) or minimizing (`"min"`) the Bellman
  value.

- get_log:

  Output the log messages.

## Value

The optimal gain (g) calculated.

## See also

[`get_policy()`](http://relund.github.io/mdp/reference/get_policy.md).
