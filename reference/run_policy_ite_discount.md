# Perform policy iteration using the discounted expected-weight Bellman operator on the MDP.

The policy can afterwards be received using functions `get_policy` and
`get_policy_w`.

## Usage

``` r
run_policy_ite_discount(
  mdp,
  w,
  dur,
  rate = 0,
  rate_base = 1,
  discount_factor = NULL,
  max_ite = 100,
  discount_method = "continuous",
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

- rate:

  The interest rate.

- rate_base:

  The time-horizon the rate is valid over.

- discount_factor:

  The discount rate for one time unit. If specified `rate` and
  `rate_base` are not used to calculate the discount rate.

- max_ite:

  Max number of iterations. If the model does not satisfy the unichain
  assumption the algorithm may loop.

- discount_method:

  Either 'continuous' or 'discrete', corresponding to discount factor
  `exp(-rate/rate_base)` or `1/(1 + rate/rate_base)`, respectively. Only
  used if `discount_factor` is `NULL`.

- objective:

  Optimize by maximizing (`"max"`) or minimizing (`"min"`) the Bellman
  value.

- get_log:

  Output the log messages.

## Value

Nothing.

## See also

[`get_policy()`](http://relund.github.io/mdp/reference/get_policy.md).
