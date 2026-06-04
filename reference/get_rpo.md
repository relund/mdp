# Calculate the retention pay-off (RPO) or opportunity cost for some states.

The RPO is defined as the difference between the weight of the state
when using action `i_a` and the maximum weight of the node when using
another predecessor different from `i_a`.

## Usage

``` r
get_rpo(
  mdp,
  w,
  i_a,
  s_id = ifelse(mdp$time_horizon >= Inf, mdp$founder_states_last + 1,
    1):ifelse(mdp$time_horizon >= Inf, mdp$states + mdp$founder_states_last, mdp$states)
    - 1,
  criterion = "expected",
  dur = "",
  rate = 0,
  rate_base = 1,
  discount_factor = NULL,
  g = 0,
  objective = c("max", "min"),
  discount_method = "continuous",
  state_str = TRUE
)
```

## Arguments

- mdp:

  The MDP loaded using
  [`load_mdp()`](http://relund.github.io/mdp/reference/load_mdp.md).

- w:

  The label of the weight we calculate RPO for.

- i_a:

  The action index we calculate the RPO with respect to (same size as
  `s_id`).

- s_id:

  Vector of id's of the states we want to retrieve.

- criterion:

  The Bellman operator shortcut. If `expected` use expected weights, if
  `discount` use discounted expected weights, if `average` use average
  expected weights.

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

- g:

  The optimal gain (g) calculated (used if `criterion = "average"`).

- objective:

  Optimize by maximizing (`"max"`) or minimizing (`"min"`) the Bellman
  value.

- discount_method:

  Either 'continuous' or 'discrete', corresponding to discount factor
  `exp(-rate/rate_base)` or `1/(1 + rate/rate_base)`, respectively. Only
  used if `discount_factor` is `NULL`.

- state_str:

  Output the state string.

## Value

The RPO (matrix/data frame).
