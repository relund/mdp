# Calculate the retention pay-off (RPO) or opportunity cost for some states.

The RPO is defined as the difference between the weight of the state
when using action `iA` and the maximum weight of the node when using
another predecessor different from `iA`.

## Usage

``` r
getRPO(
  mdp,
  w,
  iA,
  sId = ifelse(mdp$timeHorizon >= Inf, mdp$founderStatesLast + 1,
    1):ifelse(mdp$timeHorizon >= Inf, mdp$states + mdp$founderStatesLast, mdp$states) - 1,
  criterion = "expected",
  dur = "",
  rate = 0,
  rateBase = 1,
  discountFactor = NULL,
  g = 0,
  discountMethod = "continuous",
  stateStr = TRUE
)
```

## Arguments

- mdp:

  The MDP loaded using
  [`loadMDP()`](http://relund.github.io/mdp/reference/loadMDP.md).

- w:

  The label of the weight/reward we calculate RPO for.

- iA:

  The action index we calculate the RPO with respect to (same size as
  `sId`).

- sId:

  Vector of id's of the states we want to retrieve.

- criterion:

  The criterion used. If `expected` used expected reward, if `discount`
  used discounted rewards, if `average` use average rewards.

- dur:

  The label of the duration/time such that discount rates can be
  calculated.

- rate:

  The interest rate.

- rateBase:

  The time-horizon the rate is valid over.

- discountFactor:

  The discount rate for one time unit. If specified `rate` and
  `rateBase` are not used to calculate the discount rate.

- g:

  The optimal gain (g) calculated (used if `criterion = "average"`).

- discountMethod:

  Either 'continuous' or 'discrete', corresponding to discount factor
  `exp(-rate/rateBase)` or `1/(1 + rate/rateBase)`, respectively. Only
  used if `discountFactor` is `NULL`.

- stateStr:

  Output the state string.

## Value

The RPO (matrix/data frame).
