# Perform policy iteration (discounted reward criterion) on the MDP.

The policy can afterwards be received using functions `getPolicy` and
`getPolicyW`.

## Usage

``` r
runPolicyIteDiscount(
  mdp,
  w,
  dur,
  rate = 0,
  rateBase = 1,
  discountFactor = NULL,
  maxIte = 100,
  discountMethod = "continuous",
  getLog = TRUE
)
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

- rate:

  The interest rate.

- rateBase:

  The time-horizon the rate is valid over.

- discountFactor:

  The discount rate for one time unit. If specified `rate` and
  `rateBase` are not used to calculate the discount rate.

- maxIte:

  Max number of iterations. If the model does not satisfy the unichain
  assumption the algorithm may loop.

- discountMethod:

  Either 'continuous' or 'discrete', corresponding to discount factor
  `exp(-rate/rateBase)` or `1/(1 + rate/rateBase)`, respectively. Only
  used if `discountFactor` is `NULL`.

- getLog:

  Output the log messages.

## Value

Nothing.

## See also

[`getPolicy()`](http://relund.github.io/mdp/reference/getPolicy.md).
