# Return the index of a weight in the model. Note that index always start from zero (C++ style), i.e. the first weight, the first state at a stage etc has index 0.

Return the index of a weight in the model. Note that index always start
from zero (C++ style), i.e. the first weight, the first state at a stage
etc has index 0.

## Usage

``` r
getWIdx(mdp, wLbl)
```

## Arguments

- mdp:

  The MDP loaded using
  [`loadMDP()`](http://relund.github.io/mdp/reference/loadMDP.md).

- wLbl:

  The label/string of the weight.

## Value

The index (integer).
