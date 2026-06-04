# Return the index of a weight in the model. Note that index always start from zero (C++ style), i.e. the first weight, the first state at a stage etc has index 0.

Return the index of a weight in the model. Note that index always start
from zero (C++ style), i.e. the first weight, the first state at a stage
etc has index 0.

## Usage

``` r
get_w_idx(mdp, w_lbl)
```

## Arguments

- mdp:

  The MDP loaded using
  [`load_mdp()`](http://relund.github.io/mdp/reference/load_mdp.md).

- w_lbl:

  The label/string of the weight.

## Value

The index (integer).
