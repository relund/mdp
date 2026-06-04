# Info about the actions in the HMDP model under consideration.

Info about the actions in the HMDP model under consideration.

## Usage

``` r
action_info(
  prefix = "",
  file = "actionIdx.bin",
  weight_file = "actionWeight.bin",
  trans_pr_file = "transProb.bin",
  labels = "actionIdxLbl.bin"
)
```

## Arguments

- prefix:

  A character string with the prefix added to til file(s).

- file:

  The HMDP binary file containing the description under consideration.

- weight_file:

  The HMDP binary file containing the action costs.

- trans_pr_file:

  The HMDP binary file containing the transition probabilities.

- labels:

  The HMDP binary file containing the labels under consideration.

## Value

A matrix with columns from `action_idx_mat`, `actionCostMat` and
`trans_prob_mat` if labels is NULL. If labels not are NULL then a data
frame are returned with a label column too.
