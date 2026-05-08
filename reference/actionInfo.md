# Info about the actions in the HMDP model under consideration.

Info about the actions in the HMDP model under consideration.

## Usage

``` r
actionInfo(
  prefix = "",
  file = "actionIdx.bin",
  weightFile = "actionWeight.bin",
  transPrFile = "transProb.bin",
  labels = "actionIdxLbl.bin"
)
```

## Arguments

- prefix:

  A character string with the prefix added to til file(s).

- file:

  The HMDP binary file containing the description under consideration.

- weightFile:

  The HMDP binary file containing the action costs.

- transPrFile:

  The HMDP binary file containing the transition probabilities.

- labels:

  The HMDP binary file containing the labels under consideration.

## Value

A matrix with columns from `actionIdxMat`, `actionCostMat` and
`transProbMat` if labels is NULL. If labels not are NULL then a data
frame are returned with a label column too.
