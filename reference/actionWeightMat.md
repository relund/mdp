# Info about the weights of the actions in the HMDP model under consideration.

Info about the weights of the actions in the HMDP model under
consideration.

## Usage

``` r
actionWeightMat(
  prefix = "",
  file = "actionWeight.bin",
  labels = "actionWeightLbl.bin"
)
```

## Arguments

- prefix:

  A character string with the prefix added to til file(s).

- file:

  The HMDP binary file containing the description under consideration.

- labels:

  The HMDP binary file containing the labels under consideration.

## Value

A matrix with columns (`aId`, ...) where `aId` is the action row id and
... are the weights of the action.
