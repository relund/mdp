# Info about the actions in the HMDP model under consideration.

Info about the actions in the HMDP model under consideration.

## Usage

``` r
actionIdxDf(prefix = "", file = "actionIdx.bin", labels = "actionIdxLbl.bin")
```

## Arguments

- prefix:

  A character string with the prefix added to til file(s).

- file:

  The HMDP binary file containing the description under consideration.

- labels:

  The HMDP binary file containing the labels under consideration.

## Value

A data frame with the same columns as in `actionIdxMat` plus another
column containing the labels.
