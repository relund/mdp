# Info about the states in the HMDP model under consideration.

Info about the states in the HMDP model under consideration.

## Usage

``` r
stateIdxDf(prefix = "", file = "stateIdx.bin", labels = "stateIdxLbl.bin")
```

## Arguments

- prefix:

  A character string with the prefix added to the file(s).

- file:

  The HMDP binary file containing the description under consideration.

- labels:

  The HMDP binary file containing the labels under consideration.

## Value

A data frame with the same columns as in `stateIdxMat` plus another
column containing the labels.
