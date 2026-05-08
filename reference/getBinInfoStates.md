# Info about the states in the binary files of the HMDP model under consideration.

Info about the states in the binary files of the HMDP model under
consideration.

## Usage

``` r
getBinInfoStates(
  prefix = "",
  labels = TRUE,
  stateStr = TRUE,
  fileS = "stateIdx.bin",
  labelS = "stateIdxLbl.bin"
)
```

## Arguments

- prefix:

  A character string with the prefix added to til binary files.

- labels:

  Should labels be extracted.

- stateStr:

  Should state strings be extracted. If false then add columns (n0, s0,
  a0, ...) where n0 the index of the stage at level 0, s0 the index of
  the state and a0 the index of the action. If the HMDP has more than
  one level columns index (d1, s1, a1, ...) are added.

- fileS:

  The binary file containing the description of states.

- labelS:

  The binary file containing the state labels.

## Value

A data frame with the information.

## Note

The model don't have to be loaded, i.e only read the binary files. The
state id (`sId`) will not be the same as in the loaded model!
