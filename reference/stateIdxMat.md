# Info about the states in the HMDP model under consideration.

Info about the states in the HMDP model under consideration.

## Usage

``` r
stateIdxMat(prefix = "", file = "stateIdx.bin")
```

## Arguments

- prefix:

  A character string with the prefix added to til file(s).

- file:

  The HMDP binary file containing the description under consideration.

## Value

A matrix with columns `(sId, n0, s0, a0, ...)` where `sId` is the state
row id, `n0` the index of the stage at level 0, `s0` the index of the
state and `a0` the index of the action. If the HMDP has more than one
level columns index `(d1, s1, a1, ...)` are added.
