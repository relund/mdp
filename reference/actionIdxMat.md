# Info about the actions in the HMDP model under consideration.

Info about the actions in the HMDP model under consideration.

## Usage

``` r
actionIdxMat(prefix = "", file = "actionIdx.bin")
```

## Arguments

- prefix:

  A character string with the prefix added to til file(s).

- file:

  The HMDP binary file containing the description under consideration.

## Value

A matrix with columns (`aId`, ...) where `aId` is the action row id and
`...` are alternating pairs `(scp, idx)`, one for each possible
transition where `scp` is the scope that can be 4 values: 2 - A
transition to a child process (stage zero in the child process), 1 - A
transition to next stage in the current process, 0 - A transition to the
next stage in the father process. the idx in the pair denote the index
of the state at the stage considered. Finally, if scope equals 3 then a
transition to the state with `sId = idx` is considered.
