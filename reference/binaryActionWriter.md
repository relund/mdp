# Function for writing actions of a HMDP model to binary files. The function defines sub-functions which can be used to define actions saved in a set of binary files. It is assumed that the states have been defined using `binaryMDPWriter` and that the id of the states is known (can be retrieved using e.g. `stateIdxDf`).

Binary files are efficient for storing large models. Compared to the HMP
(XML) format the binary files use less storage space and loading the
model is faster.

## Usage

``` r
binaryActionWriter(
  prefix = "",
  binNames = c("actionIdx.bin", "actionIdxLbl.bin", "actionWeight.bin",
    "actionWeightLbl.bin", "transProb.bin"),
  append = TRUE
)
```

## Arguments

- prefix:

  A character string with the prefix added to `binNames`.

- binNames:

  A character vector of length 5 giving the names of the binary files
  storing the model.

- append:

  Logical indicating whether should keep the currents actions (default -
  TRUE) defined or delete them and start over (FALSE).

## Value

A list of functions.

## Details

The functions which can be used are:

- `setWeights(labels, ...)`: Set the labels of the weights used in the
  actions. `labels` is a vector of label names, `...` are not used. The
  function must be called before starting building the model.

- `addAction(label=NULL, sIdx, weights, prob, ...)`: Add an action.
  Parameter `sIdx` is the id of the state defining the action, `weights`
  must be a vector of action weights, `prob` is a matrix `(sIdx,pr)`
  where the first column contain the id of the transition state (see the
  description of `actionIdx.bin` below - scope is assumed to the 3),
  `...` is currently not used.

- `endAction()`: Ends an action.

- `closeWriter()`: Close the writer. Must be called when the model
  description has finished.

Five binary files are created using the following format:

- `actionIdx.bin`: File of integers containing the indexes defining all
  actions in the format
  `sIdx scope idx scope idx scope idx -1 sIdx scope idx scope idx -1 sIdx scope -1 ...`.
  `sIdx` corresponds to the index/line number in `stateIdx.bin` (index
  starts from 0). Next pairs `(scope idx)` will follow indicating the
  possible transitions. Scope can be 4 values: 2 - A transition to a
  child process (stage zero in the child process), 1 - A transition to
  next stage in the current process, 0 - A transition to the next stage
  in the father process. Here `idx` in the pair denote the index of the
  state at the stage considered, e.g. if scope=1 and `idx`=2 we consider
  state number 3 at next stage in the current process. Finally, if scope
  = 3 then a transition to a state specified by it's state `sIdx` is
  given. That is, if scope=3 and `idx`=5 then we have a transition to
  the state specified at line 6 in `stateIdxLbl.bin`. This is useful
  when considering shared child processes.

- `actionIdxLbl.bin`: File of characters in the format
  `aIdx label aIdx label ...` Here `aIdx` corresponds to the index/line
  number in `actionIdx.bin` (index starts from 0). Note no delimiter is
  used.

- `actionWeight.bin`: File of doubles containing the weights of the
  actions in the format "c1 c2 c3 c1 c2 c3 ..." assuming three weights
  for each action.

- `actionWeightLbl.bin`: File of characters containing the labels of the
  weights in the format `label1 label2 label3` assuming three weights
  for each action.

- `transProb.bin`: File of doubles containing the probabilities of the
  transitions defined in actions in `actionIdx.bin`. The format is "p1
  p2 p3 -1 p1 -1 p1 p2 -1 ...". Here -1 is used to indicate that a new
  action is considered (new line).

## Note

Note all indexes are starting from zero (C/C++ style).

## Examples

``` r
## Use temp dir
wd <- setwd(tempdir())

# Create a small HMDP with two levels
w<-binaryMDPWriter()
w$setWeights(c("Duration","Net reward","Items"))
w$process()
   w$stage()
      w$state(label="M0")
         w$action(label="A0",weights=c(0,0,0),prob=c(2,0,1))
            w$process()
               w$stage()
                  w$state(label="D")
                     w$action(label="A0",weights=c(0,0,1),prob=c(1,0,0.5,1,1,0.5))
                     w$endAction()
                  w$endState()
               w$endStage()
               w$stage()
                  w$state(label="C0")
                     w$action(label="A0",weights=c(0,0,0),prob=c(1,0,1))
                     w$endAction()
                     w$action(label="A1",weights=c(1,2,1),prob=c(1,0,0.5,1,1,0.5))
                     w$endAction()
                  w$endState()
                  w$state(label="C1")
                     w$action(label="A0",weights=c(0,0,0),prob=c(1,0,1))
                     w$endAction()
                     w$action(label="A1",weights=c(1,2,1),prob=c(1,0,0.5,1,1,0.5))
                     w$endAction()
                  w$endState()
               w$endStage()
               w$stage()
                  w$state(label="C0")
                     w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1))
                     w$endAction()
                  w$endState()
                  w$state(label="C1")
                     w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1))
                     w$endAction()
                  w$endState()
               w$endStage()
            w$endProcess()
         w$endAction()
         w$action(label="A1",weights=c(0,0,0),prob=c(2,0,1))
            w$process()
               w$stage()
                  w$state(label="D")
                     w$action(label="A0",weights=c(0,0,1),prob=c(1,0,1))
                     w$endAction()
                  w$endState()
               w$endStage()
               w$stage()
                  w$state(label="C0")
                     w$action(label="A0",weights=c(0,0,0),prob=c(1,0,1))
                     w$endAction()
                     w$action(label="A1",weights=c(1,2,1),prob=c(1,0,0.5,1,1,0.5))
                     w$endAction()
                  w$endState()
               w$endStage()
               w$stage()
                  w$state(label="C0")
                     w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1))
                     w$endAction()
                  w$endState()
                  w$state(label="C1")
                     w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1))
                     w$endAction()
                     w$action(label="A1",weights=c(0,10,5),prob=c(0,0,0.5,0,1,0.5))
                     w$endAction()
                  w$endState()
               w$endStage()
            w$endProcess()
         w$endAction()
      w$endState()
      w$state(label="M1")
         w$action(label="A0",weights=c(0,0,0),prob=c(2,0,1))
            w$process()
               w$stage()
                  w$state(label="D")
                     w$action(label="A0",weights=c(0,0,1),prob=c(1,0,0.5,1,1,0.5))
                     w$endAction()
                  w$endState()
               w$endStage()
               w$stage()
                  w$state(label="C0")
                     w$action(label="A0",weights=c(0,0,0),prob=c(1,0,1))
                     w$endAction()
                  w$endState()
                  w$state(label="C1")
                     w$action(label="A0",weights=c(0,0,0),prob=c(1,0,1))
                     w$endAction()
                  w$endState()
               w$endStage()
               w$stage()
                  w$state(label="C0")
                     w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1))
                     w$endAction()
                  w$endState()
                  w$state(label="C1")
                     w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1))
                     w$endAction()
                  w$endState()
               w$endStage()
            w$endProcess()
         w$endAction()
      w$endState()
   w$endStage()
w$endProcess()
w$closeWriter()
#> 
#>   Statistics:
#>     states : 16 
#>     actions: 21 
#>     weights: 3 
#> 
#>   Closing binary MDP writer.
#> 

## Info about the binary files (don't have to load the model first)
getBinInfoStates()
#> # A tibble: 16 × 3
#>      sId stageStr  label
#>    <dbl> <chr>     <chr>
#>  1     0 0,0       M0   
#>  2     1 0,0,0,0,0 D    
#>  3     2 0,0,0,1,0 C0   
#>  4     3 0,0,0,1,1 C1   
#>  5     4 0,0,0,2,0 C0   
#>  6     5 0,0,0,2,1 C1   
#>  7     6 0,0,1,0,0 D    
#>  8     7 0,0,1,1,0 C0   
#>  9     8 0,0,1,2,0 C0   
#> 10     9 0,0,1,2,1 C1   
#> 11    10 0,1       M1   
#> 12    11 0,1,0,0,0 D    
#> 13    12 0,1,0,1,0 C0   
#> 14    13 0,1,0,1,1 C1   
#> 15    14 0,1,0,2,0 C0   
#> 16    15 0,1,0,2,1 C1   
getBinInfoActions()
#> # A tibble: 21 × 9
#>      aId   sId scope index pr      Duration `Net reward` Items label
#>    <dbl> <int> <chr> <chr> <chr>      <dbl>        <dbl> <dbl> <chr>
#>  1     0     0 2     0     1              0            0     0 A0   
#>  2     1     1 1,1   0,1   0.5,0.5        0            0     1 A0   
#>  3     2     2 1     0     1              0            0     0 A0   
#>  4     3     2 1,1   0,1   0.5,0.5        1            2     1 A1   
#>  5     4     3 1     0     1              0            0     0 A0   
#>  6     5     3 1,1   0,1   0.5,0.5        1            2     1 A1   
#>  7     6     4 0     0     1              1            4     0 A0   
#>  8     7     5 0     0     1              1            4     0 A0   
#>  9     8     0 2     0     1              0            0     0 A1   
#> 10     9     6 1     0     1              0            0     1 A0   
#> # ℹ 11 more rows

## reset working dir
setwd(wd)
```
