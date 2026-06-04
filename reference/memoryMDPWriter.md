# Function for building an HMDP model directly in memory.

`memoryMDPWriter()` defines the same main sub-functions as
[`binaryMDPWriter()`](http://relund.github.io/mdp/reference/binaryMDPWriter.md),
but stores states and actions directly in C++ memory instead of writing
intermediate binary files. `closeWriter()` compiles the model and
returns the loaded `"HMDP"` object.

## Usage

``` r
memoryMDPWriter(
  prefix = "",
  eps = 1e-05,
  check = TRUE,
  verbose = FALSE,
  getLog = TRUE
)
```

## Arguments

- prefix:

  A character string kept for compatibility and stored in the returned
  object metadata.

- eps:

  The sum of transition probabilities must at most differ `eps` from one
  when `check = TRUE`.

- check:

  Check if the MDP seems correct before returning it.

- verbose:

  More output when compiling and running algorithms.

- getLog:

  Output the log messages.

## Value

A list of functions. Calling `closeWriter()` returns an `"HMDP"` object.

## Details

External or included processes are not supported by `memoryMDPWriter()`.

## Note

Note all indexes are starting from zero (C/C++ style).

## Examples

``` r
## Use temp dir
wd <- setwd(tempdir())

# Create a small HMDP with two levels
w<-memoryMDPWriter()
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
#>   Closing memory MDP writer.
#> 
#> Build the HMDP from memory (0.00010196 sec.)
#> Checking MDP and found no errors (1.073e-06 sec.)
#> $binNames
#> [1] "<memory>"
#> 
#> $timeHorizon
#> [1] Inf
#> 
#> $states
#> [1] 16
#> 
#> $founderStatesLast
#> [1] 2
#> 
#> $actions
#> [1] 21
#> 
#> $levels
#> [1] 2
#> 
#> $weightNames
#> [1] "Duration"   "Net reward" "Items"     
#> 
#> $weightActionNames
#> [1] "Duration"   "Net reward" "Items"     
#> 
#> $weightTransNames
#> character(0)
#> 
#> $ptr
#> C++ object <0x559e262e1b50> of class 'HMDP' <0x559e23190c70>
#> 
#> attr(,"class")
#> [1] "HMDP" "list"

## Info about the binary files (don't have to load the model first)
if (FALSE) {
   getBinInfoStates()
   getBinInfoActions()
}

## reset working dir
setwd(wd)
```
