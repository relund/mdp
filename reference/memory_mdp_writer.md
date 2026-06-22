# Function for building an HMDP model directly in memory.

`memory_mdp_writer()` defines the same main sub-functions as
[`binary_mdp_writer()`](http://relund.github.io/mdp/reference/binary_mdp_writer.md),
but stores states and actions directly in C++ memory instead of writing
intermediate binary files. `close_writer()` compiles the model and
returns the loaded `"HMDP"` object.

## Usage

``` r
memory_mdp_writer(
  prefix = "",
  eps = 1e-05,
  check = TRUE,
  verbose = FALSE,
  get_log = TRUE
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

- get_log:

  Output the log messages.

## Value

A list of functions. Calling `close_writer()` returns an `"HMDP"`
object.

## Details

External or included processes are not supported by
`memory_mdp_writer()`.

## Note

Note all indexes are starting from zero (C/C++ style).

## Examples

``` r
## Use temp dir
wd <- setwd(tempdir())

# Create a small HMDP with two levels
w<-memory_mdp_writer()
w$set_weights(c("Duration","Net reward","Items"))
w$process()
   w$stage()
      w$state(label="M0")
         w$action(label="A0",weights=c(0,0,0),prob=c(2,0,1))
            w$process()
               w$stage()
                  w$state(label="D")
                     w$action(label="A0",weights=c(0,0,1),prob=c(1,0,0.5,1,1,0.5))
                     w$end_action()
                  w$end_state()
               w$end_stage()
               w$stage()
                  w$state(label="C0")
                     w$action(label="A0",weights=c(0,0,0),prob=c(1,0,1))
                     w$end_action()
                     w$action(label="A1",weights=c(1,2,1),prob=c(1,0,0.5,1,1,0.5))
                     w$end_action()
                  w$end_state()
                  w$state(label="C1")
                     w$action(label="A0",weights=c(0,0,0),prob=c(1,0,1))
                     w$end_action()
                     w$action(label="A1",weights=c(1,2,1),prob=c(1,0,0.5,1,1,0.5))
                     w$end_action()
                  w$end_state()
               w$end_stage()
               w$stage()
                  w$state(label="C0")
                     w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1))
                     w$end_action()
                  w$end_state()
                  w$state(label="C1")
                     w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1))
                     w$end_action()
                  w$end_state()
               w$end_stage()
            w$end_process()
         w$end_action()
         w$action(label="A1",weights=c(0,0,0),prob=c(2,0,1))
            w$process()
               w$stage()
                  w$state(label="D")
                     w$action(label="A0",weights=c(0,0,1),prob=c(1,0,1))
                     w$end_action()
                  w$end_state()
               w$end_stage()
               w$stage()
                  w$state(label="C0")
                     w$action(label="A0",weights=c(0,0,0),prob=c(1,0,1))
                     w$end_action()
                     w$action(label="A1",weights=c(1,2,1),prob=c(1,0,0.5,1,1,0.5))
                     w$end_action()
                  w$end_state()
               w$end_stage()
               w$stage()
                  w$state(label="C0")
                     w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1))
                     w$end_action()
                  w$end_state()
                  w$state(label="C1")
                     w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1))
                     w$end_action()
                     w$action(label="A1",weights=c(0,10,5),prob=c(0,0,0.5,0,1,0.5))
                     w$end_action()
                  w$end_state()
               w$end_stage()
            w$end_process()
         w$end_action()
      w$end_state()
      w$state(label="M1")
         w$action(label="A0",weights=c(0,0,0),prob=c(2,0,1))
            w$process()
               w$stage()
                  w$state(label="D")
                     w$action(label="A0",weights=c(0,0,1),prob=c(1,0,0.5,1,1,0.5))
                     w$end_action()
                  w$end_state()
               w$end_stage()
               w$stage()
                  w$state(label="C0")
                     w$action(label="A0",weights=c(0,0,0),prob=c(1,0,1))
                     w$end_action()
                  w$end_state()
                  w$state(label="C1")
                     w$action(label="A0",weights=c(0,0,0),prob=c(1,0,1))
                     w$end_action()
                  w$end_state()
               w$end_stage()
               w$stage()
                  w$state(label="C0")
                     w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1))
                     w$end_action()
                  w$end_state()
                  w$state(label="C1")
                     w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1))
                     w$end_action()
                  w$end_state()
               w$end_stage()
            w$end_process()
         w$end_action()
      w$end_state()
   w$end_stage()
w$end_process()
w$close_writer()
#> 
#>   Statistics:
#>     states : 16 
#>     actions: 21 
#>     weights: 3 
#> 
#>   Closing memory MDP writer.
#> 
#> Build the HMDP from memory (9.9798e-05 sec.)
#> Checking MDP and found no errors (1.833e-06 sec.)
#> $bin_names
#> [1] "<memory>"
#> 
#> $time_horizon
#> [1] Inf
#> 
#> $states
#> [1] 16
#> 
#> $founder_states_last
#> [1] 2
#> 
#> $actions
#> [1] 21
#> 
#> $levels
#> [1] 2
#> 
#> $weight_names
#> [1] "Duration"   "Net reward" "Items"     
#> 
#> $weight_action_names
#> [1] "Duration"   "Net reward" "Items"     
#> 
#> $weight_trans_names
#> character(0)
#> 
#> $ptr
#> C++ object <0x557d77c12020> of class 'HMDP' <0x557d77f7bab0>
#> 
#> attr(,"class")
#> [1] "HMDP" "list"

## Info about the binary files (don't have to load the model first)
if (FALSE) {
   get_bin_info_states()
   get_bin_info_actions()
}

## reset working dir
setwd(wd)
```
