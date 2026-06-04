# Info about the actions in the HMDP model under consideration.

Info about the actions in the HMDP model under consideration.

## Usage

``` r
get_bin_info_actions(
  prefix = "",
  labels = TRUE,
  file_a = "actionIdx.bin",
  file_pr = "transProb.bin",
  file_w = "actionWeight.bin",
  file_label_w = "actionWeightLbl.bin",
  file_label_a = "actionIdxLbl.bin"
)
```

## Arguments

- prefix:

  A character string with the prefix added to til binary files.

- labels:

  Should labels be extracted.

- file_a:

  The binary file containing the description of actions.

- file_pr:

  The binary file containing the description of transition
  probabilities.

- file_w:

  The binary file containing the description of weights.

- file_label_w:

  The binary file containing the weight labels.

- file_label_a:

  The binary file containing the action labels.

## Value

A data frame with the information. Scope string contain the scope of the
transitions and can be 4 values:

- 0: A transition to the next stage in the father process,

- 1: A transition to next stage in the current process,

- 2: A transition to a child process (stage zero in the child process),

- 3: A transition to the state with `s_id = idx` is considered.

The index string denote the index (id is scope = 3) of the state at the
next stage.

## Note

The model don't have to be loaded, i.e only read the binary files. The
state id (`s_id`) will not be the same as in the loaded model!

## Examples

``` r
## Use temp dir
wd <- setwd(tempdir())

# Create a small HMDP with two levels
w<-binary_mdp_writer()
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
#>   Closing binary MDP writer.
#> 

## Info about the binary files (don't have to load the model first)
get_bin_info_states()
#> # A tibble: 16 × 3
#>     s_id stage_str label
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
get_bin_info_actions()
#> # A tibble: 21 × 9
#>      aId  s_id scope index pr      Duration `Net reward` Items label
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
