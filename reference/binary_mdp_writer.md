# Function for writing an HMDP model to binary files. The function defines sub-functions which can be used to define an HMDP model saved in a set of binary files.

Binary files are efficient for storing large models. Compared to the HMP
(XML) format the binary files use less storage space and loads the model
faster.

## Usage

``` r
binary_mdp_writer(
  prefix = "",
  bin_names = c("stateIdx.bin", "stateIdxLbl.bin", "actionIdx.bin", "actionIdxLbl.bin",
    "actionWeight.bin", "actionWeightLbl.bin", "transProb.bin", "externalProcesses.bin",
    "transWeight.bin", "transWeightLbl.bin"),
  get_log = TRUE
)
```

## Arguments

- prefix:

  A character string with the prefix added to `bin_names`.

- bin_names:

  A character vector giving the names of the binary files storing the
  model.

- get_log:

  Output log text.

## Value

A list of functions.

## Details

The returned writer exposes these functions:

- `set_weights(labels, ...)`: sets the labels of the weights used in the
  actions. `labels` is a vector of label names. `...` is currently
  ignored. Call this before building the model.

- `process()`: starts a (sub)process. It may also be used to specify a
  traditional MDP using matrices in `MDPtoolbox` style. In that style,
  `p` is a list of matrices, one per action, each of size `$S x S$`
  where `$S$` is the number of states. Each used row must sum to one, or
  all entries in a row must be zero if unused. `r` is a matrix of size
  `$S x A$`, where `$A$` is the number of actions, and `d` is a matrix
  of size `$S x A$` with durations. If `d` is omitted, all durations are
  assumed to be 1.

- `end_process()`: ends a (sub)process.

- `stage(label = NULL)`: starts a stage. `label` is currently unused in
  the binary format.

- `end_stage()`: ends a stage.

- `state(label = NULL)`: starts a state and returns, invisibly, the
  state id. That id can later be referenced with scope 3.

- `end_state()`: ends a state.

- `action(scope = NULL, id = NULL, pr = NULL, prob = NULL, weights, trans_weights = NULL, label = NULL, end = FALSE, ...)`:
  starts an action. `weights` must be a vector of action weights.
  `trans_weights` must contain transition weights ordered by transition,
  with all transition weight labels for the first transition followed by
  all labels for the second transition, and so on. Transition
  probabilities can be entered in two ways:

  1.  `prob` contains triples `(scope, id, pr)`.

  2.  `id` and `pr` are vectors of equal length. If `scope` is omitted,
      all scopes default to 1.

  See the description of `actionIdx.bin` below. If `end = TRUE`, calling
  `end_action()` is not necessary. `...` is currently ignored.

- `end_action()`: ends an action. Do not use this if `end = TRUE` was
  used when the action was specified.

- `include_process(prefix, label = NULL, weights, prob, term_states, trans_weights = NULL)`:
  includes an external process. External processes are loaded into
  memory only when needed, which helps with large models. `prefix` is
  the external process prefix. `weights` must be a vector of action
  weights, and `prob` must contain triples `(scope, idx, pr)`; see the
  description of `actionIdx.bin` below. `term_states` must specify the
  number of states at the last stage in the external process. Inside an
  `include_process ... end_include_process` block, you must specify the
  father jump actions of the last stage in the external process. The
  external process is represented by its first and last stage together
  with its jump actions. The function returns, invisibly, the state ids
  of the first stage in the external process, which can later be
  referenced with scope 3.

- `end_include_process()`: ends an `include_process` block.

- `close_writer()`: closes the writer. Call this when the model
  description is finished.

Ten binary files are created:

- `stateIdx.bin`: integers defining all states in the format
  `"n0 s0 -1 n0 s0 a0 n1 s1 -1 n0 s0 a0 n1 s1 a1 n2 s2 -1 n0 s0 ..."`.
  Here `-1` indicates that a new state is considered.

- `stateIdxLbl.bin`: character data in the format
  `s_idx label s_idx label ...`. Here `s_idx` corresponds to the index
  or line number in `stateIdxLbl.bin`, starting from 0. No delimiter is
  used.

- `actionIdx.bin`: integers defining all actions in the format
  `s_idx scope idx scope idx scope idx -1 s_idx scope idx scope idx -1 s_idx scope -1 ...`.
  `s_idx` corresponds to the index or line number in `stateIdx.bin`,
  starting from 0. The following `(scope, idx)` pairs indicate possible
  transitions. Scope can take four values:

  - `2`: a transition to a child process, at stage zero in the child
    process.

  - `1`: a transition to the next stage in the current process.

  - `0`: a transition to the next stage in the father process.

  - `3`: a transition to a state specified by its state `s_idx`.

  For example, if `scope = 1` and `idx = 2`, the transition is to state
  number 3 at the next stage in the current process. If `scope = 3` and
  `idx = 5`, the transition is to the state specified at line 6 in
  `stateIdxLbl.bin`. This is useful when considering shared child
  processes.

- `actionIdxLbl.bin`: character data in the format
  `a_idx label a_idx label ...`. Here `a_idx` corresponds to the index
  or line number in `actionIdx.bin`, starting from 0. No delimiter is
  used.

- `actionWeight.bin`: doubles containing action weights in the format
  `"c1 c2 c3 c1 c2 c3 ..."`, assuming three weights for each action.

- `actionWeightLbl.bin`: character data containing the weight labels in
  the format `label1 label2 label3`, assuming three weights for each
  action.

- `transProb.bin`: doubles containing transition probabilities defined
  in `actionIdx.bin`. The format is `"p1 p2 p3 -1 p1 -1 p1 p2 -1 ..."`.
  Here `-1` indicates that a new action is considered.

- `externalProcesses.bin`: character data containing links to external
  processes in the format `stage_str prefix stage_str prefix ...`. Here
  `stage_str` corresponds to the stage index, for example `n0 s0 a0 n1`,
  of the stage corresponding to the first stage in the external process,
  and `prefix` is the external process prefix. No delimiter is used.

- `transWeight.bin`: doubles containing transition weights in the format
  `"t11 t12 t21 t22 -1 ..."`, assuming two transition weights for each
  transition and two transitions in the first action.

- `transWeightLbl.bin`: character data containing the transition weight
  labels.

## Note

Note all indexes are starting from zero (C/C++ style).

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
