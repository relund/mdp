# Function for writing an HMDP model to a hmp file (XML). The function define sub-functions which can be used to define an HMDP model stored in a hmp file.

HMP files are in XML format and human readable using e.g. a text editor.
HMP files are not suitable for storing large HMDP models since text
files are very verbose. Moreover, approximation of the weights and
probabilities may occur since the parser writing the hmp file may no
output all digits. If you consider large models then use the binary file
format instead.

## Usage

``` r
hmpMDPWriter(
  file = "r.hmp",
  rate = 0.1,
  rateBase = 1,
  precision = 1e-05,
  desc = "HMP file created using hmpMDPWriter in R",
  getLog = TRUE
)
```

## Arguments

- file:

  The name of the file storing the model (e.g. `r.hmp`).

- rate:

  The interest rate (used if consider discounting).

- rateBase:

  The time where the `rate` is taken over, e.g. if the `rate` is 0.1 and
  `rateBase` is 365 days then we have an interest rate of 10 percent
  over the year.

- precision:

  The precision used when checking if probabilities sum to one.

- desc:

  Description of the model.

- getLog:

  Output log text.

## Value

A list of functions.

## Details

The returned writer exposes these functions:

- `setWeights(labels, duration)`: sets the labels of the weights used in
  the actions. `labels` is a vector of label names. `duration`
  identifies which label corresponds to duration or time. For example,
  if the first entry in `labels` is time, then `duration = 1`. Call this
  before building the model.

- `process()`: starts a (sub)process.

- `endProcess()`: ends a (sub)process.

- `stage(label = NULL)`: starts a stage.

- `endStage()`: ends a stage.

- `state(label = NULL)`: starts a state and returns the state index
  `sIdx`.

- `endState()`: ends a state.

- `action(label = NULL, weights, prob, statesNext = NULL)`: starts an
  action. `weights` must be a vector of action weights, and `prob` must
  contain triples `(scope, idx, pr)`. `scope` can take three values:

  - `0`: a transition to the next stage in the father process.

  - `1`: a transition to the next stage in the current process.

  - `2`: a transition to a child process, at stage zero in the child
    process.

  The `idx` value denotes the index of the state at the stage
  considered. For example, if `scope = 1` and `idx = 2`, the transition
  is to state number 3 at the next stage in the current process,
  counting from zero. `scope = 3` is not supported in the `hmp` file
  format. `statesNext` is the number of states in the next stage of the
  process and is only needed when there is a transition to the father.

- `endAction()`: ends an action.

- `closeWriter()`: closes the writer. Call this when the model
  description is finished.

## Note

Note all indexes are starting from zero (C/C++ style).

## Examples

``` r
## Use temp dir
wd <- setwd(tempdir())

## Create a small HMDP with two levels
w<-hmpMDPWriter()
w$setWeights(c("Duration","Net reward","Items"), duration=1)
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
           w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1), statesNext=0)
           w$endAction()
         w$endState()
         w$state(label="C1")
           w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1), statesNext=0)
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
           w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1), statesNext=0)
           w$endAction()
         w$endState()
         w$state(label="C1")
           w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1), statesNext=0)
           w$endAction()
           w$action(label="A1",weights=c(0,10,5),prob=c(0,0,0.5,0,1,0.5), statesNext=0)
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
           w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1), statesNext=0)
           w$endAction()
         w$endState()
         w$state(label="C1")
           w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1), statesNext=0)
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
#> Model saved to file: r.hmp 

## Have a look at the hmp file
cat(readr::read_file("r.hmp"))
#> <?xml version="1.0" encoding="UTF-8"?>
#> <mlhmp l="HMP file created using hmpMDPWriter in R" b="0.1" dsl="1" precision="1e-05" version="1.1">
#>   <i>0.1</i>
#>   <quantities l="Net reward"/>
#>   <quantities l="Items"/>
#>   <sources>0 1</sources>
#>   <proc>
#>     <g>
#>       <s l="M0">
#>         <a l="A0">
#>           <proc>
#>             <g>
#>               <s l="D">
#>                 <a l="A0">
#>                   <q>0 1</q>
#>                   <p t="s">0 0.5 1 0.5</p>
#>                   <d>0</d>
#>                 </a>
#>               </s>
#>             </g>
#>             <g>
#>               <s l="C0">
#>                 <a l="A0">
#>                   <q>0 0</q>
#>                   <p t="d">0</p>
#>                   <d>0</d>
#>                 </a>
#>                 <a l="A1">
#>                   <q>2 1</q>
#>                   <p t="s">0 0.5 1 0.5</p>
#>                   <d>1</d>
#>                 </a>
#>               </s>
#>               <s l="C1">
#>                 <a l="A0">
#>                   <q>0 0</q>
#>                   <p t="d">0</p>
#>                   <d>0</d>
#>                 </a>
#>                 <a l="A1">
#>                   <q>2 1</q>
#>                   <p t="s">0 0.5 1 0.5</p>
#>                   <d>1</d>
#>                 </a>
#>               </s>
#>             </g>
#>             <g>
#>               <s l="C0">
#>                 <a l="A0">
#>                   <q>4 0</q>
#>                   <p t="d">0</p>
#>                   <d>1</d>
#>                 </a>
#>               </s>
#>               <s l="C1">
#>                 <a l="A0">
#>                   <q>4 0</q>
#>                   <p t="d">0</p>
#>                   <d>1</d>
#>                 </a>
#>               </s>
#>             </g>
#>           </proc>
#>         </a>
#>         <a l="A1">
#>           <proc>
#>             <g>
#>               <s l="D">
#>                 <a l="A0">
#>                   <q>0 1</q>
#>                   <p t="d">0</p>
#>                   <d>0</d>
#>                 </a>
#>               </s>
#>             </g>
#>             <g>
#>               <s l="C0">
#>                 <a l="A0">
#>                   <q>0 0</q>
#>                   <p t="d">0</p>
#>                   <d>0</d>
#>                 </a>
#>                 <a l="A1">
#>                   <q>2 1</q>
#>                   <p t="s">0 0.5 1 0.5</p>
#>                   <d>1</d>
#>                 </a>
#>               </s>
#>             </g>
#>             <g>
#>               <s l="C0">
#>                 <a l="A0">
#>                   <q>4 0</q>
#>                   <p t="d">0</p>
#>                   <d>1</d>
#>                 </a>
#>               </s>
#>               <s l="C1">
#>                 <a l="A0">
#>                   <q>4 0</q>
#>                   <p t="d">0</p>
#>                   <d>1</d>
#>                 </a>
#>                 <a l="A1">
#>                   <q>10 5</q>
#>                   <p t="s">0 0.5 1 0.5</p>
#>                   <d>0</d>
#>                 </a>
#>               </s>
#>             </g>
#>           </proc>
#>         </a>
#>       </s>
#>       <s l="M1">
#>         <a l="A0">
#>           <proc>
#>             <g>
#>               <s l="D">
#>                 <a l="A0">
#>                   <q>0 1</q>
#>                   <p t="s">0 0.5 1 0.5</p>
#>                   <d>0</d>
#>                 </a>
#>               </s>
#>             </g>
#>             <g>
#>               <s l="C0">
#>                 <a l="A0">
#>                   <q>0 0</q>
#>                   <p t="d">0</p>
#>                   <d>0</d>
#>                 </a>
#>               </s>
#>               <s l="C1">
#>                 <a l="A0">
#>                   <q>0 0</q>
#>                   <p t="d">0</p>
#>                   <d>0</d>
#>                 </a>
#>               </s>
#>             </g>
#>             <g>
#>               <s l="C0">
#>                 <a l="A0">
#>                   <q>4 0</q>
#>                   <p t="d">0</p>
#>                   <d>1</d>
#>                 </a>
#>               </s>
#>               <s l="C1">
#>                 <a l="A0">
#>                   <q>4 0</q>
#>                   <p t="d">0</p>
#>                   <d>1</d>
#>                 </a>
#>               </s>
#>             </g>
#>           </proc>
#>         </a>
#>       </s>
#>     </g>
#>   </proc>
#> </mlhmp>

## Reset working dir
setwd(wd)
```
