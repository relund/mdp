# Convert a HMDP model stored in binary format to a `hmp` (XML) file. The function simply parse the binary files and create `hmp` files using the [`hmpMDPWriter()`](http://relund.github.io/mdp/reference/hmpMDPWriter.md).

Convert a HMDP model stored in binary format to a `hmp` (XML) file. The
function simply parse the binary files and create `hmp` files using the
[`hmpMDPWriter()`](http://relund.github.io/mdp/reference/hmpMDPWriter.md).

## Usage

``` r
convertBinary2HMP(
  prefix = "",
  binNames = c("stateIdx.bin", "stateIdxLbl.bin", "actionIdx.bin", "actionIdxLbl.bin",
    "actionWeight.bin", "actionWeightLbl.bin", "transProb.bin"),
  out = paste0(prefix, "converted.hmp"),
  duration = 1,
  getLog = TRUE
)
```

## Arguments

- prefix:

  A character string with the prefix which will be added to the binary
  files.

- binNames:

  A character vector of length 7 giving the names of the binary files
  storing the model.

- out:

  The name of the HMP file (e.g. `r.hmp`).

- duration:

  Weight number storing the duration (NULL if none).

- getLog:

  Output log text.

## Value

NULL (invisible).

## Note

Note all indexes are starting from zero (C/C++ style).

## See also

`convertHMP2Binary`.

## Examples

``` r
## Set working dir
fDir <- system.file("models", package = "MDP2")
wd <- setwd(tempdir())
## Convert the machine example to a hmp file
prefix1 <- paste0(fDir,"/machine1_")
getBinInfoStates(prefix1)
#> # A tibble: 14 × 3
#>      sId stageStr label      
#>    <dbl> <chr>    <chr>      
#>  1     0 0,0      Dummy      
#>  2     1 1,0      good       
#>  3     2 1,1      average    
#>  4     3 2,0      good       
#>  5     4 2,1      average    
#>  6     5 2,2      not working
#>  7     6 3,0      good       
#>  8     7 3,1      average    
#>  9     8 3,2      not working
#> 10     9 3,3      replaced   
#> 11    10 4,0      good       
#> 12    11 4,1      average    
#> 13    12 4,2      not working
#> 14    13 4,3      replaced   
convertBinary2HMP(prefix1, duration = NULL, out = "machine1_converted.hmp")
#> 
#> Model saved to file: machine1_converted.hmp 
#> Converted binary files to hmp format.
#>    user  system elapsed 
#>   0.015   0.000   0.015 
# have a look at the hmp file
cat(readr::read_file("machine1_converted.hmp"))
#> <?xml version="1.0" encoding="UTF-8"?>
#> <mlhmp l="HMP file created by converting binary files" b="0.1" dsl="1" precision="1e-05" version="1.1">
#>   <i>0.1</i>
#>   <quantities l="Net reward"/>
#>   <sources>0 1</sources>
#>   <proc>
#>     <g>
#>       <s l="Dummy">
#>         <a l="buy">
#>           <q>-100</q>
#>           <p t="s">0 0.7 1 0.3</p>
#>           <d>1</d>
#>         </a>
#>       </s>
#>     </g>
#>     <g>
#>       <s l="good">
#>         <a l="mt">
#>           <q>55</q>
#>           <p t="d">0</p>
#>           <d>1</d>
#>         </a>
#>         <a l="nmt">
#>           <q>70</q>
#>           <p t="s">0 0.6 1 0.4</p>
#>           <d>1</d>
#>         </a>
#>       </s>
#>       <s l="average">
#>         <a l="mt">
#>           <q>40</q>
#>           <p t="d">0</p>
#>           <d>1</d>
#>         </a>
#>         <a l="nmt">
#>           <q>50</q>
#>           <p t="s">1 0.6 2 0.4</p>
#>           <d>1</d>
#>         </a>
#>       </s>
#>     </g>
#>     <g>
#>       <s l="good">
#>         <a l="mt">
#>           <q>55</q>
#>           <p t="d">0</p>
#>           <d>1</d>
#>         </a>
#>         <a l="nmt">
#>           <q>70</q>
#>           <p t="s">0 0.5 1 0.5</p>
#>           <d>1</d>
#>         </a>
#>       </s>
#>       <s l="average">
#>         <a l="mt">
#>           <q>40</q>
#>           <p t="d">0</p>
#>           <d>1</d>
#>         </a>
#>         <a l="nmt">
#>           <q>50</q>
#>           <p t="s">1 0.5 2 0.5</p>
#>           <d>1</d>
#>         </a>
#>       </s>
#>       <s l="not working">
#>         <a l="mt">
#>           <q>30</q>
#>           <p t="d">0</p>
#>           <d>1</d>
#>         </a>
#>         <a l="rep">
#>           <q>5</q>
#>           <p t="d">3</p>
#>           <d>1</d>
#>         </a>
#>       </s>
#>     </g>
#>     <g>
#>       <s l="good">
#>         <a l="mt">
#>           <q>55</q>
#>           <p t="d">0</p>
#>           <d>1</d>
#>         </a>
#>         <a l="nmt">
#>           <q>70</q>
#>           <p t="s">0 0.2 1 0.8</p>
#>           <d>1</d>
#>         </a>
#>       </s>
#>       <s l="average">
#>         <a l="mt">
#>           <q>40</q>
#>           <p t="d">0</p>
#>           <d>1</d>
#>         </a>
#>         <a l="nmt">
#>           <q>50</q>
#>           <p t="s">1 0.2 2 0.8</p>
#>           <d>1</d>
#>         </a>
#>       </s>
#>       <s l="not working">
#>         <a l="mt">
#>           <q>30</q>
#>           <p t="d">0</p>
#>           <d>1</d>
#>         </a>
#>         <a l="rep">
#>           <q>5</q>
#>           <p t="d">3</p>
#>           <d>1</d>
#>         </a>
#>       </s>
#>       <s l="replaced">
#>         <a l="Dummy">
#>           <q>0</q>
#>           <p t="d">3</p>
#>           <d>1</d>
#>         </a>
#>       </s>
#>     </g>
#>     <g>
#>       <s l="good"/>
#>       <s l="average"/>
#>       <s l="not working"/>
#>       <s l="replaced"/>
#>     </g>
#>   </proc>
#> </mlhmp>

## Convert the machine example hmp file to binary files
convertHMP2Binary(file = paste0(fDir,"/machine1.hmp"), prefix = "machine_cov_")
#> 
#>   Statistics:
#>     states : 14 
#>     actions: 18 
#>     weights: 1 
#> 
#>   Closing binary MDP writer.
#> 
#> Converted /home/runner/work/_temp/Library/MDP2/models/machine1.hmp to binary format.
#> 
#>    user  system elapsed 
#>   0.008   0.000   0.008 
getBinInfoStates(prefix = "machine_cov_")
#> # A tibble: 14 × 3
#>      sId stageStr label      
#>    <dbl> <chr>    <chr>      
#>  1     0 0,0      Dummy      
#>  2     1 1,0      good       
#>  3     2 1,1      average    
#>  4     3 2,0      good       
#>  5     4 2,1      average    
#>  6     5 2,2      not working
#>  7     6 3,0      good       
#>  8     7 3,1      average    
#>  9     8 3,2      not working
#> 10     9 3,3      replaced   
#> 11    10 4,0      good       
#> 12    11 4,1      average    
#> 13    12 4,2      not working
#> 14    13 4,3      replaced   
## Convert the machine example with a single dummy node to a hmp file
#convertBinary2HMP("machine2_")  # error since using scope = 3 not supported in hmp files

## Reset working dir
setwd(wd)
```
