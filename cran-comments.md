## Test environments
* Mac OS (local), R (release)
* Mac OS (latest) on GitHub Actions, R (release)
* Windows (latest) on GitHub Actions, R (release)
* Ubuntu (latest) on GitHub Actions, R (release, devel, oldrel-1)
* R-hub (`devtools::check_rhub`)
* Win-builder (`devtools::check_win_release`)


## R CMD check results
R CMD check results (local)

0 errors ✔ | 0 warnings ✔ | 0 notes ✔


## Comments from last submission

> Please omit the redundant "in R" at the end of your title.

Done

> Please do not modify the global environment (e.g. by using <<-) in your
functions. This is not allowed by the CRAN policies. -> R/binary.R;
R/convert.R ; R/hmpMDPWriter.R

The global environment is not modified. <<- is only used inside 
sub-functions of a function, i.e. like an RC6 class. This is a feature
used for the functions in R/binary.R; R/convert.R; R/hmpMDPWriter.R

> Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)
Missing Rd-tags:
      plotHypergraph.Rd: \value
      randomHMDP.Rd: \value
      
Done

> Some code lines in examples are commented out. Please never do that.
Ideally find toy examples that can be regularly executed and checked.
Lengthy examples (> 5 sec), can be wrapped in \donttest{}.
Examples in comments in:
       getPolicy.Rd
       runCalcWeights.Rd
       runValueIte.Rd
       setPolicy.Rd
       

       

> Please fix and resubmit.



## Downstream dependencies
* None


## Steps for releasing to CRAN
```r
devtools::spell_check()
spelling::update_wordlist()
devtools::check(env_vars = c(NOT_CRAN = "true"))
# devtools::check_rhub()
devtools::check_win_release(quiet = TRUE)  # win-builder
# Push files to GitHub for GitHub actions check
devtools::release() 
```

