## Test environments
* Mac OS (local), R (release)
* Mac OS (latest) on GitHub Actions, R (release)
* Windows (latest) on GitHub Actions, R (release)
* Ubuntu (latest) on GitHub Actions, R (release, devel, oldrel-1)
* Windows (windows-x86_64-release) on R-hub (`devtools::check_rhub`)
* Windows on Win-builder (`devtools::check_win_release`)


## R CMD check results
R CMD check results (local)

0 errors ✔ | 0 warnings ✔ | 0 notes ✔


## Comments from last submission

> Please always make sure to reset to user's options(), working directory
or par() after you changed it in examples and vignettes and demos.
e.g.:
oldpar <- par(mfrow = c(1,2))
...
par(oldpar)
-> similar for options() and setwd()

Done

> Please do not modify the global environment (e.g. by using <<-) in your
functions. This is not allowed by the CRAN policies. 

As stated last time, Tthe global environment is not modified. <<- is only used inside 
sub-functions of a function, i.e. like an RC6 class. This is a feature
used for the functions in R/binary.R; R/convert.R; R/hmpMDPWriter.R

> Please fix and resubmit.



## Downstream dependencies
* None


## Steps for releasing to CRAN
```r
update.packages(ask = FALSE)
devtools::spell_check()
spelling::update_wordlist()
devtools::check(env_vars = c(NOT_CRAN = "true"))
devtools::check_rhub(interactive = F)
devtools::check_win_release(quiet = TRUE) # win-builder
# Push files to GitHub for GitHub actions check
# Submit to CRAN
devtools::release()
```

