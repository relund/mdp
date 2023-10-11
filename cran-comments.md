## Test environments
* Mac OS (local, x86_64), R (release)
* Mac OS (latest) on GitHub Actions, R (release)
* Windows (latest) on GitHub Actions, R (release)
* Ubuntu (latest) on GitHub Actions, R (release, devel, oldrel-1)
* Windows (windows-x86_64-release) on R-hub (`devtools::check_rhub`)
* Windows on Win-builder (`devtools::check_win_release`)


## R CMD check results
R CMD check results (local)

0 errors ✔ | 0 warnings ✔ | 0 notes ✔


## Comments from last submission



## Downstream dependencies
* None


## Steps done before releasing to CRAN
```r
update.packages(ask = FALSE)
devtools::spell_check()
spelling::update_wordlist()
devtools::check(env_vars = c(NOT_CRAN = "true"))
devtools::check_rhub(platforms = c("windows-x86_64-release"), interactive = F)
devtools::check_win_release(quiet = TRUE) # win-builder
# Push files to GitHub for GitHub actions check
# Submit to CRAN
devtools::release()
```

