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

