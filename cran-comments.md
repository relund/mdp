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

Fix CRAN error checks

>The check problems on the Debian systems are caused by attempts to write
to the user library to which all packages get installed before checking
(and which now is remounted read-only for checking).

>Having package code which is run as part of the checks and attempts to
write to the user library violates the CRAN Policy's

  >Packages should not write in the user’s home filespace (including
  clipboards), nor anywhere else on the file system apart from the R
  session’s temporary directory (or during installation in the location
  pointed to by TMPDIR: and such usage should be cleaned up).

Fixed


## Downstream dependencies
* None


## Steps for releasing to CRAN
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

