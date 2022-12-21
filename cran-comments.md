## Test environments
* Mac OS (local), R (release)
* Mac OS (latest) on GitHub Actions, R (release)
* Windows (latest) on GitHub Actions, R (release)
* Ubuntu (latest) on GitHub Actions, R (release, devel, oldrel-1)
* Win-builder (`devtools::check_win_release`)


## R CMD check results
R CMD check results

* checking compiled code ... NOTE
File ‘MDP2/libs/MDP2.so’:
  Found ‘__ZNSt3__14cerrE’, possibly from ‘std::cerr’ (C++)
    Object: ‘hmdp.o’
  Found ‘__ZNSt3__14coutE’, possibly from ‘std::cout’ (C++)
    Object: ‘hmdp.o’

Compiled code should not call entry points which might terminate R nor
write to stdout/stderr instead of to the console, nor use Fortran I/O
nor system RNGs.
See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.

0 errors ✔ | 0 warnings ✔ | 1 notes ✔


## Downstream dependencies
* None


## Steps for releasing to CRAN
```r
devtools::spell_check()
spelling::update_wordlist()
devtools::check(env_vars = c(NOT_CRAN = "true"))
devtools::check_rhub()
devtools::check_win_release(quiet = TRUE)
  # Push files to GitHub for GitHub actions check
devtools::release() 
```

