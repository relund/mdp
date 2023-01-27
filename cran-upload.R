## Check package
update.packages(ask = FALSE)
devtools::spell_check()
spelling::update_wordlist()
devtools::check(env_vars = c(NOT_CRAN = "true"))
devtools::check_rhub(interactive = F)
devtools::check_win_release(quiet = TRUE) # win-builder
# Push files to GitHub for GitHub actions check
# Submit to CRAN
devtools::release()