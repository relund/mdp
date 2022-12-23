devtools::spell_check()
spelling::update_wordlist()
devtools::check(env_vars = c(NOT_CRAN = "true"))
devtools::check_win_release(quiet = TRUE)  # win-builder
devtools::check_rhub(interactive = F)
# Push files to GitHub for GitHub actions check
devtools::release() 
