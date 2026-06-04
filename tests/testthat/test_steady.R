library(MDP2)
clean_up <- function() unlink("*.bin")

test_that("steadyStatePr", {
  # sprintf("%.100f",g)
  source("files/HCT_ex6.1.1.R")
  mdp <- load_mdp("hct611_", get_log = FALSE)
  run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE)
  pr <- get_steady_state_pr(mdp)
  expect_equal(pr, c(0.56603774, 0.28301887, 0.09433962, 0.02358491, 0.03301887), tolerance = 1.5e-7)
})

clean_up()
