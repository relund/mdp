library(MDP2)
clean_up <- function() unlink("*.bin")


test_that("Total reward", {
  source("files/machine_replacement_v1.R")
  mdp <- load_mdp("machine1_", get_log = FALSE)
  w <- "Net reward" # label of the weight we want to optimize
  scrapValues <- c(30, 10, 5, 0) # scrap values (the values of the 4 states at stage 4)
  run_value_ite(mdp, w, term_values = scrapValues, get_log = FALSE)
  expect_equal(get_policy(mdp, 13)$weight, 102.2)
  rm(mdp)

  # same model with a single dummy node
  source("files/machine_replacement_v2.R")
  mdp <- load_mdp("machine2_", get_log = FALSE)
  w <- "Net reward" # label of the weight we want to optimize
  run_value_ite(mdp, w, term_values = 0, get_log = FALSE)
  expect_equal(get_policy(mdp, 12)$weight, 102.2)
  rm(mdp)
})

test_that("Transition-level total reward", {
  w <- binary_mdp_writer(prefix = "trans_reward_", get_log = FALSE)
  w$set_weights(character())
  w$set_trans_weights("Transition reward")
  w$process()
  w$stage()
  w$state()
  w$action(
    weights = numeric(0),
    prob = c(1, 0, 0.25, 1, 1, 0.75),
    trans_weights = c(10, 20)
  )
  w$end_action()
  w$end_state()
  w$end_stage()
  w$stage()
  w$state()
  w$end_state()
  w$state()
  w$end_state()
  w$end_stage()
  w$end_process()
  w$close_writer()

  mdp <- load_mdp("trans_reward_", get_log = FALSE)
  expect_equal(mdp$weight_action_names, character())
  expect_equal(mdp$weight_trans_names, "Transition reward")
  expect_error(
    mdp$ptr$valueIte(0, 0, 1L, 0, 0L, 0L, c(0, 0), 0, 1),
    "Transition-level weights are not supported for BellmanOp::Discounted"
  )
  run_value_ite(mdp, "Transition reward", term_values = c(100, 200), get_log = FALSE)
  policy <- get_policy(mdp)
  expect_equal(policy$weight[policy$state_str == "0,0"], 192.5)
  rm(mdp)
})

test_that("Global weight lookup rejects ambiguous names", {
  mdp <- list(weight_names = c("Net", "Net reward"))
  expect_equal(get_w_idx(mdp, "Net"), 0)
  expect_error(get_w_idx(mdp, "e"), "ambiguous")
})

test_that("Value iteration supports minimization objective", {
  w <- binary_mdp_writer(prefix = "sense_", get_log = FALSE)
  w$set_weights("Cost")
  w$process()
  w$stage()
  w$state()
  w$action(weights = 10, prob = c(1, 0, 1), end = TRUE)
  w$end_action()
  w$action(weights = 1, prob = c(1, 0, 1), end = TRUE)
  w$end_action()
  w$end_state()
  w$end_stage()
  w$stage()
  w$state()
  w$end_state()
  w$end_stage()
  w$end_process()
  w$close_writer()

  mdp <- load_mdp("sense_", get_log = FALSE)
  run_value_ite(mdp, "Cost", term_values = 0, objective = "max", get_log = FALSE)
  policy <- get_policy(mdp)
  expect_equal(policy$a_idx[policy$state_str == "0,0"], 0)
  expect_equal(policy$weight[policy$state_str == "0,0"], 10)
  expect_equal(get_rpo(mdp, "Cost", i_a = 0, s_id = 1, objective = "max")$rpo, 9)

  run_value_ite(mdp, "Cost", term_values = 0, objective = "min", get_log = FALSE)
  policy <- get_policy(mdp)
  expect_equal(policy$a_idx[policy$state_str == "0,0"], 1)
  expect_equal(policy$weight[policy$state_str == "0,0"], 1)
  expect_equal(get_rpo(mdp, "Cost", i_a = 1, s_id = 1, objective = "min")$rpo, 9)
})

test_that("Value iteration supports minimum and maximum successor Bellman operators", {
  w <- binary_mdp_writer(prefix = "minmax_", get_log = FALSE)
  w$set_weights("Weight")
  w$set_trans_weights("Trans weight")
  w$process()
  w$stage()
  w$state()
  w$action(
    weights = 0,
    prob = c(1, 0, 0.5, 1, 1, 0.5),
    trans_weights = c(100, 0)
  )
  w$end_action()
  w$action(
    weights = 5,
    prob = c(1, 0, 1),
    trans_weights = 5
  )
  w$end_action()
  w$end_state()
  w$end_stage()
  w$stage()
  w$state()
  w$end_state()
  w$state()
  w$end_state()
  w$end_stage()
  w$end_process()
  w$close_writer()

  mdp <- load_mdp("minmax_", get_log = FALSE)

  run_value_ite(mdp, "Weight", term_values = c(1, 10), bellman_op = "min", objective = "max", get_log = FALSE)
  policy <- get_policy(mdp)
  expect_equal(policy$a_idx[policy$state_str == "0,0"], 1)
  expect_equal(policy$weight[policy$state_str == "0,0"], 6)

  run_value_ite(mdp, "Weight", term_values = c(1, 10), bellman_op = "max", objective = "max", get_log = FALSE)
  policy <- get_policy(mdp)
  expect_equal(policy$a_idx[policy$state_str == "0,0"], 0)
  expect_equal(policy$weight[policy$state_str == "0,0"], 10)

  run_value_ite(mdp, "Weight", term_values = c(1, 10), bellman_op = "min", objective = "min", get_log = FALSE)
  policy <- get_policy(mdp)
  expect_equal(policy$a_idx[policy$state_str == "0,0"], 0)
  expect_equal(policy$weight[policy$state_str == "0,0"], 1)

  run_value_ite(mdp, "Weight", term_values = c(1, 10), bellman_op = "max", objective = "min", get_log = FALSE)
  policy <- get_policy(mdp)
  expect_equal(policy$a_idx[policy$state_str == "0,0"], 1)
  expect_equal(policy$weight[policy$state_str == "0,0"], 6)

  run_value_ite(mdp, "Trans weight", term_values = c(1, 10), bellman_op = "min", objective = "max", get_log = FALSE)
  policy <- get_policy(mdp)
  expect_equal(policy$a_idx[policy$state_str == "0,0"], 0)
  expect_equal(policy$weight[policy$state_str == "0,0"], 10)

  run_value_ite(mdp, "Trans weight", term_values = c(1, 10), bellman_op = "max", objective = "max", get_log = FALSE)
  policy <- get_policy(mdp)
  expect_equal(policy$a_idx[policy$state_str == "0,0"], 0)
  expect_equal(policy$weight[policy$state_str == "0,0"], 101)
})

test_that("SecondMoment supports action-level weights", {
  w <- binary_mdp_writer(prefix = "second_moment_action_", get_log = FALSE)
  w$set_weights("Weight")
  w$process()
  w$stage()
  w$state()
  w$action(label = "high", weights = 2, prob = c(1, 0, 1), end = TRUE)
  w$end_action()
  w$action(label = "low", weights = 1, prob = c(1, 1, 1), end = TRUE)
  w$end_action()
  w$end_state()
  w$end_stage()
  w$stage()
  w$state()
  w$action(weights = 3, prob = c(1, 0, 1), end = TRUE)
  w$end_action()
  w$end_state()
  w$state()
  w$action(weights = 0, prob = c(1, 0, 1), end = TRUE)
  w$end_action()
  w$end_state()
  w$end_stage()
  w$stage()
  w$state()
  w$end_state()
  w$end_stage()
  w$end_process()
  w$close_writer()

  mdp <- load_mdp("second_moment_action_", get_log = FALSE)

  run_value_ite(mdp, "Weight", term_values = 0, get_log = FALSE)
  run_calc_weights(mdp, "Weight", criterion = "second_moment", term_values = 0)
  policy <- get_policy(mdp)
  expect_equal(policy$weight[policy$state_str == "0,0"], 25)

  run_value_ite(mdp, "Weight", term_values = 0, bellman_op = "second_moment", objective = "max", get_log = FALSE)
  policy <- get_policy(mdp)
  expect_equal(policy$a_idx[policy$state_str == "0,0"], 0)
  expect_equal(policy$weight[policy$state_str == "0,0"], 25)

  run_value_ite(mdp, "Weight", term_values = 0, bellman_op = "second_moment", objective = "min", get_log = FALSE)
  policy <- get_policy(mdp)
  expect_equal(policy$a_idx[policy$state_str == "0,0"], 1)
  expect_equal(policy$weight[policy$state_str == "0,0"], 1)
})

test_that("SecondMoment supports transition-level weights", {
  w <- binary_mdp_writer(prefix = "second_moment_transition_", get_log = FALSE)
  w$set_weights(character())
  w$set_trans_weights("Weight")
  w$process()
  w$stage()
  w$state()
  w$action(
    label = "high", weights = numeric(0), prob = c(1, 0, 1),
    trans_weights = 2, end = TRUE
  )
  w$end_action()
  w$action(
    label = "low", weights = numeric(0), prob = c(1, 1, 1),
    trans_weights = 1, end = TRUE
  )
  w$end_action()
  w$end_state()
  w$end_stage()
  w$stage()
  w$state()
  w$action(
    weights = numeric(0), prob = c(1, 0, 1),
    trans_weights = 3, end = TRUE
  )
  w$end_action()
  w$end_state()
  w$state()
  w$action(
    weights = numeric(0), prob = c(1, 0, 1),
    trans_weights = 0, end = TRUE
  )
  w$end_action()
  w$end_state()
  w$end_stage()
  w$stage()
  w$state()
  w$end_state()
  w$end_stage()
  w$end_process()
  w$close_writer()

  mdp <- load_mdp("second_moment_transition_", get_log = FALSE)

  run_value_ite(mdp, "Weight", term_values = 0, get_log = FALSE)
  run_calc_weights(mdp, "Weight", criterion = "second_moment", term_values = 0)
  policy <- get_policy(mdp)
  expect_equal(policy$weight[policy$state_str == "0,0"], 25)

  run_value_ite(mdp, "Weight", term_values = 0, bellman_op = "second_moment", objective = "max", get_log = FALSE)
  policy <- get_policy(mdp)
  expect_equal(policy$a_idx[policy$state_str == "0,0"], 0)
  expect_equal(policy$weight[policy$state_str == "0,0"], 25)

  run_value_ite(mdp, "Weight", term_values = 0, bellman_op = "second_moment", objective = "min", get_log = FALSE)
  policy <- get_policy(mdp)
  expect_equal(policy$a_idx[policy$state_str == "0,0"], 1)
  expect_equal(policy$weight[policy$state_str == "0,0"], 1)
})

test_that("SecondMoment value iteration is finite-horizon only", {
  source("files/two_level_hmdp.R")
  mdp <- load_mdp("2lev_", get_log = FALSE)
  run_value_ite(mdp, "Net reward", "Duration", bellman_op = "second_moment", term_values = rep(0, mdp$founder_states_last), get_log = FALSE)
  expect_match(mdp$ptr$getLog(), "SecondMoment value iteration is currently only supported for finite time-horizon HMDPs")
})

test_that("Variance supports action-level fixed-policy evaluation", {
  w <- binary_mdp_writer(prefix = "variance_action_", get_log = FALSE)
  w$set_weights("Weight")
  w$process()
  w$stage()
  w$state()
  w$action(weights = 1, prob = c(1, 0, 0.5, 1, 1, 0.5), end = TRUE)
  w$end_action()
  w$end_state()
  w$end_stage()
  w$stage()
  w$state()
  w$action(weights = 0, prob = c(1, 0, 1), end = TRUE)
  w$end_action()
  w$end_state()
  w$state()
  w$action(weights = 2, prob = c(1, 0, 1), end = TRUE)
  w$end_action()
  w$end_state()
  w$end_stage()
  w$stage()
  w$state()
  w$end_state()
  w$end_stage()
  w$end_process()
  w$close_writer()

  mdp <- load_mdp("variance_action_", get_log = FALSE)
  run_value_ite(mdp, "Weight", term_values = 0, get_log = FALSE)

  mdp$ptr$setTerminalW(0)
  run_calc_weights(mdp, "Weight", criterion = "expected", term_values = 0)
  expected <- get_policy(mdp)$weight

  mdp$ptr$setTerminalW(0)
  run_calc_weights(mdp, "Weight", criterion = "second_moment", term_values = 0)
  second_moment <- get_policy(mdp)$weight

  run_calc_weights(mdp, "Weight", criterion = "variance", term_values = 0)
  variance <- get_policy(mdp)$weight

  s0 <- get_policy(mdp)$state_str == "0,0"
  expect_equal(variance[s0], 1)
  expect_equal(variance, second_moment - expected^2)
})

test_that("Variance supports transition-level fixed-policy evaluation", {
  w <- binary_mdp_writer(prefix = "variance_transition_", get_log = FALSE)
  w$set_weights(character())
  w$set_trans_weights("Weight")
  w$process()
  w$stage()
  w$state()
  w$action(
    weights = numeric(0), prob = c(1, 0, 0.5, 1, 1, 0.5),
    trans_weights = c(1, 1), end = TRUE
  )
  w$end_action()
  w$end_state()
  w$end_stage()
  w$stage()
  w$state()
  w$action(
    weights = numeric(0), prob = c(1, 0, 1),
    trans_weights = 0, end = TRUE
  )
  w$end_action()
  w$end_state()
  w$state()
  w$action(
    weights = numeric(0), prob = c(1, 0, 1),
    trans_weights = 2, end = TRUE
  )
  w$end_action()
  w$end_state()
  w$end_stage()
  w$stage()
  w$state()
  w$end_state()
  w$end_stage()
  w$end_process()
  w$close_writer()

  mdp <- load_mdp("variance_transition_", get_log = FALSE)
  run_value_ite(mdp, "Weight", term_values = 0, get_log = FALSE)

  mdp$ptr$setTerminalW(0)
  run_calc_weights(mdp, "Weight", criterion = "expected", term_values = 0)
  expected <- get_policy(mdp)$weight

  mdp$ptr$setTerminalW(0)
  run_calc_weights(mdp, "Weight", criterion = "second_moment", term_values = 0)
  second_moment <- get_policy(mdp)$weight

  run_calc_weights(mdp, "Weight", criterion = "variance", term_values = 0)
  variance <- get_policy(mdp)$weight

  s0 <- get_policy(mdp)$state_str == "0,0"
  expect_equal(variance[s0], 1)
  expect_equal(variance, second_moment - expected^2)
})

test_that("Variance uses terminal values as means and terminal variance zero", {
  w <- binary_mdp_writer(prefix = "variance_terminal_", get_log = FALSE)
  w$set_weights("Weight")
  w$process()
  w$stage()
  w$state()
  w$action(weights = 0, prob = c(1, 0, 0.5, 1, 1, 0.5), end = TRUE)
  w$end_action()
  w$end_state()
  w$end_stage()
  w$stage()
  w$state()
  w$end_state()
  w$state()
  w$end_state()
  w$end_stage()
  w$end_process()
  w$close_writer()

  mdp <- load_mdp("variance_terminal_", get_log = FALSE)
  run_value_ite(mdp, "Weight", term_values = c(0, 2), get_log = FALSE)
  run_calc_weights(mdp, "Weight", criterion = "variance", term_values = c(0, 2))
  policy <- get_policy(mdp)

  expect_equal(policy$weight[policy$state_str == "0,0"], 1)
  expect_equal(policy$weight[policy$state_str == "1,0"], 0)
  expect_equal(policy$weight[policy$state_str == "1,1"], 0)
})

test_that("Variance is not a value-iteration Bellman operator", {
  w <- binary_mdp_writer(prefix = "variance_not_value_ite_", get_log = FALSE)
  w$set_weights("Weight")
  w$process()
  w$stage()
  w$state()
  w$action(weights = 1, prob = c(1, 0, 1), end = TRUE)
  w$end_action()
  w$end_state()
  w$end_stage()
  w$stage()
  w$state()
  w$end_state()
  w$end_stage()
  w$end_process()
  w$close_writer()

  mdp <- load_mdp("variance_not_value_ite_", get_log = FALSE)
  expect_error(
    run_value_ite(mdp, "Weight", term_values = 0, bellman_op = "variance", get_log = FALSE),
    "should be one of"
  )
  expect_error(
    mdp$ptr$calcRPO(8, 0, as.integer(c(0)), 0L, as.integer(c(0)), 0, 0L, 1),
    "Bellman operator not implemented"
  )
})


test_that("Long run average reward", {
  source("files/two_level_hmdp.R")
  mdp <- load_mdp("2lev_", get_log = FALSE)
  expect_lt(mdp$ptr$policyIteFixedPolicy(1L, 0L, 1L, 1), -1e+15)
  expect_match(mdp$ptr$getLog(), "valid fixed policy must be set")
  expect_equal(run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE), 5.71428571428571441259691710001789033412933349609375)
  expect_equal(run_policy_ite_ave(mdp, "Net reward", "Items", get_log = FALSE), 4)
  expect_equal(run_policy_ite_ave(mdp, "Items", "Duration", get_log = FALSE), 2.71428571428571441259691710001789033412933349609375)
  rm(mdp)

  # sprintf("%.100f",g)
  source("files/HCT_ex6.1.1.R")
  mdp <- load_mdp("hct611_", get_log = FALSE)
  expect_equal(run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE), -0.433789954337899297254210750907077454030513763427734375)
  rm(mdp)

  source("files/HCT_exc6.4.R")
  mdp <- load_mdp("hct64_", get_log = FALSE)
  expect_equal(run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE), -17.7686915887850460649133310653269290924072265625)
  rm(mdp)

  source("files/HCT_exc6.7.R")
  mdp <- load_mdp("hct67_", get_log = FALSE)
  expect_equal(run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE), -7.8132707659357780727304998436011373996734619140625)
  rm(mdp)

  source("files/HCT_exc7.3.R")
  mdp <- load_mdp("hct73_", get_log = FALSE)
  expect_equal(run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE), 4)
  rm(mdp)
})


test_that("Discounted expected reward", {
  mdp <- load_mdp("2lev_", get_log = FALSE)
  rate <- 0.1
  run_policy_ite_discount(mdp, "Net reward", "Duration", rate, get_log = FALSE)
  weights_policy_ite <- get_policy(mdp)$weight
  expect_equal(weights_policy_ite[16], 56.33119951473481279435873148031532764434814453125)
  run_value_ite(mdp, "Net reward", "Duration", rate, eps = 1e-15, max_ite = 10000, get_log = FALSE)
  weights_run_value_ite <- get_policy(mdp)$weight
  expect_equal(weights_policy_ite, weights_run_value_ite)
  rate <- 0.01
  run_policy_ite_discount(mdp, "Net reward", "Duration", rate, get_log = FALSE)
  weights_policy_ite <- get_policy(mdp)$weight
  run_value_ite(mdp, "Net reward", "Duration", rate, eps = 1e-15, max_ite = 10000, get_log = FALSE)
  weights_run_value_ite <- get_policy(mdp)$weight
  expect_equal(weights_policy_ite, weights_run_value_ite)
  rm(mdp)

  mdp <- load_mdp("hct611_", get_log = FALSE)
  rate <- 0.1
  run_policy_ite_discount(mdp, "Net reward", "Duration", rate, get_log = FALSE)
  weights_policy_ite <- get_policy(mdp)$weight
  expect_equal(weights_policy_ite[5], -12.34541222517888314769152202643454074859619140625)
  expect_equal(sum(weights_policy_ite), -36.6474287369603786146399215795099735260009765625)
  run_value_ite(mdp, "Net reward", "Duration", rate, eps = 1e-15, max_ite = 10000, get_log = FALSE)
  weights_run_value_ite <- get_policy(mdp)$weight
  expect_equal(weights_policy_ite, weights_run_value_ite)
  rate <- 0.01
  run_policy_ite_discount(mdp, "Net reward", "Duration", rate, get_log = FALSE)
  weights_policy_ite <- get_policy(mdp)$weight
  run_value_ite(mdp, "Net reward", "Duration", rate, eps = 1e-15, max_ite = 10000, get_log = FALSE)
  weights_run_value_ite <- get_policy(mdp)$weight
  expect_equal(weights_policy_ite, weights_run_value_ite)
  rm(mdp)

  mdp <- load_mdp("hct64_", get_log = FALSE)
  rate <- 0.1
  run_policy_ite_discount(mdp, "Net reward", "Duration", rate, get_log = FALSE)
  weights_policy_ite <- get_policy(mdp)$weight
  expect_equal(sum(weights_policy_ite), -920.57968416603171135648153722286224365234375)
  run_value_ite(mdp, "Net reward", "Duration", rate, eps = 1e-15, max_ite = 10000, get_log = FALSE)
  weights_run_value_ite <- get_policy(mdp)$weight
  expect_equal(weights_policy_ite, weights_run_value_ite)
  rate <- 0.01
  run_policy_ite_discount(mdp, "Net reward", "Duration", rate, get_log = FALSE)
  weights_policy_ite <- get_policy(mdp)$weight
  run_value_ite(mdp, "Net reward", "Duration", rate, eps = 1e-15, max_ite = 10000, get_log = FALSE)
  weights_run_value_ite <- get_policy(mdp)$weight
  expect_equal(weights_policy_ite, weights_run_value_ite)
  rm(mdp)

  mdp <- load_mdp("hct67_", get_log = FALSE)
  rate <- 0.1
  run_policy_ite_discount(mdp, "Net reward", "Duration", rate, get_log = FALSE)
  weights_policy_ite <- get_policy(mdp)$weight
  expect_equal(sum(weights_policy_ite), -2754.88121322102369958884082734584808349609375)
  run_value_ite(mdp, "Net reward", "Duration", rate, eps = 1e-15, max_ite = 10000, get_log = FALSE)
  weights_run_value_ite <- get_policy(mdp)$weight
  expect_equal(weights_policy_ite, weights_run_value_ite)
  rate <- 0.01
  run_policy_ite_discount(mdp, "Net reward", "Duration", rate, get_log = FALSE)
  weights_policy_ite <- get_policy(mdp)$weight
  run_value_ite(mdp, "Net reward", "Duration", rate, eps = 1e-15, max_ite = 10000, get_log = FALSE)
  weights_run_value_ite <- get_policy(mdp)$weight
  expect_equal(weights_policy_ite, weights_run_value_ite)
  rm(mdp)

  mdp <- load_mdp("hct73_", get_log = FALSE)
  rate <- 0.1
  run_policy_ite_discount(mdp, "Net reward", "Duration", rate, get_log = FALSE)
  weights_policy_ite <- get_policy(mdp)$weight
  expect_equal(sum(weights_policy_ite), 240.1591230611287528518005274236202239990234375)
  run_value_ite(mdp, "Net reward", "Duration", rate, eps = 1e-15, max_ite = 10000, get_log = FALSE)
  weights_run_value_ite <- get_policy(mdp)$weight
  expect_equal(weights_policy_ite, weights_run_value_ite)
  rate <- 0.01
  run_policy_ite_discount(mdp, "Net reward", "Duration", rate, get_log = FALSE)
  weights_policy_ite <- get_policy(mdp)$weight
  run_value_ite(mdp, "Net reward", "Duration", rate, eps = 1e-15, max_ite = 10000, get_log = FALSE)
  weights_run_value_ite <- get_policy(mdp)$weight
  expect_equal(weights_policy_ite, weights_run_value_ite)
  rm(mdp)

  # test discount factor conversion
  mdp <- load_mdp("hct73_", get_log = FALSE)
  d <- 0.9
  rate <- -log(d)
  run_policy_ite_discount(mdp, "Net reward", "Duration", rate, get_log = FALSE)
  weights_policy_ite1 <- get_policy(mdp)$weight
  run_policy_ite_discount(mdp, "Net reward", "Duration", discount_factor = d, get_log = FALSE)
  weights_policy_ite2 <- get_policy(mdp)$weight
  expect_equal(weights_policy_ite1, weights_policy_ite2)
  rm(mdp)
})


clean_up()
