build_action_weight_mdp <- function() {
  w <- memory_mdp_writer(get_log = FALSE)
  w$set_weights(c("Duration", "Net reward"))
  w$set_trans_weights(c("Transition reward", "Transition risk"))
  w$process()
  w$stage()
  w$state(label = "initial")
  w$action(
    label = "low", weights = c(1, 5), prob = c(1, 0, 1),
    trans_weights = c(5, 50), end = TRUE
  )
  w$action(
    label = "high", weights = c(1, 8), prob = c(1, 0, 1),
    trans_weights = c(8, 80), end = TRUE
  )
  w$end_state()
  w$end_stage()
  w$stage()
  w$state(label = "terminal", end = TRUE)
  w$end_stage()
  w$end_process()
  w$close_writer()
}

test_that("set_action_weight modifies one action weight in memory", {
  mdp <- build_action_weight_mdp()
  initial_id <- mdp$ptr$getIds("0")[[1]]

  expect_invisible(set_action_weight(mdp, 10, s_id = initial_id, a_idx = 0, weight_name = "Net reward"))

  actions <- get_info(
    mdp, s_id = initial_id, with_list = FALSE, df_level = "action"
  )$df
  expect_equal(actions$weights[[1]], c(1, 10))
  expect_equal(actions$weights[[2]], c(1, 8))

  run_value_ite(mdp, "Net reward", term_values = 0, get_log = FALSE)
  policy <- get_policy(mdp, s_id = initial_id)
  expect_equal(policy$a_idx, 0)
  expect_equal(policy$weight, 10)
})

test_that("set_action_weight validates scalar inputs and model indices", {
  mdp <- build_action_weight_mdp()
  initial_id <- mdp$ptr$getIds("0")[[1]]

  expect_error(set_action_weight(mdp, c(1, 2), initial_id, 0, "Net reward"), "one finite numeric")
  expect_error(set_action_weight(mdp, Inf, initial_id, 0, "Net reward"), "one finite numeric")
  expect_error(set_action_weight(mdp, 1, c(0, 1), 0, "Net reward"), "one non-negative whole")
  expect_error(set_action_weight(mdp, 1, initial_id, 0.5, "Net reward"), "one non-negative whole")
  expect_error(set_action_weight(mdp, 1, initial_id, 0, "Missing"), "does not exist")
  expect_error(set_action_weight(mdp, 1, initial_id, 0, "r"), "ambiguous")
  expect_error(set_action_weight(mdp, 1, initial_id, 0, "Transition reward"), "does not exist")
  expect_error(set_action_weight(mdp, 1, 2, 0, "Net reward"), "State index out of range")
  expect_error(set_action_weight(mdp, 1, initial_id, 2, "Net reward"), "Action index out of range")
})

test_that("set_transition_weight modifies one transition weight in memory", {
  mdp <- build_action_weight_mdp()
  initial_id <- mdp$ptr$getIds("0")[[1]]

  expect_invisible(set_transition_weight(
    mdp, 10, s_id = initial_id, a_idx = 0, transition_idx = 0,
    weight_name = "Transition reward"
  ))

  actions <- get_info(
    mdp, s_id = initial_id, with_list = FALSE, df_level = "action"
  )$df
  expect_equal(actions$trans_weights[[1]], c(10, 50))
  expect_equal(actions$trans_weights[[2]], c(8, 80))

  run_value_ite(mdp, "Transition reward", term_values = 0, get_log = FALSE)
  policy <- get_policy(mdp, s_id = initial_id)
  expect_equal(policy$a_idx, 0)
  expect_equal(policy$weight, 10)
})

test_that("set_transition_weight validates scalar inputs and model indices", {
  mdp <- build_action_weight_mdp()
  initial_id <- mdp$ptr$getIds("0")[[1]]

  expect_error(set_transition_weight(mdp, c(1, 2), initial_id, 0, 0, "Transition reward"), "one finite numeric")
  expect_error(set_transition_weight(mdp, Inf, initial_id, 0, 0, "Transition reward"), "one finite numeric")
  expect_error(set_transition_weight(mdp, 1, c(0, 1), 0, 0, "Transition reward"), "one non-negative whole")
  expect_error(set_transition_weight(mdp, 1, initial_id, 0.5, 0, "Transition reward"), "one non-negative whole")
  expect_error(set_transition_weight(mdp, 1, initial_id, 0, 0.5, "Transition reward"), "one non-negative whole")
  expect_error(set_transition_weight(mdp, 1, initial_id, 0, 0, "Missing"), "does not exist")
  expect_error(set_transition_weight(mdp, 1, initial_id, 0, 0, "Transition"), "ambiguous")
  expect_error(set_transition_weight(mdp, 1, initial_id, 0, 0, "Net reward"), "does not exist")
  expect_error(set_transition_weight(mdp, 1, 2, 0, 0, "Transition reward"), "State index out of range")
  expect_error(set_transition_weight(mdp, 1, initial_id, 2, 0, "Transition reward"), "Action index out of range")
  expect_error(set_transition_weight(mdp, 1, initial_id, 0, 1, "Transition reward"), "Transition index out of range")
})

test_that("set_transition_weight changes persist when the model is saved", {
  mdp <- build_action_weight_mdp()
  initial_id <- mdp$ptr$getIds("0")[[1]]
  prefix <- paste0(tempfile("transition_weight_"), "_")

  set_transition_weight(mdp, 12, initial_id, 0, 0, "Transition reward")
  save_mdp(mdp, prefix = prefix, get_log = FALSE)
  reloaded <- load_mdp(prefix = prefix, get_log = FALSE)
  reloaded_id <- reloaded$ptr$getIds("0")[[1]]
  action <- reloaded$ptr$getActionInfo(reloaded_id)[[1]]

  expect_equal(action$transWeights, c(12, 50))
})
