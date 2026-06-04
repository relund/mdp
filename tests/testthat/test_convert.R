library(MDP2)
clean_up <- function() {
  unlink("*.bin")
  unlink("*.hmp")
}

test_that("convert_hmp_to_binary", {
  source("files/two_level_hmdp.R")
  prefix <- "2lev_"
  mdp <- load_mdp(prefix, get_log = FALSE)
  info1 <- get_info(mdp, df_level = "action", as_strings_actions = TRUE)
  g1 <- run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE)
  rm(mdp)
  convert_binary_to_hmp(prefix, get_log = FALSE)
  prefix1 <- "2lev-converted_"
  convert_hmp_to_binary(paste0(prefix, "converted.hmp"), prefix1, get_log = FALSE)
  mdp <- load_mdp(prefix1, get_log = FALSE)
  info2 <- get_info(mdp, df_level = "action", as_strings_actions = TRUE)
  g2 <- run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE)
  rm(mdp)
  expect_equal(g1, g2)
  expect_equal(info1$df, info2$df)

  source("files/machine_replacement_v1.R")
  prefix <- "machine1_"
  mdp <- load_mdp(prefix, get_log = FALSE)
  info1 <- get_info(mdp, df_level = "action", as_strings_actions = TRUE)
  run_value_ite(mdp, "Net reward", term_values = c(30, 10, 5, 0), get_log = FALSE)
  g1 <- get_policy(mdp)[9, 5]
  rm(mdp)
  convert_binary_to_hmp(prefix, duration = NULL, get_log = FALSE)
  prefix1 <- "machine1-converted_"
  convert_hmp_to_binary(paste0(prefix, "converted.hmp"), prefix1, get_log = FALSE)
  mdp <- load_mdp(prefix1, get_log = FALSE)
  info2 <- get_info(mdp, df_level = "action", as_strings_actions = TRUE)
  run_value_ite(mdp, "Net reward", term_values = c(30, 10, 5, 0), get_log = FALSE)
  g2 <- get_policy(mdp)[9, 5]
  rm(mdp)
  expect_equal(g1, g2)
  expect_equal(info1$df$trans, info2$df$trans)
  expect_equal(info1$df$pr, info2$df$pr)

  # Small MDP
  prefix <- "test_"
  w <- binary_mdp_writer(prefix, get_log = FALSE)
  w$set_weights(c("Duration", "Net rewards"))
  w$process()
  w$stage("Lactation cycle")
  w$state("Low")
  w$action(label = "Keep", scope = 1, weights = c(1, 10000), id = c(0, 1), pr = c(0.6, 0.4))
  w$end_action()
  w$end_state()
  w$state("Average")
  w$action(label = "Keep", scope = 1, weights = c(1, 12000), id = c(0, 1), pr = c(0.2, 0.8))
  w$end_action()
  w$end_state()
  w$end_stage()
  w$end_process()
  w$close_writer()
  mdp <- load_mdp(prefix, get_log = FALSE)
  info1 <- get_info(mdp, df_level = "action", as_strings_actions = TRUE)
  g1 <- run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE)
  rm(mdp)
  convert_binary_to_hmp(prefix, get_log = FALSE)
  prefix1 <- "test-converted_"
  convert_hmp_to_binary(paste0(prefix, "converted.hmp"), prefix1, get_log = FALSE)
  mdp <- load_mdp(prefix1, get_log = FALSE)
  info2 <- get_info(mdp, df_level = "action", as_strings_actions = TRUE)
  g2 <- run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE)
  rm(mdp)
  expect_equal(g1, g2)
  expect_equal(info1$df, info2$df)

  # Small MDP with no labels
  prefix <- "test2_"
  w <- binary_mdp_writer(prefix, get_log = FALSE)
  w$set_weights(c("D", "N"))
  w$process()
  w$stage()
  w$state()
  w$action(scope = 1, weights = c(1, 10000), id = c(0, 1), pr = c(0.6, 0.4), end = TRUE)
  w$end_state()
  w$state()
  w$action(scope = 1, weights = c(1, 12000), id = c(0, 1), pr = c(0.2, 0.8), end = TRUE)
  w$end_state()
  w$end_stage()
  w$end_process()
  w$close_writer()
  mdp <- load_mdp(prefix, get_log = FALSE)
  info1 <- get_info(mdp, df_level = "action", as_strings_actions = TRUE)
  g1 <- run_policy_ite_ave(mdp, "N", "D", get_log = FALSE)
  rm(mdp)
  convert_binary_to_hmp(prefix, get_log = FALSE)
  prefix1 <- "test-converted_"
  convert_hmp_to_binary(paste0(prefix, "converted.hmp"), prefix1, get_log = FALSE)
  mdp <- load_mdp(prefix1, get_log = FALSE)
  info2 <- get_info(mdp, df_level = "action", as_strings_actions = TRUE)
  g2 <- run_policy_ite_ave(mdp, "N", "D", get_log = FALSE)
  rm(mdp)
  expect_equal(g1, g2)
  info1$df$label <- NULL
  info2$df$label <- NULL
  info1$df$label_action <- NULL
  info2$df$label_action <- NULL
  expect_equal(info1$df, info2$df)

  ## Existing hmp files
  n <- 3
  prefix <- paste0("cow", n, "_")
  convert_hmp_to_binary(paste0("files/cow", n, ".hmp"), prefix, get_log = FALSE)
  mdp <- load_mdp(prefix, get_log = FALSE)
  info1 <- get_info(mdp, df_level = "action", as_strings_actions = TRUE)
  expect_equal(mdp$states, n)
  rm(mdp)
  convert_binary_to_hmp(prefix, get_log = FALSE)
  prefix1 <- paste0("cow-converted", n, "_")
  convert_hmp_to_binary(paste0(prefix, "converted.hmp"), prefix1, get_log = FALSE)
  mdp <- load_mdp(prefix1, get_log = FALSE)
  info2 <- get_info(mdp, df_level = "action", as_strings_actions = TRUE)
  rm(mdp)
  expect_equal(info1$df, info2$df)
  # 12 states
  n <- 12
  prefix <- paste0("cow", n, "_")
  convert_hmp_to_binary(paste0("files/cow", n, ".hmp"), prefix, get_log = FALSE)
  mdp <- load_mdp(prefix, get_log = FALSE)
  info1 <- get_info(mdp, df_level = "action", as_strings_actions = TRUE)
  expect_equal(mdp$states, n)
  rm(mdp)
  convert_binary_to_hmp(prefix, get_log = FALSE)
  prefix1 <- paste0("cow-converted", n, "_")
  convert_hmp_to_binary(paste0(prefix, "converted.hmp"), prefix1, get_log = FALSE)
  mdp <- load_mdp(prefix1, get_log = FALSE)
  info2 <- get_info(mdp, df_level = "action", as_strings_actions = TRUE)
  rm(mdp)
  expect_equal(info1$df, info2$df)
  ## 36 states
  n <- 36
  prefix <- paste0("cow", n, "_")
  convert_hmp_to_binary(paste0("files/cow", n, ".hmp"), prefix, get_log = FALSE)
  mdp <- load_mdp(prefix, get_log = FALSE)
  info1 <- get_info(mdp, df_level = "action", as_strings_actions = TRUE)
  expect_equal(mdp$states, n)
  rm(mdp)
  convert_binary_to_hmp(prefix, get_log = FALSE)
  prefix1 <- paste0("cow-converted", n, "_")
  convert_hmp_to_binary(paste0(prefix, "converted.hmp"), prefix1, get_log = FALSE)
  mdp <- load_mdp(prefix1, get_log = FALSE)
  info2 <- get_info(mdp, df_level = "action", as_strings_actions = TRUE)
  rm(mdp)
  expect_equal(info1$df, info2$df)
})

clean_up()
