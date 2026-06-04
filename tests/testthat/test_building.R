library(MDP2)
clean_up <- function() {
  unlink("*.bin")
  unlink("*.hmp")
}

test_that("binary_mdp_writer", {
  # sprintf("%.100f",g)
  source("files/HCT_ex6.1.1.R")
  mdp <- load_mdp("hct611_", get_log = FALSE)
  gProb <- run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE)
  rm(mdp)
  source("files/HCT_ex6.1.1_v2.R")
  mdp <- load_mdp("hct611v2_", get_log = FALSE)
  gPr <- run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE)
  rm(mdp)
  expect_equal(gProb, gPr)

  source("files/HCT_exc6.4.R")
  mdp <- load_mdp("hct64_", get_log = FALSE)
  gProb <- run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE)
  rm(mdp)
  source("files/HCT_exc6.4_v2.R")
  mdp <- load_mdp("hct64v2_", get_log = FALSE)
  gPr <- run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE)
  rm(mdp)
  expect_equal(gProb, gPr)

  source("files/HCT_exc6.7.R")
  mdp <- load_mdp("hct67_", get_log = FALSE)
  gProb <- run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE)
  rm(mdp)
  source("files/HCT_exc6.7_v2.R")
  mdp <- load_mdp("hct67v2_", get_log = FALSE)
  gPr <- run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE)
  rm(mdp)
  expect_equal(gProb, gPr)

  source("files/HCT_exc7.3.R")
  mdp <- load_mdp("hct73_", get_log = FALSE)
  gProb <- run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE)
  rm(mdp)
  source("files/HCT_exc7.3_v2.R")
  mdp <- load_mdp("hct73v2_", get_log = FALSE)
  gPr <- run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE)
  rm(mdp)
  expect_equal(gProb, gPr)

  ## test loading with P, R and D
  source("files/HCT_ex6.1.1_v3.R")
  # test using L.R.A reward / t.unit
  mdp <- load_mdp("hct611v3-1_", get_log = FALSE)
  expect_equal(run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE), -0.433789954337899297254210750907077454030513763427734375)
  rm(mdp)
  mdp <- load_mdp("hct611v3-2_", get_log = FALSE)
  expect_equal(run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE), -0.433789954337899297254210750907077454030513763427734375)
  rm(mdp)
  mdp <- load_mdp("hct611v3-3_", get_log = FALSE)
  expect_equal(run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE), -0.433789954337899297254210750907077454030513763427734375)
  rm(mdp)
  mdp <- load_mdp("hct611v3-4_", get_log = FALSE)
  expect_equal(run_policy_ite_ave(mdp, "Net reward", "Duration", get_log = FALSE), -0.433789954337899297254210750907077454030513763427734375)
  rm(mdp)
  # test using discounted reward
  # compare MDP formulations
  mdp <- load_mdp("hct611v3-1_", get_log = FALSE)
  run_policy_ite_discount(mdp, "Net reward", "Duration", discount_factor = 0.9, get_log = FALSE)
  weights_policy_ite <- get_policy(mdp)$weight
  expect_equal(sum(weights_policy_ite), -37.94871237074722358784129028208553791046142578125)
  rm(mdp)
  mdp <- load_mdp("hct611v3-2_", get_log = FALSE)
  run_policy_ite_discount(mdp, "Net reward", "Duration", discount_factor = 0.9, get_log = FALSE)
  weights_policy_ite <- get_policy(mdp)$weight
  expect_equal(sum(weights_policy_ite), -37.94871237074722358784129028208553791046142578125)
  rm(mdp)
  mdp <- load_mdp("hct611v3-3_", get_log = FALSE)
  run_policy_ite_discount(mdp, "Net reward", "Duration", discount_factor = 0.9, get_log = FALSE)
  weights_policy_ite <- get_policy(mdp)$weight
  expect_equal(sum(weights_policy_ite), -37.94871237074722358784129028208553791046142578125)
  rm(mdp)
  # compare semi-MDP formulations
  mdp <- load_mdp("hct611v2_", get_log = FALSE)
  run_policy_ite_discount(mdp, "Net reward", "Duration", discount_factor = 0.9, get_log = FALSE)
  weights_policy_ite1 <- get_policy(mdp)$weight
  rm(mdp)
  mdp <- load_mdp("hct611v3-4_", get_log = FALSE)
  run_policy_ite_discount(mdp, "Net reward", "Duration", discount_factor = 0.9, get_log = FALSE)
  weights_policy_ite2 <- get_policy(mdp)$weight
  rm(mdp)
  expect_equal(weights_policy_ite1, weights_policy_ite2)
})

test_that("load_mdp supports models with states and no actions", {
  prefix <- paste0(tempfile("no_actions_"), "_")
  w <- binary_mdp_writer(prefix, get_log = FALSE)
  w$set_weights(c("Duration", "Net reward"))
  w$process()
  w$stage()
  for (ii in 2:10) {
    w$state(label = "test")
    w$end_state()
  }
  w$end_stage()
  w$end_process()
  w$close_writer()

  mdp <- load_mdp(prefix, get_log = FALSE)

  expect_s3_class(mdp, "HMDP")
  expect_equal(mdp$states, 9)
  expect_equal(mdp$actions, 0)
  expect_equal(mdp$weight_names, c("Duration", "Net reward"))
})

test_that("load_mdp rejects transitions to non-existing states", {
  prefix <- paste0(tempfile("bad_transition_"), "_")
  w <- binary_mdp_writer(prefix, get_log = FALSE)
  w$set_weights(c("Duration", "Net reward"))
  w$process()
  w$stage()
  for (ii in 2:10) {
    w$state(label = "test")
    w$action(
      label = "a1",
      scope = 1,
      weights = c(1, 100),
      id = c(0, 99),
      pr = c(0.5, 0.5),
      end = TRUE
    )
    w$end_state()
  }
  w$end_stage()
  w$end_process()
  w$close_writer()

  expect_message(
    mdp <- load_mdp(prefix, get_log = FALSE),
    "transition to a non-existing state"
  )
  expect_null(mdp)
})

test_that("binary_mdp_writer detects unclosed writer blocks", {
  prefix <- paste0(tempfile("writer_stack_"), "_")
  w <- binary_mdp_writer(prefix, get_log = FALSE)
  w$set_weights(c("Duration", "Net reward"))
  w$process()
  w$stage()
  w$state()
  w$action(scope = 1, weights = c(1, 100), id = 0, pr = 1)

  expect_error(
    w$end_state(),
    "Call end_action\\(\\) or use action\\(\\.\\.\\., end = TRUE\\)"
  )
  expect_error(
    w$close_writer(),
    "action is still open"
  )

  w$end_action()
  expect_error(
    w$end_stage(),
    "Cannot end a stage unless a stage is open"
  )
  expect_error(
    w$close_writer(),
    "state is still open"
  )

  w$end_state()
  w$end_stage()
  w$end_process()
  w$close_writer()
})

test_that("binary_mdp_writer requires correct block nesting", {
  prefix <- paste0(tempfile("writer_nesting_"), "_")
  w <- binary_mdp_writer(prefix, get_log = FALSE)
  w$set_weights(c("Duration", "Net reward"))

  expect_error(w$stage(), "outside an open process")
  expect_error(w$state(), "outside an open stage")
  expect_error(
    w$action(scope = 1, weights = c(1, 100), id = 0, pr = 1),
    "outside an open state"
  )
  expect_error(w$end_action(), "unless an action is open")
  expect_error(w$end_state(), "while another writer block is open")
  expect_error(w$end_stage(), "unless a stage is open")
  expect_error(w$end_process(), "unless a process is open")

  w$process()
  expect_error(w$process(), "before closing the current writer block")
  w$end_process()
  w$close_writer()
})

test_that("binary_mdp_writer allows one redundant end_action after end equals TRUE", {
  prefix <- paste0(tempfile("writer_redundant_end_"), "_")
  w <- binary_mdp_writer(prefix, get_log = FALSE)
  w$set_weights(c("Duration", "Net reward"))
  w$process()
  w$stage()
  w$state()
  w$action(scope = 1, weights = c(1, 100), id = 0, pr = 1, end = TRUE)
  expect_no_error(w$end_action())
  expect_error(w$end_action(), "unless an action is open")
  w$end_state()
  w$end_stage()
  w$end_process()
  w$close_writer()
})

test_that("binary_mdp_writer allows nested process inside an action", {
  prefix <- paste0(tempfile("writer_nested_"), "_")
  w <- binary_mdp_writer(prefix, get_log = FALSE)
  w$set_weights(c("Duration", "Net reward"))
  w$process()
  w$stage()
  w$state()
  w$action(weights = c(0, 0), prob = c(2, 0, 1))
  expect_no_error(w$process())
  w$stage()
  w$state()
  w$action(weights = c(0, 0), prob = c(1, 0, 1))
  w$end_action()
  w$end_state()
  w$end_stage()
  w$end_process()
  w$end_action()
  w$end_state()
  w$end_stage()
  w$end_process()
  w$close_writer()
})

test_that("memory_mdp_writer builds the same model as binary_mdp_writer", {
  build_model <- function(w) {
    w$set_weights(c("Duration", "Net reward"))
    w$process()
    w$stage()
    w$state(label = "s0")
    w$action(
      label = "a0", scope = c(1, 1), id = c(0, 1),
      pr = c(0.25, 0.75), weights = c(1, 10), end = TRUE
    )
    w$end_state()
    w$state(label = "s1")
    w$action(
      label = "a1", scope = c(1, 1), id = c(0, 1),
      pr = c(0.5, 0.5), weights = c(1, 20), end = TRUE
    )
    w$end_state()
    w$end_stage()
    w$stage()
    w$state(label = "t0", end = TRUE)
    w$state(label = "t1", end = TRUE)
    w$end_stage()
    w$end_process()
    w
  }

  prefix <- paste0(tempfile("memory_compare_"), "_")
  wb <- build_model(binary_mdp_writer(prefix, get_log = FALSE))
  wb$close_writer()
  binary_mdp <- load_mdp(prefix, get_log = FALSE)

  wm <- build_model(memory_mdp_writer(get_log = FALSE))
  memory_mdp <- wm$close_writer()

  expect_s3_class(memory_mdp, "HMDP")
  expect_equal(memory_mdp$states, binary_mdp$states)
  expect_equal(memory_mdp$actions, binary_mdp$actions)
  expect_equal(memory_mdp$weight_names, binary_mdp$weight_names)
  memory_ids <- memory_mdp$ptr$getIds("0")
  binary_ids <- binary_mdp$ptr$getIds("0")
  expect_equal(memory_mdp$ptr$getActionInfo(memory_ids[1]), binary_mdp$ptr$getActionInfo(binary_ids[1]))
  expect_equal(memory_mdp$ptr$getActionInfo(memory_ids[2]), binary_mdp$ptr$getActionInfo(binary_ids[2]))
})

test_that("memory_mdp_writer supports transition weights", {
  w <- memory_mdp_writer(get_log = FALSE)
  w$set_weights(c("Duration"))
  w$set_trans_weights(c("Reward", "Risk"))
  w$process()
  w$stage()
  w$state(label = "s0")
  w$action(
    label = "a0",
    scope = c(1, 1),
    id = c(0, 1),
    pr = c(0.5, 0.5),
    weights = 1,
    trans_weights = c(10, 1, 20, 2),
    end = TRUE
  )
  w$end_state()
  w$end_stage()
  w$stage()
  w$state(label = "t0", end = TRUE)
  w$state(label = "t1", end = TRUE)
  w$end_stage()
  w$end_process()

  mdp <- w$close_writer()
  info <- mdp$ptr$getActionInfo(mdp$ptr$getIds("0")[1])[[1]]

  expect_equal(mdp$weight_action_names, "Duration")
  expect_equal(mdp$weight_trans_names, c("Reward", "Risk"))
  expect_equal(info$transWeights, c(10, 1, 20, 2))
})

test_that("memory_mdp_writer supports models with states and no actions", {
  w <- memory_mdp_writer(get_log = FALSE)
  w$set_weights(c("Duration", "Net reward"))
  w$process()
  w$stage()
  for (ii in 1:4) w$state(label = paste0("s", ii), end = TRUE)
  w$end_stage()
  w$end_process()

  mdp <- w$close_writer()

  expect_s3_class(mdp, "HMDP")
  expect_equal(mdp$states, 4)
  expect_equal(mdp$actions, 0)
  expect_equal(mdp$weight_names, c("Duration", "Net reward"))
})

test_that("memory_mdp_writer supports nested processes", {
  expect_no_error(source(system.file("examples/memory_mdp_writer-ex.R", package = "MDP2")))
})

test_that("memory_mdp_writer rejects external processes and use after close", {
  w <- memory_mdp_writer(get_log = FALSE)
  w$set_weights(c("Duration", "Net reward"))
  expect_error(w$include_process(), "does not support external processes")
  w$process()
  w$stage()
  w$state(label = "s0", end = TRUE)
  w$end_stage()
  w$end_process()
  mdp <- w$close_writer()

  expect_s3_class(mdp, "HMDP")
  expect_error(w$state(), "memory_mdp_writer is closed")
  expect_error(w$close_writer(), "memory_mdp_writer is closed")
})

build_with_writer <- function(build_model) {
  prefix <- paste0(tempfile("writer_compare_"), "_")
  wb <- build_model(binary_mdp_writer(prefix, get_log = FALSE))
  wb$close_writer()
  binary_mdp <- load_mdp(prefix, get_log = FALSE)

  wm <- build_model(memory_mdp_writer(get_log = FALSE))
  memory_mdp <- wm$close_writer()

  list(binary = binary_mdp, memory = memory_mdp)
}

expect_same_model_summary <- function(models) {
  expect_s3_class(models$memory, "HMDP")
  expect_equal(models$memory$states, models$binary$states)
  expect_equal(models$memory$actions, models$binary$actions)
  expect_equal(models$memory$levels, models$binary$levels)
  expect_equal(models$memory$weight_names, models$binary$weight_names)
  expect_equal(models$memory$weight_action_names, models$binary$weight_action_names)
  expect_equal(models$memory$weight_trans_names, models$binary$weight_trans_names)
}

build_vignette_hct_hierarchical <- function(w) {
  N <- 5
  labels <- paste0("i = ", 1:N)
  Cf <- -10
  Cp <- c(0, -7, -7, -5)
  Q <- matrix(
    c(
      0.90, 0.10, 0, 0, 0,
      0, 0.80, 0.10, 0.05, 0.05,
      0, 0, 0.70, 0.10, 0.20,
      0, 0, 0, 0.50, 0.50
    ),
    nrow = 4, byrow = TRUE
  )
  trans_pr <- function(i, a) {
    pr <- NULL
    idx <- NULL
    if (a == "nr") {
      pr <- Q[i, ]
      idx <- which(pr > 0)
      pr <- pr[idx]
      idx <- idx - 1
    }
    if (a == "pr" | a == "fr") {
      pr <- 1
      idx <- 0
    }
    list(pr = pr, idx = idx)
  }

  w$set_weights(c("Duration", "Net reward"))
  w$process()
  w$stage()
  w$state(label = labels[1])
  lst <- trans_pr(1, "nr")
  w$action(label = "nr", weights = c(1, 0), pr = lst$pr, id = lst$idx, end = TRUE)
  w$end_state()
  for (i in 2:(N - 1)) {
    w$state(label = labels[i])
    lst <- trans_pr(i, "nr")
    w$action(label = "nr", weights = c(1, 0), pr = lst$pr, id = lst$idx, end = TRUE)
    lst <- trans_pr(i, "pr")
    w$action(label = "pr", weights = c(1, Cp[i]), pr = lst$pr, id = lst$idx, end = TRUE)
    w$end_state()
  }
  w$state(label = labels[N])
  lst <- trans_pr(N, "fr")
  w$action(label = "fr", weights = c(2, Cf), pr = lst$pr, id = lst$idx, end = TRUE)
  w$end_state()
  w$end_stage()
  w$end_process()
  w
}

build_vignette_hct_matrices <- function(w) {
  N <- 5
  Cf <- -10
  Cp <- c(0, -7, -7, -5)
  Q <- matrix(
    c(
      0.90, 0.10, 0, 0, 0,
      0, 0.80, 0.10, 0.05, 0.05,
      0, 0, 0.70, 0.10, 0.20,
      0, 0, 0, 0.50, 0.50
    ),
    nrow = 4, byrow = TRUE
  )

  P <- list()
  P[[1]] <- as.matrix(rbind(Q, 0))
  Z <- matrix(0, nrow = N, ncol = N)
  Z[2, 1] <- Z[3, 1] <- Z[4, 1] <- 1
  P[[2]] <- Z
  Z <- matrix(0, nrow = N, ncol = N)
  Z[5, 1] <- 1
  P[[3]] <- Z

  R <- matrix(0, nrow = N, ncol = 3)
  R[2:4, 2] <- Cp[2:4]
  R[5, 3] <- Cf
  D <- matrix(1, nrow = N, ncol = 3)
  D[5, 3] <- 2

  w$set_weights(c("Duration", "Net reward"))
  w$process(P, R, D)
  w
}

build_vignette_machine <- function(w) {
  w$set_weights(c("Net reward"))
  w$process()
  w$stage()
  w$state(label = "dummy")
  w$action(label = "buy", weights = -100, pr = c(0.7, 0.3), id = c(0, 1), end = TRUE)
  w$end_state()
  w$end_stage()
  w$stage()
  w$state(label = "good")
  w$action(label = "mt", weights = 55, pr = 1, id = 0, end = TRUE)
  w$action(label = "nmt", weights = 70, pr = c(0.6, 0.4), id = c(0, 1), end = TRUE)
  w$end_state()
  w$state(label = "average")
  w$action(label = "mt", weights = 40, pr = 1, id = 0, end = TRUE)
  w$action(label = "nmt", weights = 50, pr = c(0.6, 0.4), id = c(1, 2), end = TRUE)
  w$end_state()
  w$end_stage()
  w$stage()
  w$state(label = "good")
  w$action(label = "mt", weights = 55, pr = 1, id = 0, end = TRUE)
  w$action(label = "nmt", weights = 70, pr = c(0.5, 0.5), id = c(0, 1), end = TRUE)
  w$end_state()
  w$state(label = "average")
  w$action(label = "mt", weights = 40, pr = 1, id = 0, end = TRUE)
  w$action(label = "nmt", weights = 50, pr = c(0.5, 0.5), id = c(1, 2), end = TRUE)
  w$end_state()
  w$state(label = "not working")
  w$action(label = "mt", weights = 30, pr = 1, id = 0, end = TRUE)
  w$action(label = "rep", weights = 5, pr = 1, id = 3, end = TRUE)
  w$end_state()
  w$end_stage()
  w$stage()
  w$state(label = "good")
  w$action(label = "mt", weights = 55, pr = 1, id = 0, end = TRUE)
  w$action(label = "nmt", weights = 70, pr = c(0.2, 0.8), id = c(0, 1), end = TRUE)
  w$end_state()
  w$state(label = "average")
  w$action(label = "mt", weights = 40, pr = 1, id = 0, end = TRUE)
  w$action(label = "nmt", weights = 50, pr = c(0.2, 0.8), id = c(1, 2), end = TRUE)
  w$end_state()
  w$state(label = "not working")
  w$action(label = "mt", weights = 30, pr = 1, id = 0, end = TRUE)
  w$action(label = "rep", weights = 5, pr = 1, id = 3, end = TRUE)
  w$end_state()
  w$state(label = "replaced")
  w$action(label = "dummy", weights = 0, pr = 1, id = 3, end = TRUE)
  w$end_state()
  w$end_stage()
  w$stage()
  w$state(label = "good", end = TRUE)
  w$state(label = "average", end = TRUE)
  w$state(label = "not working", end = TRUE)
  w$state(label = "replaced", end = TRUE)
  w$end_stage()
  w$end_process()
  w
}

build_vignette_cow <- function(w) {
  cow_file <- c(
    file.path("files", "cow.csv"),
    file.path("tests", "testthat", "files", "cow.csv"),
    file.path("vignettes", "vignette_files", "cow.csv"),
    file.path("..", "..", "vignettes", "vignette_files", "cow.csv"),
    file.path("..", "vignettes", "vignette_files", "cow.csv")
  )
  cow_file <- cow_file[file.exists(cow_file)][1]
  if (is.na(cow_file)) stop("Could not find cow.csv test fixture.", call. = FALSE)
  cow_df <- utils::read.csv(cow_file)
  lev1_w <- function(s0Idx, n1Idx, s1Idx, a1Lbl) {
    row <- cow_df[cow_df$s0 == s0Idx & cow_df$n1 == n1Idx & cow_df$s1 == s1Idx & cow_df$label == a1Lbl, ]
    as.numeric(row[c("Duration", "Reward", "Output")])
  }
  lev1_pr <- function(s0Idx, n1Idx, s1Idx, a1Lbl) {
    row <- cow_df[cow_df$s0 == s0Idx & cow_df$n1 == n1Idx & cow_df$s1 == s1Idx & cow_df$label == a1Lbl, ]
    as.numeric(row[paste0(rep(c("scp", "idx", "pr"), 3), rep(0:2, each = 3))])
  }

  lblS0 <- c("Bad genetic level", "Avg genetic level", "Good genetic level")
  lblS1 <- c("Low yield", "Avg yield", "High yield")
  w$set_weights(c("Duration", "Net reward", "Yield"))
  w$process()
  w$stage()
  for (s0 in 0:2) {
    w$state(label = lblS0[s0 + 1])
    w$action(label = "Keep", weights = c(0, 0, 0), prob = c(2, 0, 1))
    w$process()
    w$stage()
    w$state(label = "Dummy")
    w$action(
      label = "Dummy", weights = c(0, 0, 0),
      prob = c(1, 0, 1 / 3, 1, 1, 1 / 3, 1, 2, 1 / 3), end = TRUE
    )
    w$end_state()
    w$end_stage()
    for (d1 in 1:4) {
      w$stage()
      for (s1 in 0:2) {
        w$state(label = lblS1[s1 + 1])
        if (d1 != 4) {
          w$action(
            label = "Keep", weights = lev1_w(s0, d1, s1, "Keep"),
            prob = lev1_pr(s0, d1, s1, "Keep"), end = TRUE
          )
        }
        w$action(
          label = "Replace", weights = lev1_w(s0, d1, s1, "Replace"),
          prob = lev1_pr(s0, d1, s1, "Replace"), end = TRUE
        )
        w$end_state()
      }
      w$end_stage()
    }
    w$end_process()
    w$end_action()
    w$end_state()
  }
  w$end_stage()
  w$end_process()
  w
}

test_that("building vignette HCT hierarchical model matches for binary and memory writers", {
  models <- build_with_writer(build_vignette_hct_hierarchical)
  expect_same_model_summary(models)
  expect_equal(
    run_policy_ite_ave(models$memory, "Net reward", "Duration", get_log = FALSE),
    run_policy_ite_ave(models$binary, "Net reward", "Duration", get_log = FALSE)
  )
})

test_that("building vignette HCT matrix model matches for binary and memory writers", {
  models <- build_with_writer(build_vignette_hct_matrices)
  expect_same_model_summary(models)
  expect_equal(
    run_policy_ite_ave(models$memory, "Net reward", "Duration", get_log = FALSE),
    run_policy_ite_ave(models$binary, "Net reward", "Duration", get_log = FALSE)
  )
})

test_that("building vignette machine model matches for binary and memory writers", {
  models <- build_with_writer(build_vignette_machine)
  expect_same_model_summary(models)
  run_value_ite(models$binary, "Net reward", term_values = c(30, 10, 5, 0), get_log = FALSE)
  binary_policy <- get_policy(models$binary)
  run_value_ite(models$memory, "Net reward", term_values = c(30, 10, 5, 0), get_log = FALSE)
  memory_policy <- get_policy(models$memory)
  expect_equal(memory_policy$weight, binary_policy$weight)
  expect_equal(memory_policy$action_label, binary_policy$action_label)
})

test_that("building vignette cow model matches for binary and memory writers", {
  models <- build_with_writer(build_vignette_cow)
  expect_same_model_summary(models)
  expect_equal(
    run_policy_ite_ave(models$memory, "Net reward", "Duration", get_log = FALSE),
    run_policy_ite_ave(models$binary, "Net reward", "Duration", get_log = FALSE)
  )
  expect_equal(
    run_policy_ite_ave(models$memory, "Yield", "Duration", get_log = FALSE),
    run_policy_ite_ave(models$binary, "Yield", "Duration", get_log = FALSE)
  )
})

clean_up()
