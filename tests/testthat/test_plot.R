library(MDP2)

test_that("plot_hypergraph draws optional trans labels", {
  hgf <- list(
    nodes = tibble::tibble(
      s_id = c(0, 1, 2),
      g_id = c(1, 3, 4),
      label = c("Head", "Tail A", "Tail B")
    ),
    hyperarcs = tibble::tibble(
      s_id = 0,
      action_weights = list(10),
      trans = list(c(1, 2)),
      pr = list(c(0.25, 0.75)),
      trans_labels = list(c("left", "right")),
      trans_weights = list(matrix(c(99, 101), ncol = 1, dimnames = list(NULL, "Reward"))),
      a_idx = 0,
      label = "A",
      lwd = 1,
      lty = 1,
      col = "black"
    )
  )
  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_invisible(plot_hypergraph(hgf, c(2, 2)))
  expect_invisible(plot_hypergraph(hgf, c(2, 2), trans_labels = "state"))
  expect_invisible(plot_hypergraph(hgf, c(2, 2), trans_labels = "s_id"))
  expect_invisible(plot_hypergraph(hgf, c(2, 2), trans_labels = "prob"))
  expect_invisible(plot_hypergraph(hgf, c(2, 2), trans_labels = "weights"))
  expect_invisible(plot_hypergraph(hgf, c(2, 2), trans_labels = "label|prob|weights"))
  expect_invisible(plot_hypergraph(hgf, c(2, 2), trans_labels = "custom"))
})

test_that("plot_hypergraph ignores missing trans and validates label columns", {
  hgf <- list(
    nodes = tibble::tibble(s_id = c(0, 1), g_id = c(1, 3), label = c("Head", "Tail")),
    hyperarcs = tibble::tibble(
      s_id = 0,
      action_weights = list(numeric(0)),
      trans = list(c(1, NA_real_)),
      pr = list(c(1, NA_real_)),
      a_idx = 0,
      label = "A",
      lwd = 1,
      lty = 1,
      col = "black"
    )
  )
  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_invisible(plot_hypergraph(hgf, c(2, 2), trans_labels = "prob"))
  expect_error(
    plot_hypergraph(hgf, c(2, 2), trans_labels = "custom"),
    'trans_labels = "custom" requires a trans_labels list-column'
  )
  expect_error(
    plot_hypergraph(hgf, c(2, 2), trans_labels = "weights"),
    'trans_labels containing "weights" requires a trans_weights list-column'
  )
  expect_error(
    plot_hypergraph(
      list(nodes = hgf$nodes, hyperarcs = dplyr::select(hgf$hyperarcs, -pr)),
      c(2, 2),
      trans_labels = "prob"
    ),
    'trans_labels containing "prob" requires a pr list-column'
  )
})

test_that("plot_hypergraph supports zero, single, and multiple transition weights", {
  hgf <- list(
    nodes = tibble::tibble(s_id = c(0, 1), g_id = c(1, 3), label = c("Head", "Tail")),
    hyperarcs = tibble::tibble(
      s_id = c(0, 0, 0),
      action_weights = list(numeric(0), numeric(0), numeric(0)),
      trans = list(1, 1, 1),
      pr = list(1, 1, 1),
      trans_weights = list(
        matrix(numeric(0), nrow = 1, ncol = 0),
        matrix(10, nrow = 1),
        matrix(c(10, 20), nrow = 1)
      ),
      a_idx = c(0, 1, 2),
      label = c("A", "B", "C"),
      lwd = c(1, 1, 1),
      lty = c(1, 1, 1),
      col = c("black", "black", "black")
    )
  )
  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_invisible(plot_hypergraph(hgf, c(2, 2), trans_labels = "weights"))
})

test_that("get_hypergraph returns nested weight and transition columns", {
  prefix <- file.path(tempdir(), "plot_trans_weight_")
  w <- binary_mdp_writer(prefix = prefix, get_log = FALSE)
  w$set_weights("Action weight")
  w$set_trans_weights("Transition weight")
  w$process()
  w$stage()
  w$state()
  w$action(
    weights = 5,
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

  mdp <- load_mdp(prefix, get_log = FALSE)
  hgf <- get_hypergraph(mdp)

  expect_true(all(c("action_weights", "trans", "pr", "trans_weights") %in% names(hgf$hyperarcs)))
  expect_false(any(grepl("^trans[0-9]+$|^pr[0-9]+$", names(hgf$hyperarcs))))
  expect_equal(hgf$hyperarcs$action_weights[[1]], 5)
  expect_equal(hgf$hyperarcs$trans[[1]], c(0, 1))
  expect_equal(hgf$hyperarcs$pr[[1]], c(0.25, 0.75))
  expect_equal(
    hgf$hyperarcs$trans_weights[[1]],
    matrix(c(10, 20), ncol = 1, dimnames = list(NULL, "Transition weight"))
  )
})

test_that("plot_hypergraph supports state_label, action_label, and action_w_label options", {
  prefix <- file.path(tempdir(), "plot_opts_")
  w <- binary_mdp_writer(prefix = prefix, get_log = FALSE)
  w$set_weights("Action weight")
  w$set_trans_weights("Transition weight")
  w$process()
  w$stage()
  w$state(label = "S1")
  w$action(
    label = "A1",
    weights = 5,
    prob = c(1, 0, 0.25, 1, 1, 0.75),
    trans_weights = c(10, 20)
  )
  w$end_action()
  w$end_state()
  w$end_stage()
  w$stage()
  w$state(label = "S2")
  w$end_state()
  w$state(label = "S3")
  w$end_state()
  w$end_stage()
  w$end_process()
  w$close_writer()

  mdp <- load_mdp(prefix, get_log = FALSE)
  hgf <- get_hypergraph(mdp)
  hgf$nodes$g_id <- c(1, 3, 4)

  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_invisible(plot_hypergraph(hgf, c(2, 2)))
  expect_invisible(plot_hypergraph(hgf, c(2, 2), state_label = "s_id|label", action_label = "a_idx|label", action_w_label = "none"))
  expect_invisible(plot_hypergraph(hgf, c(2, 2), state_label = "s_idx|label"))
  expect_invisible(plot_hypergraph(hgf, c(2, 2), state_label = "weight", mdp = mdp))
  expect_invisible(plot_hypergraph(hgf, c(2, 2), state_label = "label|weight", mdp = mdp))
  expect_invisible(plot_hypergraph(hgf, c(2, 2), state_label = "s_id|weight|s_idx", mdp = mdp))
  expect_invisible(plot_hypergraph(hgf, c(2, 2), action_label = "label|a_idx"))
  expect_error(plot_hypergraph(hgf, c(2, 2), state_label = "weight"), "mdp model must be provided")
  expect_error(plot_hypergraph(hgf, c(2, 2), state_label = "label|weight"), "mdp model must be provided")
  expect_invisible(plot_hypergraph(hgf, c(2, 2), action_color = "label"))
  expect_invisible(plot_hypergraph(hgf, c(2, 2), action_color = "policy", mdp = mdp))
  expect_error(plot_hypergraph(hgf, c(2, 2), action_color = "policy"), "mdp model must be provided")
  expect_invisible(plot_hypergraph(hgf, c(2, 2), actions_visible = "policy", mdp = mdp))
  expect_error(plot_hypergraph(hgf, c(2, 2), actions_visible = "policy"), "mdp model must be provided")
})

test_that("plot_hypergraph supports custom state, action, and action weight labels", {
  hgf <- list(
    nodes = tibble::tibble(
      s_id = c(0, 1),
      g_id = c(1, 3),
      label = c("Head", "Tail"),
      state_label = c("Custom head", "Custom tail")
    ),
    hyperarcs = tibble::tibble(
      s_id = 0,
      action_weights = list(10),
      trans = list(1),
      pr = list(1),
      action_label = "Custom action",
      action_w_label = "Custom weight",
      a_idx = 0,
      label = "A",
      lwd = 1,
      lty = 1,
      col = "black"
    )
  )
  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_invisible(
    plot_hypergraph(
      hgf,
      c(2, 2),
      state_label = "custom",
      action_label = "custom",
      action_w_label = "custom"
    )
  )
  expect_error(
    plot_hypergraph(list(nodes = dplyr::select(hgf$nodes, -state_label), hyperarcs = hgf$hyperarcs), c(2, 2), state_label = "custom"),
    'state_label = "custom" requires a state_label column'
  )
  expect_error(
    plot_hypergraph(list(nodes = hgf$nodes, hyperarcs = dplyr::select(hgf$hyperarcs, -action_label)), c(2, 2), action_label = "custom"),
    'action_label = "custom" requires an action_label column'
  )
  expect_error(
    plot_hypergraph(list(nodes = hgf$nodes, hyperarcs = dplyr::select(hgf$hyperarcs, -action_w_label)), c(2, 2), action_w_label = "custom"),
    'action_w_label = "custom" requires an action_w_label column'
  )
})

test_that("plot_hypergraph supports action_w_label and multiple action weights", {
  hgf <- list(
    nodes = tibble::tibble(s_id = c(0, 1), g_id = c(1, 3), label = c("Head", "Tail")),
    hyperarcs = tibble::tibble(
      s_id = 0,
      action_weights = list(c(1.5, 2.5)), # multiple weights
      trans = list(1),
      pr = list(1),
      trans_weights = list(matrix(10, nrow = 1)),
      a_idx = 0,
      label = "A",
      lwd = 1,
      lty = 1,
      col = "black"
    )
  )
  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_invisible(plot_hypergraph(hgf, c(2, 2), action_w_label = "weight"))
})

test_that("plot_hypergraph filters to states connected forward to selected states", {
  hgf <- list(
    nodes = tibble::tibble(
      s_id = c(0, 1, 2, 3, 4, 5, 6),
      g_id = c(99, 2, 99, 4, 99, 99, 99),
      label = paste0("S", c(0, 1, 2, 3, 4, 5, 6))
    ),
    hyperarcs = tibble::tibble(
      s_id = c(0, 1, 2, 5),
      action_weights = list(numeric(0), numeric(0), numeric(0), numeric(0)),
      trans = list(c(1, 2), 3, 4, 6),
      pr = list(c(0.25, 0.75), 1, 1, 1),
      trans_labels = list(c("keep", "drop"), "target", "other", "disconnected"),
      trans_weights = list(
        matrix(c(10, 20), ncol = 1),
        matrix(30, ncol = 1),
        matrix(40, ncol = 1),
        matrix(50, ncol = 1)
      ),
      a_idx = c(0, 0, 0, 0),
      label = c("A", "B", "C", "D"),
      lwd = c(1, 1, 1, 1),
      lty = c(1, 1, 1, 1),
      col = c("black", "black", "black", "black")
    )
  )
  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_invisible(plot_hypergraph(hgf, c(2, 2), connected_to = 1, trans_labels = "prob"))
  expect_invisible(plot_hypergraph(hgf, c(2, 2), connected_to = 1, trans_labels = "custom"))
  expect_invisible(plot_hypergraph(hgf, c(2, 2), connected_to = 1, trans_labels = "weights"))
  expect_error(
    plot_hypergraph(hgf, c(2, 2), connected_to = 999),
    "connected_to contains s_id values not present in hgf\\$nodes"
  )
})

test_that("plot_hypergraph connected_to uses policy-visible actions", {
  hgf <- list(
    nodes = tibble::tibble(
      s_id = c(0, 1, 2),
      g_id = c(1, 99, 2),
      label = c("S0", "S1", "S2")
    ),
    hyperarcs = tibble::tibble(
      s_id = c(0, 0),
      action_weights = list(numeric(0), numeric(0)),
      trans = list(1, 2),
      pr = list(1, 1),
      a_idx = c(0, 1),
      label = c("to S1", "to S2"),
      lwd = c(1, 1),
      lty = c(1, 1),
      col = c("black", "black")
    )
  )
  testthat::local_mocked_bindings(
    get_policy = function(mdp) tibble::tibble(s_id = 0, a_idx = 1),
    .package = "MDP2"
  )

  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_invisible(plot_hypergraph(hgf, c(1, 2), actions_visible = "policy", connected_to = 0, mdp = list()))
})

test_that("plot_hypergraph recalculates visible grid after connected_to filtering", {
  hgf <- list(
    nodes = tibble::tibble(
      s_id = c(0, 1, 2, 3, 4, 5, 6),
      g_id = c(1, 5, 6, 10, 12, 99, 100),
      label = paste0("S", c(0, 1, 2, 3, 4, 5, 6))
    ),
    hyperarcs = tibble::tibble(
      s_id = c(0, 1, 2, 5),
      action_weights = list(numeric(0), numeric(0), numeric(0), numeric(0)),
      trans = list(c(1, 2), 3, 4, 6),
      pr = list(c(0.5, 0.5), 1, 1, 1),
      a_idx = c(0, 0, 0, 0),
      label = c("A", "B", "C", "D"),
      lwd = c(1, 1, 1, 1),
      lty = c(1, 1, 1, 1),
      col = c("black", "black", "black", "black")
    )
  )
  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_invisible(plot_hypergraph(hgf, c(6, 2), connected_to = 0, recalc_grid = TRUE))
})
