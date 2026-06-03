library(MDP2)

test_that("plotHypergraph draws optional trans labels", {
   hgf <- list(
      nodes = tibble::tibble(
         sId = c(0, 1, 2),
         gId = c(1, 3, 4),
         label = c("Head", "Tail A", "Tail B")
      ),
      hyperarcs = tibble::tibble(
         sId = 0,
         actionWeights = list(10),
         trans = list(c(1, 2)),
         pr = list(c(0.25, 0.75)),
         transLabels = list(c("left", "right")),
         transWeights = list(matrix(c(99, 101), ncol = 1, dimnames = list(NULL, "Reward"))),
         aIdx = 0,
         label = "A",
         lwd = 1,
         lty = 1,
         col = "black"
      )
   )
   grDevices::pdf(file = tempfile(fileext = ".pdf"))
   on.exit(grDevices::dev.off(), add = TRUE)

   expect_invisible(plotHypergraph(hgf, c(2, 2)))
   expect_invisible(plotHypergraph(hgf, c(2, 2), transLabels = "state"))
   expect_invisible(plotHypergraph(hgf, c(2, 2), transLabels = "sId"))
   expect_invisible(plotHypergraph(hgf, c(2, 2), transLabels = "prob"))
   expect_invisible(plotHypergraph(hgf, c(2, 2), transLabels = "weights"))
   expect_invisible(plotHypergraph(hgf, c(2, 2), transLabels = "label|prob|weights"))
   expect_invisible(plotHypergraph(hgf, c(2, 2), transLabels = "custom"))
})

test_that("plotHypergraph ignores missing trans and validates label columns", {
   hgf <- list(
      nodes = tibble::tibble(sId = c(0, 1), gId = c(1, 3), label = c("Head", "Tail")),
      hyperarcs = tibble::tibble(
         sId = 0,
         actionWeights = list(numeric(0)),
         trans = list(c(1, NA_real_)),
         pr = list(c(1, NA_real_)),
         aIdx = 0,
         label = "A",
         lwd = 1,
         lty = 1,
         col = "black"
      )
   )
   grDevices::pdf(file = tempfile(fileext = ".pdf"))
   on.exit(grDevices::dev.off(), add = TRUE)

   expect_invisible(plotHypergraph(hgf, c(2, 2), transLabels = "prob"))
   expect_error(
      plotHypergraph(hgf, c(2, 2), transLabels = "custom"),
      'transLabels = "custom" requires a transLabels list-column'
   )
   expect_error(
      plotHypergraph(hgf, c(2, 2), transLabels = "weights"),
      'transLabels containing "weights" requires a transWeights list-column'
   )
   expect_error(
      plotHypergraph(
         list(nodes = hgf$nodes, hyperarcs = dplyr::select(hgf$hyperarcs, -pr)),
         c(2, 2),
         transLabels = "prob"
      ),
      'transLabels containing "prob" requires a pr list-column'
   )
})

test_that("plotHypergraph supports zero, single, and multiple transition weights", {
   hgf <- list(
      nodes = tibble::tibble(sId = c(0, 1), gId = c(1, 3), label = c("Head", "Tail")),
      hyperarcs = tibble::tibble(
         sId = c(0, 0, 0),
         actionWeights = list(numeric(0), numeric(0), numeric(0)),
         trans = list(1, 1, 1),
         pr = list(1, 1, 1),
         transWeights = list(
            matrix(numeric(0), nrow = 1, ncol = 0),
            matrix(10, nrow = 1),
            matrix(c(10, 20), nrow = 1)
         ),
         aIdx = c(0, 1, 2),
         label = c("A", "B", "C"),
         lwd = c(1, 1, 1),
         lty = c(1, 1, 1),
         col = c("black", "black", "black")
      )
   )
   grDevices::pdf(file = tempfile(fileext = ".pdf"))
   on.exit(grDevices::dev.off(), add = TRUE)

   expect_invisible(plotHypergraph(hgf, c(2, 2), transLabels = "weights"))
})

test_that("getHypergraph returns nested weight and transition columns", {
   prefix <- file.path(tempdir(), "plot_trans_weight_")
   w <- binaryMDPWriter(prefix = prefix, getLog = FALSE)
   w$setWeights("Action weight")
   w$setTransWeights("Transition weight")
   w$process()
      w$stage()
         w$state()
            w$action(
               weights = 5,
               prob = c(1, 0, 0.25, 1, 1, 0.75),
               transWeights = c(10, 20)
            )
            w$endAction()
         w$endState()
      w$endStage()
      w$stage()
         w$state()
         w$endState()
         w$state()
         w$endState()
      w$endStage()
   w$endProcess()
   w$closeWriter()

   mdp <- loadMDP(prefix, getLog = FALSE)
   hgf <- getHypergraph(mdp)

   expect_true(all(c("actionWeights", "trans", "pr", "transWeights") %in% names(hgf$hyperarcs)))
   expect_false(any(grepl("^trans[0-9]+$|^pr[0-9]+$", names(hgf$hyperarcs))))
   expect_equal(hgf$hyperarcs$actionWeights[[1]], 5)
   expect_equal(hgf$hyperarcs$trans[[1]], c(0, 1))
   expect_equal(hgf$hyperarcs$pr[[1]], c(0.25, 0.75))
   expect_equal(
      hgf$hyperarcs$transWeights[[1]],
      matrix(c(10, 20), ncol = 1, dimnames = list(NULL, "Transition weight"))
   )
})

test_that("plotHypergraph supports stateLabel, actionLabel, and actionWLabel options", {
   prefix <- file.path(tempdir(), "plot_opts_")
   w <- binaryMDPWriter(prefix = prefix, getLog = FALSE)
   w$setWeights("Action weight")
   w$setTransWeights("Transition weight")
   w$process()
      w$stage()
         w$state(label = "S1")
            w$action(
               label = "A1",
               weights = 5,
               prob = c(1, 0, 0.25, 1, 1, 0.75),
               transWeights = c(10, 20)
            )
            w$endAction()
         w$endState()
      w$endStage()
      w$stage()
         w$state(label = "S2")
         w$endState()
         w$state(label = "S3")
         w$endState()
      w$endStage()
   w$endProcess()
   w$closeWriter()

   mdp <- loadMDP(prefix, getLog = FALSE)
   hgf <- getHypergraph(mdp)
   hgf$nodes$gId <- c(1, 3, 4)

   grDevices::pdf(file = tempfile(fileext = ".pdf"))
   on.exit(grDevices::dev.off(), add = TRUE)

   expect_invisible(plotHypergraph(hgf, c(2, 2)))
   expect_invisible(plotHypergraph(hgf, c(2, 2), stateLabel = "sId|label", actionLabel = "aIdx|label", actionWLabel = "none"))
   expect_invisible(plotHypergraph(hgf, c(2, 2), stateLabel = "sIdx|label"))
   expect_invisible(plotHypergraph(hgf, c(2, 2), stateLabel = "weight", mdp = mdp))
   expect_invisible(plotHypergraph(hgf, c(2, 2), stateLabel = "label|weight", mdp = mdp))
   expect_invisible(plotHypergraph(hgf, c(2, 2), stateLabel = "sId|weight|sIdx", mdp = mdp))
   expect_invisible(plotHypergraph(hgf, c(2, 2), actionLabel = "label|aIdx"))
   expect_error(plotHypergraph(hgf, c(2, 2), stateLabel = "weight"), "mdp model must be provided")
   expect_error(plotHypergraph(hgf, c(2, 2), stateLabel = "label|weight"), "mdp model must be provided")
   expect_invisible(plotHypergraph(hgf, c(2, 2), actionColor = "label"))
   expect_invisible(plotHypergraph(hgf, c(2, 2), actionColor = "policy", mdp = mdp))
   expect_error(plotHypergraph(hgf, c(2, 2), actionColor = "policy"), "mdp model must be provided")
   expect_invisible(plotHypergraph(hgf, c(2, 2), actionsVisible = "policy", mdp = mdp))
   expect_error(plotHypergraph(hgf, c(2, 2), actionsVisible = "policy"), "mdp model must be provided")
})

test_that("plotHypergraph supports custom state, action, and action weight labels", {
   hgf <- list(
      nodes = tibble::tibble(
         sId = c(0, 1),
         gId = c(1, 3),
         label = c("Head", "Tail"),
         stateLabel = c("Custom head", "Custom tail")
      ),
      hyperarcs = tibble::tibble(
         sId = 0,
         actionWeights = list(10),
         trans = list(1),
         pr = list(1),
         actionLabel = "Custom action",
         actionWLabel = "Custom weight",
         aIdx = 0,
         label = "A",
         lwd = 1,
         lty = 1,
         col = "black"
      )
   )
   grDevices::pdf(file = tempfile(fileext = ".pdf"))
   on.exit(grDevices::dev.off(), add = TRUE)

   expect_invisible(
      plotHypergraph(
         hgf,
         c(2, 2),
         stateLabel = "custom",
         actionLabel = "custom",
         actionWLabel = "custom"
      )
   )
   expect_error(
      plotHypergraph(list(nodes = dplyr::select(hgf$nodes, -stateLabel), hyperarcs = hgf$hyperarcs), c(2, 2), stateLabel = "custom"),
      'stateLabel = "custom" requires a stateLabel column'
   )
   expect_error(
      plotHypergraph(list(nodes = hgf$nodes, hyperarcs = dplyr::select(hgf$hyperarcs, -actionLabel)), c(2, 2), actionLabel = "custom"),
      'actionLabel = "custom" requires an actionLabel column'
   )
   expect_error(
      plotHypergraph(list(nodes = hgf$nodes, hyperarcs = dplyr::select(hgf$hyperarcs, -actionWLabel)), c(2, 2), actionWLabel = "custom"),
      'actionWLabel = "custom" requires an actionWLabel column'
   )
})

test_that("plotHypergraph supports actionWLabel and multiple action weights", {
   hgf <- list(
      nodes = tibble::tibble(sId = c(0, 1), gId = c(1, 3), label = c("Head", "Tail")),
      hyperarcs = tibble::tibble(
         sId = 0,
         actionWeights = list(c(1.5, 2.5)), # multiple weights
         trans = list(1),
         pr = list(1),
         transWeights = list(matrix(10, nrow = 1)),
         aIdx = 0,
         label = "A",
         lwd = 1,
         lty = 1,
         col = "black"
      )
   )
   grDevices::pdf(file = tempfile(fileext = ".pdf"))
   on.exit(grDevices::dev.off(), add = TRUE)

   expect_invisible(plotHypergraph(hgf, c(2, 2), actionWLabel = "weight"))
})

test_that("plotHypergraph filters to states connected forward to selected states", {
   hgf <- list(
      nodes = tibble::tibble(
         sId = c(0, 1, 2, 3, 4, 5, 6),
         gId = c(99, 2, 99, 4, 99, 99, 99),
         label = paste0("S", c(0, 1, 2, 3, 4, 5, 6))
      ),
      hyperarcs = tibble::tibble(
         sId = c(0, 1, 2, 5),
         actionWeights = list(numeric(0), numeric(0), numeric(0), numeric(0)),
         trans = list(c(1, 2), 3, 4, 6),
         pr = list(c(0.25, 0.75), 1, 1, 1),
         transLabels = list(c("keep", "drop"), "target", "other", "disconnected"),
         transWeights = list(
            matrix(c(10, 20), ncol = 1),
            matrix(30, ncol = 1),
            matrix(40, ncol = 1),
            matrix(50, ncol = 1)
         ),
         aIdx = c(0, 0, 0, 0),
         label = c("A", "B", "C", "D"),
         lwd = c(1, 1, 1, 1),
         lty = c(1, 1, 1, 1),
         col = c("black", "black", "black", "black")
      )
   )
   grDevices::pdf(file = tempfile(fileext = ".pdf"))
   on.exit(grDevices::dev.off(), add = TRUE)

   expect_invisible(plotHypergraph(hgf, c(2, 2), connectedTo = 1, transLabels = "prob"))
   expect_invisible(plotHypergraph(hgf, c(2, 2), connectedTo = 1, transLabels = "custom"))
   expect_invisible(plotHypergraph(hgf, c(2, 2), connectedTo = 1, transLabels = "weights"))
   expect_error(
      plotHypergraph(hgf, c(2, 2), connectedTo = 999),
      "connectedTo contains sId values not present in hgf\\$nodes"
   )
})

test_that("plotHypergraph connectedTo uses policy-visible actions", {
   hgf <- list(
      nodes = tibble::tibble(
         sId = c(0, 1, 2),
         gId = c(1, 99, 2),
         label = c("S0", "S1", "S2")
      ),
      hyperarcs = tibble::tibble(
         sId = c(0, 0),
         actionWeights = list(numeric(0), numeric(0)),
         trans = list(1, 2),
         pr = list(1, 1),
         aIdx = c(0, 1),
         label = c("to S1", "to S2"),
         lwd = c(1, 1),
         lty = c(1, 1),
         col = c("black", "black")
      )
   )
   testthat::local_mocked_bindings(
      getPolicy = function(mdp) tibble::tibble(sId = 0, aIdx = 1),
      .package = "MDP2"
   )

   grDevices::pdf(file = tempfile(fileext = ".pdf"))
   on.exit(grDevices::dev.off(), add = TRUE)

   expect_invisible(plotHypergraph(hgf, c(1, 2), actionsVisible = "policy", connectedTo = 0, mdp = list()))
})

test_that("plotHypergraph recalculates visible grid after connectedTo filtering", {
   hgf <- list(
      nodes = tibble::tibble(
         sId = c(0, 1, 2, 3, 4, 5, 6),
         gId = c(1, 5, 6, 10, 12, 99, 100),
         label = paste0("S", c(0, 1, 2, 3, 4, 5, 6))
      ),
      hyperarcs = tibble::tibble(
         sId = c(0, 1, 2, 5),
         actionWeights = list(numeric(0), numeric(0), numeric(0), numeric(0)),
         trans = list(c(1, 2), 3, 4, 6),
         pr = list(c(0.5, 0.5), 1, 1, 1),
         aIdx = c(0, 0, 0, 0),
         label = c("A", "B", "C", "D"),
         lwd = c(1, 1, 1, 1),
         lty = c(1, 1, 1, 1),
         col = c("black", "black", "black", "black")
      )
   )
   grDevices::pdf(file = tempfile(fileext = ".pdf"))
   on.exit(grDevices::dev.off(), add = TRUE)

   expect_invisible(plotHypergraph(hgf, c(6, 2), connectedTo = 0, recalcGrid = TRUE))
})
