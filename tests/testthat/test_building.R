library(MDP2)
cleanUp<-function() {
   unlink("*.bin")
   unlink("*.hmp")
}

test_that("binaryMDPWriter",{
   #sprintf("%.100f",g)
   source("files/HCT_ex6.1.1.R")
   mdp<-loadMDP("hct611_", getLog = FALSE)
   gProb<-runPolicyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   source("files/HCT_ex6.1.1_v2.R")
   mdp<-loadMDP("hct611v2_", getLog = FALSE)
   gPr<-runPolicyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   expect_equal(gProb, gPr)
   
   source("files/HCT_exc6.4.R")
   mdp<-loadMDP("hct64_", getLog = FALSE)
   gProb<-runPolicyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   source("files/HCT_exc6.4_v2.R")
   mdp<-loadMDP("hct64v2_", getLog = FALSE)
   gPr<-runPolicyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   expect_equal(gProb, gPr)
   
   source("files/HCT_exc6.7.R")
   mdp<-loadMDP("hct67_", getLog = FALSE)
   gProb<-runPolicyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   source("files/HCT_exc6.7_v2.R")
   mdp<-loadMDP("hct67v2_", getLog = FALSE)
   gPr<-runPolicyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   expect_equal(gProb, gPr)
   
   source("files/HCT_exc7.3.R")
   mdp<-loadMDP("hct73_", getLog = FALSE)
   gProb<-runPolicyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   source("files/HCT_exc7.3_v2.R")
   mdp<-loadMDP("hct73v2_", getLog = FALSE)
   gPr<-runPolicyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   expect_equal(gProb, gPr)
   
   ## test loading with P, R and D
   source("files/HCT_ex6.1.1_v3.R")
   # test using L.R.A reward / t.unit
   mdp<-loadMDP("hct611v3-1_", getLog = FALSE)
   expect_equal(runPolicyIteAve(mdp,"Net reward","Duration", getLog = FALSE), -0.433789954337899297254210750907077454030513763427734375)
   rm(mdp)
   mdp<-loadMDP("hct611v3-2_", getLog = FALSE)
   expect_equal(runPolicyIteAve(mdp,"Net reward","Duration", getLog = FALSE), -0.433789954337899297254210750907077454030513763427734375)
   rm(mdp)
   mdp<-loadMDP("hct611v3-3_", getLog = FALSE)
   expect_equal(runPolicyIteAve(mdp,"Net reward","Duration", getLog = FALSE), -0.433789954337899297254210750907077454030513763427734375)
   rm(mdp)
   mdp<-loadMDP("hct611v3-4_", getLog = FALSE)
   expect_equal(runPolicyIteAve(mdp,"Net reward","Duration", getLog = FALSE), -0.433789954337899297254210750907077454030513763427734375)
   rm(mdp)
   # test using discounted reward
   # compare MDP formulations
   mdp<-loadMDP("hct611v3-1_", getLog = FALSE)
   runPolicyIteDiscount(mdp, "Net reward", "Duration", discountFactor = 0.9, getLog = FALSE)
   weightsPolicyIte<-getPolicy(mdp)$weight
   expect_equal(sum(weightsPolicyIte), -37.94871237074722358784129028208553791046142578125)
   rm(mdp)
   mdp<-loadMDP("hct611v3-2_", getLog = FALSE)
   runPolicyIteDiscount(mdp, "Net reward", "Duration", discountFactor = 0.9, getLog = FALSE)
   weightsPolicyIte<-getPolicy(mdp)$weight
   expect_equal(sum(weightsPolicyIte), -37.94871237074722358784129028208553791046142578125)
   rm(mdp)
   mdp<-loadMDP("hct611v3-3_", getLog = FALSE)
   runPolicyIteDiscount(mdp, "Net reward", "Duration", discountFactor = 0.9, getLog = FALSE)
   weightsPolicyIte<-getPolicy(mdp)$weight
   expect_equal(sum(weightsPolicyIte), -37.94871237074722358784129028208553791046142578125)
   rm(mdp)
   # compare semi-MDP formulations
   mdp<-loadMDP("hct611v2_", getLog = FALSE)
   runPolicyIteDiscount(mdp, "Net reward", "Duration", discountFactor = 0.9, getLog = FALSE)
   weightsPolicyIte1<-getPolicy(mdp)$weight
   rm(mdp)
   mdp<-loadMDP("hct611v3-4_", getLog = FALSE)
   runPolicyIteDiscount(mdp, "Net reward", "Duration", discountFactor = 0.9, getLog = FALSE)
   weightsPolicyIte2<-getPolicy(mdp)$weight
   rm(mdp)
   expect_equal(weightsPolicyIte1, weightsPolicyIte2)
})

test_that("loadMDP supports models with states and no actions",{
   prefix <- paste0(tempfile("no_actions_"), "_")
   w <- binaryMDPWriter(prefix, getLog = FALSE)
   w$setWeights(c("Duration", "Net reward"))
   w$process()
      w$stage()
         for (ii in 2:10) {
            w$state(label = "test")
            w$endState()
         }
      w$endStage()
   w$endProcess()
   w$closeWriter()

   mdp <- loadMDP(prefix, getLog = FALSE)

   expect_s3_class(mdp, "HMDP")
   expect_equal(mdp$states, 9)
   expect_equal(mdp$actions, 0)
   expect_equal(mdp$weightNames, c("Duration", "Net reward"))
})

test_that("loadMDP rejects transitions to non-existing states",{
   prefix <- paste0(tempfile("bad_transition_"), "_")
   w <- binaryMDPWriter(prefix, getLog = FALSE)
   w$setWeights(c("Duration", "Net reward"))
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
            w$endState()
         }
      w$endStage()
   w$endProcess()
   w$closeWriter()

   expect_message(
      mdp <- loadMDP(prefix, getLog = FALSE),
      "transition to a non-existing state"
   )
   expect_null(mdp)
})

test_that("binaryMDPWriter detects unclosed writer blocks",{
   prefix <- paste0(tempfile("writer_stack_"), "_")
   w <- binaryMDPWriter(prefix, getLog = FALSE)
   w$setWeights(c("Duration", "Net reward"))
   w$process()
   w$stage()
   w$state()
   w$action(scope = 1, weights = c(1, 100), id = 0, pr = 1)

   expect_error(
      w$endState(),
      "Call endAction\\(\\) or use action\\(\\.\\.\\., end = TRUE\\)"
   )
   expect_error(
      w$closeWriter(),
      "action is still open"
   )

   w$endAction()
   expect_error(
      w$endStage(),
      "Cannot end a stage unless a stage is open"
   )
   expect_error(
      w$closeWriter(),
      "state is still open"
   )

   w$endState()
   w$endStage()
   w$endProcess()
   w$closeWriter()
})

test_that("binaryMDPWriter requires correct block nesting",{
   prefix <- paste0(tempfile("writer_nesting_"), "_")
   w <- binaryMDPWriter(prefix, getLog = FALSE)
   w$setWeights(c("Duration", "Net reward"))

   expect_error(w$stage(), "outside an open process")
   expect_error(w$state(), "outside an open stage")
   expect_error(
      w$action(scope = 1, weights = c(1, 100), id = 0, pr = 1),
      "outside an open state"
   )
   expect_error(w$endAction(), "unless an action is open")
   expect_error(w$endState(), "while another writer block is open")
   expect_error(w$endStage(), "unless a stage is open")
   expect_error(w$endProcess(), "unless a process is open")

   w$process()
   expect_error(w$process(), "before closing the current writer block")
   w$endProcess()
   w$closeWriter()
})

test_that("binaryMDPWriter allows one redundant endAction after end equals TRUE",{
   prefix <- paste0(tempfile("writer_redundant_end_"), "_")
   w <- binaryMDPWriter(prefix, getLog = FALSE)
   w$setWeights(c("Duration", "Net reward"))
   w$process()
   w$stage()
   w$state()
   w$action(scope = 1, weights = c(1, 100), id = 0, pr = 1, end = TRUE)
   expect_no_error(w$endAction())
   expect_error(w$endAction(), "unless an action is open")
   w$endState()
   w$endStage()
   w$endProcess()
   w$closeWriter()
})

test_that("binaryMDPWriter allows nested process inside an action",{
   prefix <- paste0(tempfile("writer_nested_"), "_")
   w <- binaryMDPWriter(prefix, getLog = FALSE)
   w$setWeights(c("Duration", "Net reward"))
   w$process()
      w$stage()
         w$state()
            w$action(weights = c(0, 0), prob = c(2, 0, 1))
               expect_no_error(w$process())
                  w$stage()
                     w$state()
                        w$action(weights = c(0, 0), prob = c(1, 0, 1))
                        w$endAction()
                     w$endState()
                  w$endStage()
               w$endProcess()
            w$endAction()
         w$endState()
      w$endStage()
   w$endProcess()
   w$closeWriter()
})

test_that("memoryMDPWriter builds the same model as binaryMDPWriter",{
   build_model <- function(w) {
      w$setWeights(c("Duration", "Net reward"))
      w$process()
         w$stage()
            w$state(label = "s0")
               w$action(label = "a0", scope = c(1, 1), id = c(0, 1),
                        pr = c(0.25, 0.75), weights = c(1, 10), end = TRUE)
            w$endState()
            w$state(label = "s1")
               w$action(label = "a1", scope = c(1, 1), id = c(0, 1),
                        pr = c(0.5, 0.5), weights = c(1, 20), end = TRUE)
            w$endState()
         w$endStage()
         w$stage()
            w$state(label = "t0", end = TRUE)
            w$state(label = "t1", end = TRUE)
         w$endStage()
      w$endProcess()
      w
   }

   prefix <- paste0(tempfile("memory_compare_"), "_")
   wb <- build_model(binaryMDPWriter(prefix, getLog = FALSE))
   wb$closeWriter()
   binary_mdp <- loadMDP(prefix, getLog = FALSE)

   wm <- build_model(memoryMDPWriter(getLog = FALSE))
   memory_mdp <- wm$closeWriter()

   expect_s3_class(memory_mdp, "HMDP")
   expect_equal(memory_mdp$states, binary_mdp$states)
   expect_equal(memory_mdp$actions, binary_mdp$actions)
   expect_equal(memory_mdp$weightNames, binary_mdp$weightNames)
   memory_ids <- memory_mdp$ptr$getIds("0")
   binary_ids <- binary_mdp$ptr$getIds("0")
   expect_equal(memory_mdp$ptr$getActionInfo(memory_ids[1]), binary_mdp$ptr$getActionInfo(binary_ids[1]))
   expect_equal(memory_mdp$ptr$getActionInfo(memory_ids[2]), binary_mdp$ptr$getActionInfo(binary_ids[2]))
})

test_that("memoryMDPWriter supports transition weights",{
   w <- memoryMDPWriter(getLog = FALSE)
   w$setWeights(c("Duration"))
   w$setTransWeights(c("Reward", "Risk"))
   w$process()
      w$stage()
         w$state(label = "s0")
            w$action(
               label = "a0",
               scope = c(1, 1),
               id = c(0, 1),
               pr = c(0.5, 0.5),
               weights = 1,
               transWeights = c(10, 1, 20, 2),
               end = TRUE
            )
         w$endState()
      w$endStage()
      w$stage()
         w$state(label = "t0", end = TRUE)
         w$state(label = "t1", end = TRUE)
      w$endStage()
   w$endProcess()

   mdp <- w$closeWriter()
   info <- mdp$ptr$getActionInfo(mdp$ptr$getIds("0")[1])[[1]]

   expect_equal(mdp$weightActionNames, "Duration")
   expect_equal(mdp$weightTransNames, c("Reward", "Risk"))
   expect_equal(info$transWeights, c(10, 1, 20, 2))
})

test_that("memoryMDPWriter supports models with states and no actions",{
   w <- memoryMDPWriter(getLog = FALSE)
   w$setWeights(c("Duration", "Net reward"))
   w$process()
      w$stage()
         for (ii in 1:4) w$state(label = paste0("s", ii), end = TRUE)
      w$endStage()
   w$endProcess()

   mdp <- w$closeWriter()

   expect_s3_class(mdp, "HMDP")
   expect_equal(mdp$states, 4)
   expect_equal(mdp$actions, 0)
   expect_equal(mdp$weightNames, c("Duration", "Net reward"))
})

test_that("memoryMDPWriter supports nested processes",{
   expect_no_error(source(system.file("examples/memoryMDPWriter-ex.R", package = "MDP2")))
})

test_that("memoryMDPWriter rejects external processes and use after close",{
   w <- memoryMDPWriter(getLog = FALSE)
   w$setWeights(c("Duration", "Net reward"))
   expect_error(w$includeProcess(), "does not support external processes")
   w$process()
      w$stage()
         w$state(label = "s0", end = TRUE)
      w$endStage()
   w$endProcess()
   mdp <- w$closeWriter()

   expect_s3_class(mdp, "HMDP")
   expect_error(w$state(), "memoryMDPWriter is closed")
   expect_error(w$closeWriter(), "memoryMDPWriter is closed")
})

build_with_writer <- function(build_model) {
   prefix <- paste0(tempfile("writer_compare_"), "_")
   wb <- build_model(binaryMDPWriter(prefix, getLog = FALSE))
   wb$closeWriter()
   binary_mdp <- loadMDP(prefix, getLog = FALSE)

   wm <- build_model(memoryMDPWriter(getLog = FALSE))
   memory_mdp <- wm$closeWriter()

   list(binary = binary_mdp, memory = memory_mdp)
}

expect_same_model_summary <- function(models) {
   expect_s3_class(models$memory, "HMDP")
   expect_equal(models$memory$states, models$binary$states)
   expect_equal(models$memory$actions, models$binary$actions)
   expect_equal(models$memory$levels, models$binary$levels)
   expect_equal(models$memory$weightNames, models$binary$weightNames)
   expect_equal(models$memory$weightActionNames, models$binary$weightActionNames)
   expect_equal(models$memory$weightTransNames, models$binary$weightTransNames)
}

build_vignette_hct_hierarchical <- function(w) {
   N <- 5
   labels <- paste0("i = ", 1:N)
   Cf <- -10
   Cp <- c(0, -7, -7, -5)
   Q <- matrix(c(
      0.90, 0.10, 0, 0, 0,
      0, 0.80, 0.10, 0.05, 0.05,
      0, 0, 0.70, 0.10, 0.20,
      0, 0, 0, 0.50, 0.50),
      nrow = 4, byrow = TRUE)
   transPr <- function(i, a) {
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

   w$setWeights(c("Duration", "Net reward"))
   w$process()
      w$stage()
         w$state(label = labels[1])
            lst <- transPr(1, "nr")
            w$action(label = "nr", weights = c(1, 0), pr = lst$pr, id = lst$idx, end = TRUE)
         w$endState()
         for (i in 2:(N - 1)) {
            w$state(label = labels[i])
               lst <- transPr(i, "nr")
               w$action(label = "nr", weights = c(1, 0), pr = lst$pr, id = lst$idx, end = TRUE)
               lst <- transPr(i, "pr")
               w$action(label = "pr", weights = c(1, Cp[i]), pr = lst$pr, id = lst$idx, end = TRUE)
            w$endState()
         }
         w$state(label = labels[N])
            lst <- transPr(N, "fr")
            w$action(label = "fr", weights = c(2, Cf), pr = lst$pr, id = lst$idx, end = TRUE)
         w$endState()
      w$endStage()
   w$endProcess()
   w
}

build_vignette_hct_matrices <- function(w) {
   N <- 5
   Cf <- -10
   Cp <- c(0, -7, -7, -5)
   Q <- matrix(c(
      0.90, 0.10, 0, 0, 0,
      0, 0.80, 0.10, 0.05, 0.05,
      0, 0, 0.70, 0.10, 0.20,
      0, 0, 0, 0.50, 0.50),
      nrow = 4, byrow = TRUE)

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

   w$setWeights(c("Duration", "Net reward"))
   w$process(P, R, D)
   w
}

build_vignette_machine <- function(w) {
   w$setWeights(c("Net reward"))
   w$process()
      w$stage()
         w$state(label = "dummy")
            w$action(label = "buy", weights = -100, pr = c(0.7, 0.3), id = c(0, 1), end = TRUE)
         w$endState()
      w$endStage()
      w$stage()
         w$state(label = "good")
            w$action(label = "mt", weights = 55, pr = 1, id = 0, end = TRUE)
            w$action(label = "nmt", weights = 70, pr = c(0.6, 0.4), id = c(0, 1), end = TRUE)
         w$endState()
         w$state(label = "average")
            w$action(label = "mt", weights = 40, pr = 1, id = 0, end = TRUE)
            w$action(label = "nmt", weights = 50, pr = c(0.6, 0.4), id = c(1, 2), end = TRUE)
         w$endState()
      w$endStage()
      w$stage()
         w$state(label = "good")
            w$action(label = "mt", weights = 55, pr = 1, id = 0, end = TRUE)
            w$action(label = "nmt", weights = 70, pr = c(0.5, 0.5), id = c(0, 1), end = TRUE)
         w$endState()
         w$state(label = "average")
            w$action(label = "mt", weights = 40, pr = 1, id = 0, end = TRUE)
            w$action(label = "nmt", weights = 50, pr = c(0.5, 0.5), id = c(1, 2), end = TRUE)
         w$endState()
         w$state(label = "not working")
            w$action(label = "mt", weights = 30, pr = 1, id = 0, end = TRUE)
            w$action(label = "rep", weights = 5, pr = 1, id = 3, end = TRUE)
         w$endState()
      w$endStage()
      w$stage()
         w$state(label = "good")
            w$action(label = "mt", weights = 55, pr = 1, id = 0, end = TRUE)
            w$action(label = "nmt", weights = 70, pr = c(0.2, 0.8), id = c(0, 1), end = TRUE)
         w$endState()
         w$state(label = "average")
            w$action(label = "mt", weights = 40, pr = 1, id = 0, end = TRUE)
            w$action(label = "nmt", weights = 50, pr = c(0.2, 0.8), id = c(1, 2), end = TRUE)
         w$endState()
         w$state(label = "not working")
            w$action(label = "mt", weights = 30, pr = 1, id = 0, end = TRUE)
            w$action(label = "rep", weights = 5, pr = 1, id = 3, end = TRUE)
         w$endState()
         w$state(label = "replaced")
            w$action(label = "dummy", weights = 0, pr = 1, id = 3, end = TRUE)
         w$endState()
      w$endStage()
      w$stage()
         w$state(label = "good", end = TRUE)
         w$state(label = "average", end = TRUE)
         w$state(label = "not working", end = TRUE)
         w$state(label = "replaced", end = TRUE)
      w$endStage()
   w$endProcess()
   w
}

build_vignette_cow <- function(w) {
   cowFile <- c(
      file.path("files", "cow.csv"),
      file.path("tests", "testthat", "files", "cow.csv"),
      file.path("vignettes", "vignette_files", "cow.csv"),
      file.path("..", "..", "vignettes", "vignette_files", "cow.csv"),
      file.path("..", "vignettes", "vignette_files", "cow.csv")
   )
   cowFile <- cowFile[file.exists(cowFile)][1]
   if (is.na(cowFile)) stop("Could not find cow.csv test fixture.", call. = FALSE)
   cowDf <- utils::read.csv(cowFile)
   lev1W <- function(s0Idx, n1Idx, s1Idx, a1Lbl) {
      row <- cowDf[cowDf$s0 == s0Idx & cowDf$n1 == n1Idx & cowDf$s1 == s1Idx & cowDf$label == a1Lbl, ]
      as.numeric(row[c("Duration", "Reward", "Output")])
   }
   lev1Pr <- function(s0Idx, n1Idx, s1Idx, a1Lbl) {
      row <- cowDf[cowDf$s0 == s0Idx & cowDf$n1 == n1Idx & cowDf$s1 == s1Idx & cowDf$label == a1Lbl, ]
      as.numeric(row[paste0(rep(c("scp", "idx", "pr"), 3), rep(0:2, each = 3))])
   }

   lblS0 <- c("Bad genetic level", "Avg genetic level", "Good genetic level")
   lblS1 <- c("Low yield", "Avg yield", "High yield")
   w$setWeights(c("Duration", "Net reward", "Yield"))
   w$process()
      w$stage()
         for (s0 in 0:2) {
            w$state(label = lblS0[s0 + 1])
               w$action(label = "Keep", weights = c(0, 0, 0), prob = c(2, 0, 1))
                  w$process()
                     w$stage()
                        w$state(label = "Dummy")
                           w$action(label = "Dummy", weights = c(0, 0, 0),
                                    prob = c(1, 0, 1/3, 1, 1, 1/3, 1, 2, 1/3), end = TRUE)
                        w$endState()
                     w$endStage()
                     for (d1 in 1:4) {
                        w$stage()
                           for (s1 in 0:2) {
                              w$state(label = lblS1[s1 + 1])
                                 if (d1 != 4) {
                                    w$action(label = "Keep", weights = lev1W(s0, d1, s1, "Keep"),
                                             prob = lev1Pr(s0, d1, s1, "Keep"), end = TRUE)
                                 }
                                 w$action(label = "Replace", weights = lev1W(s0, d1, s1, "Replace"),
                                          prob = lev1Pr(s0, d1, s1, "Replace"), end = TRUE)
                              w$endState()
                           }
                        w$endStage()
                     }
                  w$endProcess()
               w$endAction()
            w$endState()
         }
      w$endStage()
   w$endProcess()
   w
}

test_that("building vignette HCT hierarchical model matches for binary and memory writers",{
   models <- build_with_writer(build_vignette_hct_hierarchical)
   expect_same_model_summary(models)
   expect_equal(
      runPolicyIteAve(models$memory, "Net reward", "Duration", getLog = FALSE),
      runPolicyIteAve(models$binary, "Net reward", "Duration", getLog = FALSE)
   )
})

test_that("building vignette HCT matrix model matches for binary and memory writers",{
   models <- build_with_writer(build_vignette_hct_matrices)
   expect_same_model_summary(models)
   expect_equal(
      runPolicyIteAve(models$memory, "Net reward", "Duration", getLog = FALSE),
      runPolicyIteAve(models$binary, "Net reward", "Duration", getLog = FALSE)
   )
})

test_that("building vignette machine model matches for binary and memory writers",{
   models <- build_with_writer(build_vignette_machine)
   expect_same_model_summary(models)
   runValueIte(models$binary, "Net reward", termValues = c(30, 10, 5, 0), getLog = FALSE)
   binary_policy <- getPolicy(models$binary)
   runValueIte(models$memory, "Net reward", termValues = c(30, 10, 5, 0), getLog = FALSE)
   memory_policy <- getPolicy(models$memory)
   expect_equal(memory_policy$weight, binary_policy$weight)
   expect_equal(memory_policy$actionLabel, binary_policy$actionLabel)
})

test_that("building vignette cow model matches for binary and memory writers",{
   models <- build_with_writer(build_vignette_cow)
   expect_same_model_summary(models)
   expect_equal(
      runPolicyIteAve(models$memory, "Net reward", "Duration", getLog = FALSE),
      runPolicyIteAve(models$binary, "Net reward", "Duration", getLog = FALSE)
   )
   expect_equal(
      runPolicyIteAve(models$memory, "Yield", "Duration", getLog = FALSE),
      runPolicyIteAve(models$binary, "Yield", "Duration", getLog = FALSE)
   )
})

cleanUp()
