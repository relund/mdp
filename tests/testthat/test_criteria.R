library(MDP2)
cleanUp<-function() unlink("*.bin")


test_that("Total reward",{
   source("files/machine_replacement_v1.R")
   mdp<-loadMDP("machine1_", getLog = FALSE)
   w<-"Net reward"             # label of the weight we want to optimize
   scrapValues<-c(30,10,5,0)   # scrap values (the values of the 4 states at stage 4)
   runValueIte(mdp, w, termValues=scrapValues, getLog = FALSE)
   expect_equal(getPolicy(mdp,13)$weight,102.2)
   rm(mdp)
   
   # same model with a single dummy node
   source("files/machine_replacement_v2.R")
   mdp<-loadMDP("machine2_", getLog = FALSE)
   w<-"Net reward"             # label of the weight we want to optimize
   runValueIte(mdp, w, termValues=0, getLog = FALSE)
   expect_equal(getPolicy(mdp,12)$weight,102.2)
   rm(mdp)
})

test_that("Transition-level total reward",{
   w <- binaryMDPWriter(prefix = "trans_reward_", getLog = FALSE)
   w$setWeights(character())
   w$setTransWeights("Transition reward")
   w$process()
      w$stage()
         w$state()
            w$action(weights = numeric(0),
                     prob = c(1, 0, 0.25, 1, 1, 0.75),
                     transWeights = c(10, 20))
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

   mdp <- loadMDP("trans_reward_", getLog = FALSE)
   expect_equal(mdp$weightActionNames, character())
   expect_equal(mdp$weightTransNames, "Transition reward")
   expect_error(
      mdp$ptr$valueIte(0, 0, 1L, 0, 0L, 0L, c(0, 0), 0, 1),
      "Transition-level weights are not supported for BellmanOp::Discounted"
   )
   runValueIte(mdp, "Transition reward", termValues = c(100, 200), getLog = FALSE)
   policy <- getPolicy(mdp)
   expect_equal(policy$weight[policy$stateStr == "0,0"], 192.5)
   rm(mdp)
})

test_that("Global weight lookup rejects ambiguous names", {
   mdp <- list(weightNames = c("Net", "Net reward"))
   expect_equal(getWIdx(mdp, "Net"), 0)
   expect_error(getWIdx(mdp, "e"), "ambiguous")
})

test_that("Value iteration supports minimization objective", {
   w <- binaryMDPWriter(prefix = "sense_", getLog = FALSE)
   w$setWeights("Cost")
   w$process()
      w$stage()
         w$state()
            w$action(weights = 10, prob = c(1, 0, 1), end = TRUE)
            w$endAction()
            w$action(weights = 1, prob = c(1, 0, 1), end = TRUE)
            w$endAction()
         w$endState()
      w$endStage()
      w$stage()
         w$state()
         w$endState()
      w$endStage()
   w$endProcess()
   w$closeWriter()

   mdp <- loadMDP("sense_", getLog = FALSE)
   runValueIte(mdp, "Cost", termValues = 0, objective = "max", getLog = FALSE)
   policy <- getPolicy(mdp)
   expect_equal(policy$aIdx[policy$stateStr == "0,0"], 0)
   expect_equal(policy$weight[policy$stateStr == "0,0"], 10)
   expect_equal(getRPO(mdp, "Cost", iA = 0, sId = 1, objective = "max")$rpo, 9)

   runValueIte(mdp, "Cost", termValues = 0, objective = "min", getLog = FALSE)
   policy <- getPolicy(mdp)
   expect_equal(policy$aIdx[policy$stateStr == "0,0"], 1)
   expect_equal(policy$weight[policy$stateStr == "0,0"], 1)
   expect_equal(getRPO(mdp, "Cost", iA = 1, sId = 1, objective = "min")$rpo, 9)
})

test_that("Value iteration supports minimum and maximum successor Bellman operators", {
   w <- binaryMDPWriter(prefix = "minmax_", getLog = FALSE)
   w$setWeights("Weight")
   w$setTransWeights("Trans weight")
   w$process()
      w$stage()
         w$state()
            w$action(weights = 0,
                     prob = c(1, 0, 0.5, 1, 1, 0.5),
                     transWeights = c(100, 0))
            w$endAction()
            w$action(weights = 5,
                     prob = c(1, 0, 1),
                     transWeights = 5)
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

   mdp <- loadMDP("minmax_", getLog = FALSE)

   runValueIte(mdp, "Weight", termValues = c(1, 10), bellmanOp = "min", objective = "max", getLog = FALSE)
   policy <- getPolicy(mdp)
   expect_equal(policy$aIdx[policy$stateStr == "0,0"], 1)
   expect_equal(policy$weight[policy$stateStr == "0,0"], 6)

   runValueIte(mdp, "Weight", termValues = c(1, 10), bellmanOp = "max", objective = "max", getLog = FALSE)
   policy <- getPolicy(mdp)
   expect_equal(policy$aIdx[policy$stateStr == "0,0"], 0)
   expect_equal(policy$weight[policy$stateStr == "0,0"], 10)

   runValueIte(mdp, "Weight", termValues = c(1, 10), bellmanOp = "min", objective = "min", getLog = FALSE)
   policy <- getPolicy(mdp)
   expect_equal(policy$aIdx[policy$stateStr == "0,0"], 0)
   expect_equal(policy$weight[policy$stateStr == "0,0"], 1)

   runValueIte(mdp, "Weight", termValues = c(1, 10), bellmanOp = "max", objective = "min", getLog = FALSE)
   policy <- getPolicy(mdp)
   expect_equal(policy$aIdx[policy$stateStr == "0,0"], 1)
   expect_equal(policy$weight[policy$stateStr == "0,0"], 6)

   runValueIte(mdp, "Trans weight", termValues = c(1, 10), bellmanOp = "min", objective = "max", getLog = FALSE)
   policy <- getPolicy(mdp)
   expect_equal(policy$aIdx[policy$stateStr == "0,0"], 0)
   expect_equal(policy$weight[policy$stateStr == "0,0"], 10)

   runValueIte(mdp, "Trans weight", termValues = c(1, 10), bellmanOp = "max", objective = "max", getLog = FALSE)
   policy <- getPolicy(mdp)
   expect_equal(policy$aIdx[policy$stateStr == "0,0"], 0)
   expect_equal(policy$weight[policy$stateStr == "0,0"], 101)
})

test_that("SecondMoment supports action-level weights", {
   w <- binaryMDPWriter(prefix = "second_moment_action_", getLog = FALSE)
   w$setWeights("Weight")
   w$process()
      w$stage()
         w$state()
            w$action(label = "high", weights = 2, prob = c(1, 0, 1), end = TRUE)
            w$endAction()
            w$action(label = "low", weights = 1, prob = c(1, 1, 1), end = TRUE)
            w$endAction()
         w$endState()
      w$endStage()
      w$stage()
         w$state()
            w$action(weights = 3, prob = c(1, 0, 1), end = TRUE)
            w$endAction()
         w$endState()
         w$state()
            w$action(weights = 0, prob = c(1, 0, 1), end = TRUE)
            w$endAction()
         w$endState()
      w$endStage()
      w$stage()
         w$state()
         w$endState()
      w$endStage()
   w$endProcess()
   w$closeWriter()

   mdp <- loadMDP("second_moment_action_", getLog = FALSE)

   runValueIte(mdp, "Weight", termValues = 0, getLog = FALSE)
   runCalcWeights(mdp, "Weight", criterion = "secondMoment", termValues = 0)
   policy <- getPolicy(mdp)
   expect_equal(policy$weight[policy$stateStr == "0,0"], 25)

   runValueIte(mdp, "Weight", termValues = 0, bellmanOp = "secondMoment", objective = "max", getLog = FALSE)
   policy <- getPolicy(mdp)
   expect_equal(policy$aIdx[policy$stateStr == "0,0"], 0)
   expect_equal(policy$weight[policy$stateStr == "0,0"], 25)

   runValueIte(mdp, "Weight", termValues = 0, bellmanOp = "secondMoment", objective = "min", getLog = FALSE)
   policy <- getPolicy(mdp)
   expect_equal(policy$aIdx[policy$stateStr == "0,0"], 1)
   expect_equal(policy$weight[policy$stateStr == "0,0"], 1)
})

test_that("SecondMoment supports transition-level weights", {
   w <- binaryMDPWriter(prefix = "second_moment_transition_", getLog = FALSE)
   w$setWeights(character())
   w$setTransWeights("Weight")
   w$process()
      w$stage()
         w$state()
            w$action(label = "high", weights = numeric(0), prob = c(1, 0, 1),
                     transWeights = 2, end = TRUE)
            w$endAction()
            w$action(label = "low", weights = numeric(0), prob = c(1, 1, 1),
                     transWeights = 1, end = TRUE)
            w$endAction()
         w$endState()
      w$endStage()
      w$stage()
         w$state()
            w$action(weights = numeric(0), prob = c(1, 0, 1),
                     transWeights = 3, end = TRUE)
            w$endAction()
         w$endState()
         w$state()
            w$action(weights = numeric(0), prob = c(1, 0, 1),
                     transWeights = 0, end = TRUE)
            w$endAction()
         w$endState()
      w$endStage()
      w$stage()
         w$state()
         w$endState()
      w$endStage()
   w$endProcess()
   w$closeWriter()

   mdp <- loadMDP("second_moment_transition_", getLog = FALSE)

   runValueIte(mdp, "Weight", termValues = 0, getLog = FALSE)
   runCalcWeights(mdp, "Weight", criterion = "secondMoment", termValues = 0)
   policy <- getPolicy(mdp)
   expect_equal(policy$weight[policy$stateStr == "0,0"], 25)

   runValueIte(mdp, "Weight", termValues = 0, bellmanOp = "secondMoment", objective = "max", getLog = FALSE)
   policy <- getPolicy(mdp)
   expect_equal(policy$aIdx[policy$stateStr == "0,0"], 0)
   expect_equal(policy$weight[policy$stateStr == "0,0"], 25)

   runValueIte(mdp, "Weight", termValues = 0, bellmanOp = "secondMoment", objective = "min", getLog = FALSE)
   policy <- getPolicy(mdp)
   expect_equal(policy$aIdx[policy$stateStr == "0,0"], 1)
   expect_equal(policy$weight[policy$stateStr == "0,0"], 1)
})

test_that("SecondMoment value iteration is finite-horizon only", {
   source("files/two_level_hmdp.R")
   mdp <- loadMDP("2lev_", getLog = FALSE)
   runValueIte(mdp, "Net reward", "Duration", bellmanOp = "secondMoment", termValues = rep(0, mdp$founderStatesLast), getLog = FALSE)
   expect_match(mdp$ptr$getLog(), "SecondMoment value iteration is currently only supported for finite time-horizon HMDPs")
})

test_that("Variance supports action-level fixed-policy evaluation", {
   w <- binaryMDPWriter(prefix = "variance_action_", getLog = FALSE)
   w$setWeights("Weight")
   w$process()
      w$stage()
         w$state()
            w$action(weights = 1, prob = c(1, 0, 0.5, 1, 1, 0.5), end = TRUE)
            w$endAction()
         w$endState()
      w$endStage()
      w$stage()
         w$state()
            w$action(weights = 0, prob = c(1, 0, 1), end = TRUE)
            w$endAction()
         w$endState()
         w$state()
            w$action(weights = 2, prob = c(1, 0, 1), end = TRUE)
            w$endAction()
         w$endState()
      w$endStage()
      w$stage()
         w$state()
         w$endState()
      w$endStage()
   w$endProcess()
   w$closeWriter()

   mdp <- loadMDP("variance_action_", getLog = FALSE)
   runValueIte(mdp, "Weight", termValues = 0, getLog = FALSE)

   mdp$ptr$setTerminalW(0)
   runCalcWeights(mdp, "Weight", criterion = "expected", termValues = 0)
   expected <- getPolicy(mdp)$weight

   mdp$ptr$setTerminalW(0)
   runCalcWeights(mdp, "Weight", criterion = "secondMoment", termValues = 0)
   second_moment <- getPolicy(mdp)$weight

   runCalcWeights(mdp, "Weight", criterion = "variance", termValues = 0)
   variance <- getPolicy(mdp)$weight

   s0 <- getPolicy(mdp)$stateStr == "0,0"
   expect_equal(variance[s0], 1)
   expect_equal(variance, second_moment - expected^2)
})

test_that("Variance supports transition-level fixed-policy evaluation", {
   w <- binaryMDPWriter(prefix = "variance_transition_", getLog = FALSE)
   w$setWeights(character())
   w$setTransWeights("Weight")
   w$process()
      w$stage()
         w$state()
            w$action(weights = numeric(0), prob = c(1, 0, 0.5, 1, 1, 0.5),
                     transWeights = c(1, 1), end = TRUE)
            w$endAction()
         w$endState()
      w$endStage()
      w$stage()
         w$state()
            w$action(weights = numeric(0), prob = c(1, 0, 1),
                     transWeights = 0, end = TRUE)
            w$endAction()
         w$endState()
         w$state()
            w$action(weights = numeric(0), prob = c(1, 0, 1),
                     transWeights = 2, end = TRUE)
            w$endAction()
         w$endState()
      w$endStage()
      w$stage()
         w$state()
         w$endState()
      w$endStage()
   w$endProcess()
   w$closeWriter()

   mdp <- loadMDP("variance_transition_", getLog = FALSE)
   runValueIte(mdp, "Weight", termValues = 0, getLog = FALSE)

   mdp$ptr$setTerminalW(0)
   runCalcWeights(mdp, "Weight", criterion = "expected", termValues = 0)
   expected <- getPolicy(mdp)$weight

   mdp$ptr$setTerminalW(0)
   runCalcWeights(mdp, "Weight", criterion = "secondMoment", termValues = 0)
   second_moment <- getPolicy(mdp)$weight

   runCalcWeights(mdp, "Weight", criterion = "variance", termValues = 0)
   variance <- getPolicy(mdp)$weight

   s0 <- getPolicy(mdp)$stateStr == "0,0"
   expect_equal(variance[s0], 1)
   expect_equal(variance, second_moment - expected^2)
})

test_that("Variance uses terminal values as means and terminal variance zero", {
   w <- binaryMDPWriter(prefix = "variance_terminal_", getLog = FALSE)
   w$setWeights("Weight")
   w$process()
      w$stage()
         w$state()
            w$action(weights = 0, prob = c(1, 0, 0.5, 1, 1, 0.5), end = TRUE)
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

   mdp <- loadMDP("variance_terminal_", getLog = FALSE)
   runValueIte(mdp, "Weight", termValues = c(0, 2), getLog = FALSE)
   runCalcWeights(mdp, "Weight", criterion = "variance", termValues = c(0, 2))
   policy <- getPolicy(mdp)

   expect_equal(policy$weight[policy$stateStr == "0,0"], 1)
   expect_equal(policy$weight[policy$stateStr == "1,0"], 0)
   expect_equal(policy$weight[policy$stateStr == "1,1"], 0)
})

test_that("Variance is not a value-iteration Bellman operator", {
   w <- binaryMDPWriter(prefix = "variance_not_value_ite_", getLog = FALSE)
   w$setWeights("Weight")
   w$process()
      w$stage()
         w$state()
            w$action(weights = 1, prob = c(1, 0, 1), end = TRUE)
            w$endAction()
         w$endState()
      w$endStage()
      w$stage()
         w$state()
         w$endState()
      w$endStage()
   w$endProcess()
   w$closeWriter()

   mdp <- loadMDP("variance_not_value_ite_", getLog = FALSE)
   expect_error(
      runValueIte(mdp, "Weight", termValues = 0, bellmanOp = "variance", getLog = FALSE),
      "should be one of"
   )
   expect_error(
      mdp$ptr$calcRPO(8, 0, as.integer(c(0)), 0L, as.integer(c(0)), 0, 0L, 1),
      "Bellman operator not implemented"
   )
})


test_that("Long run average reward",{
   source("files/two_level_hmdp.R")
   mdp<-loadMDP("2lev_", getLog = FALSE)
   expect_lt(mdp$ptr$policyIteFixedPolicy(1L, 0L, 1L, 1), -1e+15)
   expect_match(mdp$ptr$getLog(), "valid fixed policy must be set")
   expect_equal(runPolicyIteAve(mdp,"Net reward","Duration", getLog = FALSE), 5.71428571428571441259691710001789033412933349609375)
   expect_equal(runPolicyIteAve(mdp,"Net reward","Items", getLog = FALSE), 4)
   expect_equal(runPolicyIteAve(mdp,"Items","Duration", getLog = FALSE), 2.71428571428571441259691710001789033412933349609375)
   rm(mdp)
   
   #sprintf("%.100f",g)
   source("files/HCT_ex6.1.1.R")
   mdp<-loadMDP("hct611_", getLog = FALSE)
   expect_equal(runPolicyIteAve(mdp,"Net reward","Duration", getLog = FALSE), -0.433789954337899297254210750907077454030513763427734375)
   rm(mdp)
   
   source("files/HCT_exc6.4.R")
   mdp<-loadMDP("hct64_", getLog = FALSE)
   expect_equal(runPolicyIteAve(mdp,"Net reward","Duration", getLog = FALSE), -17.7686915887850460649133310653269290924072265625)
   rm(mdp)
   
   source("files/HCT_exc6.7.R")
   mdp<-loadMDP("hct67_", getLog = FALSE)
   expect_equal(runPolicyIteAve(mdp,"Net reward","Duration", getLog = FALSE), -7.8132707659357780727304998436011373996734619140625)
   rm(mdp)
   
   source("files/HCT_exc7.3.R")
   mdp<-loadMDP("hct73_", getLog = FALSE)
   expect_equal(runPolicyIteAve(mdp,"Net reward","Duration", getLog = FALSE), 4)
   rm(mdp)
})


test_that("Discounted expected reward",{
   mdp<-loadMDP("2lev_", getLog = FALSE)
   rate<-0.1
   runPolicyIteDiscount(mdp, "Net reward", "Duration", rate, getLog = FALSE)
   weightsPolicyIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte[16], 56.33119951473481279435873148031532764434814453125)
   runValueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000, getLog = FALSE)
   weightsrunValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsrunValueIte)
   rate<-0.01
   runPolicyIteDiscount(mdp, "Net reward", "Duration", rate, getLog = FALSE)
   weightsPolicyIte<-getPolicy(mdp)$weight
   runValueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000, getLog = FALSE)
   weightsrunValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsrunValueIte)
   rm(mdp)
   
   mdp<-loadMDP("hct611_", getLog = FALSE)
   rate<-0.1
   runPolicyIteDiscount(mdp, "Net reward", "Duration", rate, getLog = FALSE)
   weightsPolicyIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte[5], -12.34541222517888314769152202643454074859619140625)
   expect_equal(sum(weightsPolicyIte), -36.6474287369603786146399215795099735260009765625)
   runValueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000, getLog = FALSE)
   weightsrunValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsrunValueIte)
   rate<-0.01
   runPolicyIteDiscount(mdp, "Net reward", "Duration", rate, getLog = FALSE)
   weightsPolicyIte<-getPolicy(mdp)$weight
   runValueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000, getLog = FALSE)
   weightsrunValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsrunValueIte)
   rm(mdp)
   
   mdp<-loadMDP("hct64_", getLog = FALSE)
   rate<-0.1
   runPolicyIteDiscount(mdp, "Net reward", "Duration", rate, getLog = FALSE)
   weightsPolicyIte<-getPolicy(mdp)$weight
   expect_equal(sum(weightsPolicyIte), -920.57968416603171135648153722286224365234375)
   runValueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000, getLog = FALSE)
   weightsrunValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsrunValueIte)
   rate<-0.01
   runPolicyIteDiscount(mdp, "Net reward", "Duration", rate, getLog = FALSE)
   weightsPolicyIte<-getPolicy(mdp)$weight
   runValueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000, getLog = FALSE)
   weightsrunValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsrunValueIte)
   rm(mdp)
   
   mdp<-loadMDP("hct67_", getLog = FALSE)
   rate<-0.1
   runPolicyIteDiscount(mdp, "Net reward", "Duration", rate, getLog = FALSE)
   weightsPolicyIte<-getPolicy(mdp)$weight
   expect_equal(sum(weightsPolicyIte), -2754.88121322102369958884082734584808349609375)
   runValueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000, getLog = FALSE)
   weightsrunValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsrunValueIte)
   rate<-0.01
   runPolicyIteDiscount(mdp, "Net reward", "Duration", rate, getLog = FALSE)
   weightsPolicyIte<-getPolicy(mdp)$weight
   runValueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000, getLog = FALSE)
   weightsrunValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsrunValueIte)
   rm(mdp)
   
   mdp<-loadMDP("hct73_", getLog = FALSE)
   rate<-0.1
   runPolicyIteDiscount(mdp, "Net reward", "Duration", rate, getLog = FALSE)
   weightsPolicyIte<-getPolicy(mdp)$weight
   expect_equal(sum(weightsPolicyIte), 240.1591230611287528518005274236202239990234375)
   runValueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000, getLog = FALSE)
   weightsrunValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsrunValueIte)
   rate<-0.01
   runPolicyIteDiscount(mdp, "Net reward", "Duration", rate, getLog = FALSE)
   weightsPolicyIte<-getPolicy(mdp)$weight
   runValueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000, getLog = FALSE)
   weightsrunValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsrunValueIte)
   rm(mdp)
   
   # test discount factor conversion
   mdp<-loadMDP("hct73_", getLog = FALSE)
   d<-0.9
   rate<- -log(d)
   runPolicyIteDiscount(mdp, "Net reward", "Duration", rate, getLog = FALSE)
   weightsPolicyIte1<-getPolicy(mdp)$weight
   runPolicyIteDiscount(mdp, "Net reward", "Duration", discountFactor = d, getLog = FALSE)
   weightsPolicyIte2<-getPolicy(mdp)$weight 
   expect_equal(weightsPolicyIte1, weightsPolicyIte2)
   rm(mdp)
})


cleanUp()
