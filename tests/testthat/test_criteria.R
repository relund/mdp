library(MDP2)
context("Check optimality")
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
      "Transition-level weights are not supported for BellmanOp::DiscountedExpectedReward"
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


test_that("Long run average reward",{
   source("files/two_level_hmdp.R")
   mdp<-loadMDP("2lev_", getLog = FALSE)
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
