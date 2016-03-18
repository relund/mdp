library(MDP2)
context("check optimality")
cleanUp<-function() unlink("*.bin")

test_that("Total reward",{
   source("files/machine_replacement_v1.R")
   mdp<-loadMDP("machine1_")
   w<-"Net reward"             # label of the weight we want to optimize
   scrapValues<-c(30,10,5,0)   # scrap values (the values of the 4 states at stage 4)
   valueIte(mdp, w, termValues=scrapValues)
   expect_equal(getPolicy(mdp,13)$weight,102.2)
   rm(mdp)
   
   # same model with a single dummy node
   source("files/machine_replacement_v2.R")
   mdp<-loadMDP("machine2_")
   w<-"Net reward"             # label of the weight we want to optimize
   valueIte(mdp, w, termValues=0)
   expect_equal(getPolicy(mdp,12)$weight,102.2)
   rm(mdp)
})


test_that("Long run average reward",{
   source("files/two_level_hmdp.R")
   mdp<-loadMDP("2lev_")
   expect_equal(policyIteAve(mdp,"Net reward","Duration"), 5.71428571428571441259691710001789033412933349609375)
   expect_equal(policyIteAve(mdp,"Net reward","Items"), 4)
   expect_equal(policyIteAve(mdp,"Items","Duration"), 2.71428571428571441259691710001789033412933349609375)
   rm(mdp)
   
   #sprintf("%.100f",g)
   source("files/HCT_ex6.1.1.R")
   mdp<-loadMDP("hct611_")
   expect_equal(policyIteAve(mdp,"Net reward","Duration"), -0.433789954337899297254210750907077454030513763427734375)
   rm(mdp)
   
   source("files/HCT_exc6.4.R")
   mdp<-loadMDP("hct64_")
   expect_equal(policyIteAve(mdp,"Net reward","Duration"), -17.7686915887850460649133310653269290924072265625)
   rm(mdp)
   
   source("files/HCT_exc6.7.R")
   mdp<-loadMDP("hct67_")
   expect_equal(policyIteAve(mdp,"Net reward","Duration"), -7.8132707659357780727304998436011373996734619140625)
   rm(mdp)
   
   source("files/HCT_exc7.3.R")
   mdp<-loadMDP("hct73_")
   expect_equal(policyIteAve(mdp,"Net reward","Duration"), 4)
   rm(mdp)
})


test_that("Discounted expected reward",{
   mdp<-loadMDP("2lev_")
   rate<-0.1
   policyIteDiscount(mdp, "Net reward", "Duration", rate)
   weightsPolicyIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte[16], 56.33119951473481279435873148031532764434814453125)
   valueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000)
   weightsValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsValueIte)
   rate<-0.01
   policyIteDiscount(mdp, "Net reward", "Duration", rate)
   weightsPolicyIte<-getPolicy(mdp)$weight
   valueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000)
   weightsValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsValueIte)
   rm(mdp)
   
   mdp<-loadMDP("hct611_")
   rate<-0.1
   policyIteDiscount(mdp, "Net reward", "Duration", rate)
   weightsPolicyIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte[5], -12.34541222517888314769152202643454074859619140625)
   expect_equal(sum(weightsPolicyIte), -36.6474287369603786146399215795099735260009765625)
   valueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000)
   weightsValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsValueIte)
   rate<-0.01
   policyIteDiscount(mdp, "Net reward", "Duration", rate)
   weightsPolicyIte<-getPolicy(mdp)$weight
   valueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000)
   weightsValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsValueIte)
   rm(mdp)
   
   mdp<-loadMDP("hct64_")
   rate<-0.1
   policyIteDiscount(mdp, "Net reward", "Duration", rate)
   weightsPolicyIte<-getPolicy(mdp)$weight
   expect_equal(sum(weightsPolicyIte), -920.57968416603171135648153722286224365234375)
   valueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000)
   weightsValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsValueIte)
   rate<-0.01
   policyIteDiscount(mdp, "Net reward", "Duration", rate)
   weightsPolicyIte<-getPolicy(mdp)$weight
   valueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000)
   weightsValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsValueIte)
   rm(mdp)
   
   mdp<-loadMDP("hct67_")
   rate<-0.1
   policyIteDiscount(mdp, "Net reward", "Duration", rate)
   weightsPolicyIte<-getPolicy(mdp)$weight
   expect_equal(sum(weightsPolicyIte), -2754.88121322102369958884082734584808349609375)
   valueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000)
   weightsValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsValueIte)
   rate<-0.01
   policyIteDiscount(mdp, "Net reward", "Duration", rate)
   weightsPolicyIte<-getPolicy(mdp)$weight
   valueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000)
   weightsValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsValueIte)
   rm(mdp)
   
   mdp<-loadMDP("hct73_")
   rate<-0.1
   policyIteDiscount(mdp, "Net reward", "Duration", rate)
   weightsPolicyIte<-getPolicy(mdp)$weight
   expect_equal(sum(weightsPolicyIte), 240.1591230611287528518005274236202239990234375)
   valueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000)
   weightsValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsValueIte)
   rate<-0.01
   policyIteDiscount(mdp, "Net reward", "Duration", rate)
   weightsPolicyIte<-getPolicy(mdp)$weight
   valueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000)
   weightsValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsValueIte)
   rm(mdp)
})


cleanUp()
