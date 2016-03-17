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
})


test_that("Discounted expected reward",{
   mdp<-loadMDP("2lev_")
   rate<-0.1
   policyIteDiscount(mdp, "Net reward", "Duration", rate)
   weightsPolicyIte<-getPolicy(mdp)$weight
   valueIte(mdp, "Net reward", "Duration", rate, eps = 1e-15, maxIte = 10000)
   weightsValueIte<-getPolicy(mdp)$weight
   expect_equal(weightsPolicyIte, weightsValueIte)
   rm(mdp)
})


cleanUp()
