library(MDP2)
context("Test calc of steady state pr")
cleanUp<-function() unlink("*.bin")

test_that("steadyStatePr",{
   #sprintf("%.100f",g)
   source("files/HCT_ex6.1.1.R")
   mdp<-loadMDP("hct611_", getLog = FALSE)
   runPolicyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   pr<-getSteadyStatePr(mdp)
   expect_equal(pr,c(0.56603774, 0.28301887, 0.09433962, 0.02358491, 0.03301887), tolerance = 1.5e-7)
})

cleanUp()
