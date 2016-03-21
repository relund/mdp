library(MDP2)
context("Test model building")
cleanUp<-function() unlink("*.bin")


test_that("binaryMDPWriter",{
   #sprintf("%.100f",g)
   source("files/HCT_ex6.1.1.R")
   mdp<-loadMDP("hct611_", getLog = FALSE)
   gProb<-policyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   source("files/HCT_ex6.1.1_v2.R")
   mdp<-loadMDP("hct611v2_", getLog = FALSE)
   gPr<-policyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   expect_equal(gProb, gPr)
   
   source("files/HCT_exc6.4.R")
   mdp<-loadMDP("hct64_", getLog = FALSE)
   gProb<-policyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   source("files/HCT_exc6.4_v2.R")
   mdp<-loadMDP("hct64v2_", getLog = FALSE)
   gPr<-policyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   expect_equal(gProb, gPr)
   
   source("files/HCT_exc6.7.R")
   mdp<-loadMDP("hct67_", getLog = FALSE)
   gProb<-policyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   source("files/HCT_exc6.7_v2.R")
   mdp<-loadMDP("hct67v2_", getLog = FALSE)
   gPr<-policyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   expect_equal(gProb, gPr)
   
   source("files/HCT_exc7.3.R")
   mdp<-loadMDP("hct73_", getLog = FALSE)
   gProb<-policyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   source("files/HCT_exc7.3_v2.R")
   mdp<-loadMDP("hct73v2_", getLog = FALSE)
   gPr<-policyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   expect_equal(gProb, gPr)
   
   ## test loading with P, R and D
   source("files/HCT_ex6.1.1_v3.R")
   # test using L.R.A reward / t.unit
   mdp<-loadMDP("hct611v3-1_", getLog = FALSE)
   expect_equal(policyIteAve(mdp,"Net reward","Duration", getLog = FALSE), -0.433789954337899297254210750907077454030513763427734375)
   rm(mdp)
   mdp<-loadMDP("hct611v3-2_", getLog = FALSE)
   expect_equal(policyIteAve(mdp,"Net reward","Duration", getLog = FALSE), -0.433789954337899297254210750907077454030513763427734375)
   rm(mdp)
   mdp<-loadMDP("hct611v3-3_", getLog = FALSE)
   expect_equal(policyIteAve(mdp,"Net reward","Duration", getLog = FALSE), -0.433789954337899297254210750907077454030513763427734375)
   rm(mdp)
   mdp<-loadMDP("hct611v3-4_", getLog = FALSE)
   expect_equal(policyIteAve(mdp,"Net reward","Duration", getLog = FALSE), -0.433789954337899297254210750907077454030513763427734375)
   rm(mdp)
   # test using discounted reward
   # compare MDP formulations
   mdp<-loadMDP("hct611v3-1_", getLog = FALSE)
   policyIteDiscount(mdp, "Net reward", "Duration", discountFactor = 0.9, getLog = FALSE)
   weightsPolicyIte<-getPolicy(mdp)$weight
   expect_equal(sum(weightsPolicyIte), -37.94871237074722358784129028208553791046142578125)
   rm(mdp)
   mdp<-loadMDP("hct611v3-2_", getLog = FALSE)
   policyIteDiscount(mdp, "Net reward", "Duration", discountFactor = 0.9, getLog = FALSE)
   weightsPolicyIte<-getPolicy(mdp)$weight
   expect_equal(sum(weightsPolicyIte), -37.94871237074722358784129028208553791046142578125)
   rm(mdp)
   mdp<-loadMDP("hct611v3-3_", getLog = FALSE)
   policyIteDiscount(mdp, "Net reward", "Duration", discountFactor = 0.9, getLog = FALSE)
   weightsPolicyIte<-getPolicy(mdp)$weight
   expect_equal(sum(weightsPolicyIte), -37.94871237074722358784129028208553791046142578125)
   rm(mdp)
   # compare semi-MDP formulations
   mdp<-loadMDP("hct611v2_", getLog = FALSE)
   policyIteDiscount(mdp, "Net reward", "Duration", discountFactor = 0.9, getLog = FALSE)
   weightsPolicyIte1<-getPolicy(mdp)$weight
   rm(mdp)
   mdp<-loadMDP("hct611v3-4_", getLog = FALSE)
   policyIteDiscount(mdp, "Net reward", "Duration", discountFactor = 0.9, getLog = FALSE)
   weightsPolicyIte2<-getPolicy(mdp)$weight
   rm(mdp)
   expect_equal(weightsPolicyIte1, weightsPolicyIte2)
})

cleanUp()
