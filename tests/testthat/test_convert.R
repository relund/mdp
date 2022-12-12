library(MDP2)
library(XML)

context("Test model conversion")
cleanUp<-function() {
   unlink("*.bin")
   unlink("*.hmp")
}

test_that("convertHMP2Binary",{
   source("files/two_level_hmdp.R")
   prefix <- "2lev_"
   mdp <- loadMDP(prefix, getLog = FALSE)
   info1 <- infoMDP(mdp)
   g1 <- policyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   convertBinary2HMP(prefix, getLog = FALSE)
   prefix1 <- "2lev-converted_"
   convertHMP2Binary(paste0(prefix, "converted.hmp"), prefix1, getLog = FALSE)
   mdp <- loadMDP(prefix1, getLog = FALSE)
   info2 <- infoMDP(mdp)
   g2<-policyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   expect_equal(g1, g2)
   expect_equal(info1$stateDF, info2$stateDF)
   expect_equal(info1$actionDF, info2$actionDF)
   
   source("files/machine_replacement_v1.R")
   prefix <- "machine1_"
   mdp <- loadMDP(prefix, getLog = FALSE)
   info1 <- infoMDP(mdp)
   valueIte(mdp, "Net reward" , termValues=c(30,10,5,0), getLog = FALSE)
   g1 <- getPolicy(mdp)[9,5]
   rm(mdp)
   convertBinary2HMP(prefix, duration = NULL, getLog = FALSE)
   prefix1 <- "machine1-converted_"
   convertHMP2Binary(paste0(prefix, "converted.hmp"), prefix1, getLog = FALSE)
   mdp <- loadMDP(prefix1, getLog = FALSE)
   info2 <- infoMDP(mdp)
   valueIte(mdp, "Net reward" , termValues=c(30,10,5,0), getLog = FALSE)
   g2 <- getPolicy(mdp)[9,5]
   rm(mdp)
   expect_equal(g1, g2)
   expect_equal(info1$stateDF, info2$stateDF)
   expect_equal(info1$actionDF$trans, info2$actionDF$trans)
   expect_equal(info1$actionDF$pr, info2$actionDF$pr)
   
   # Small MDP
   prefix <- "test_"
   w = binaryMDPWriter(prefix, getLog = FALSE)
   w$setWeights(c("Duration", "Net rewards"))
   w$process()
      w$stage("Lactation cycle")
         w$state("Low")
            w$action(label = "Keep", scope = 1, weights = c(1, 10000), id = c(0, 1), pr = c(0.6, 0.4))
            w$endAction()
         w$endState()
         w$state("Average")
            w$action(label = "Keep", scope = 1, weights = c(1, 12000), id = c(0, 1), pr= c(0.2, 0.8))
            w$endAction()
         w$endState()
      w$endStage()
   w$endProcess()
   w$closeWriter()
   mdp <- loadMDP(prefix, getLog = FALSE)
   info1 <- infoMDP(mdp)
   g1 <- policyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   convertBinary2HMP(prefix, getLog = FALSE)
   prefix1 <- "test-converted_"
   convertHMP2Binary(paste0(prefix, "converted.hmp"), prefix1, getLog = FALSE)
   mdp <- loadMDP(prefix1, getLog = FALSE)
   info2 <- infoMDP(mdp)
   g2<-policyIteAve(mdp,"Net reward","Duration", getLog = FALSE)
   rm(mdp)
   expect_equal(g1, g2)
   expect_equal(info1$stateDF, info2$stateDF)
   expect_equal(info1$actionDF, info2$actionDF)
   
   # Small MDP with no labels (seems to crash :-( can be something to do if lbl bin files empty??
   prefix <- "test2_"
   w = binaryMDPWriter(prefix, getLog = FALSE)
   w$setWeights(c("D", "N"))
   w$process()
      w$stage()
         w$state()
            w$action(scope = 1, weights = c(1, 10000), id = c(0, 1), pr = c(0.6, 0.4), end = TRUE)
         w$endState()
         w$state()
            w$action(scope = 1, weights = c(1, 12000), id = c(0, 1), pr= c(0.2, 0.8), end = TRUE)
         w$endState()
      w$endStage()
   w$endProcess()
   w$closeWriter()
   mdp <- loadMDP(prefix, getLog = FALSE)
   info1 <- infoMDP(mdp)
   g1 <- policyIteAve(mdp,"N","D", getLog = FALSE)
   rm(mdp)
   convertBinary2HMP(prefix, getLog = FALSE)
   prefix1 <- "test-converted_"
   convertHMP2Binary(paste0(prefix, "converted.hmp"), prefix1, getLog = FALSE)
   mdp <- loadMDP(prefix1, getLog = FALSE)
   info2 <- infoMDP(mdp)
   g2<-policyIteAve(mdp,"N","D", getLog = FALSE)
   rm(mdp)
   expect_equal(g1, g2)
   info1$stateDF$label <- NULL
   info2$stateDF$label <- NULL
   expect_equal(info1$stateDF, info2$stateDF)
   info1$actionDF$label <- NULL
   info2$actionDF$label <- NULL
   expect_equal(info1$actionDF, info2$actionDF)
   
   ## Existing hmp files 
   n <- 3
   prefix <- paste0("cow", n, "_")
   convertHMP2Binary(paste0("files/cow", n, ".hmp"), prefix, getLog = FALSE)
   mdp <- loadMDP(prefix, getLog = FALSE)
   info1 <- infoMDP(mdp)
   expect_equal(mdp$states, n)
   rm(mdp)
   convertBinary2HMP(prefix, getLog = FALSE)
   prefix1 <- paste0("cow-converted", n, "_")
   convertHMP2Binary(paste0(prefix, "converted.hmp"), prefix1, getLog = FALSE)
   mdp <- loadMDP(prefix1, getLog = FALSE)
   info2 <- infoMDP(mdp)
   rm(mdp)
   expect_equal(info1$stateDF, info2$stateDF)
   expect_equal(info1$actionDF, info2$actionDF)   
   # 12 states
   n <- 12
   prefix <- paste0("cow", n, "_")
   convertHMP2Binary(paste0("files/cow", n, ".hmp"), prefix, getLog = FALSE)
   mdp <- loadMDP(prefix, getLog = FALSE)
   info1 <- infoMDP(mdp)
   expect_equal(mdp$states, n)
   rm(mdp)
   convertBinary2HMP(prefix, getLog = FALSE)
   prefix1 <- paste0("cow-converted", n, "_")
   convertHMP2Binary(paste0(prefix, "converted.hmp"), prefix1, getLog = FALSE)
   mdp <- loadMDP(prefix1, getLog = FALSE)
   info2 <- infoMDP(mdp)
   rm(mdp)
   expect_equal(info1$stateDF, info2$stateDF)
   expect_equal(info1$actionDF, info2$actionDF)  
   ## 36 states
   n <- 36
   prefix <- paste0("cow", n, "_")
   convertHMP2Binary(paste0("files/cow", n, ".hmp"), prefix, getLog = FALSE)
   mdp <- loadMDP(prefix, getLog = FALSE)
   info1 <- infoMDP(mdp)
   expect_equal(mdp$states, n)
   rm(mdp)
   convertBinary2HMP(prefix, getLog = FALSE)
   prefix1 <- paste0("cow-converted", n, "_")
   convertHMP2Binary(paste0(prefix, "converted.hmp"), prefix1, getLog = FALSE)
   mdp <- loadMDP(prefix1, getLog = FALSE)
   info2 <- infoMDP(mdp)
   rm(mdp)
   expect_equal(info1$stateDF, info2$stateDF)
   expect_equal(info1$actionDF, info2$actionDF)  
})

cleanUp()
