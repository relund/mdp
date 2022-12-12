# wDir <- getwd()
# setwd(system.file("models", package = "MDP2"))
# 
# ## convert the machine example to a hmp file
# prefix1<-"machine1_"
# convertBinary2HMP(prefix1, duration=NULL)
# # have a look at the hmp file
# xmlTreeParse("machine1_converted.hmp",useInternalNodes=TRUE)
# 
# ## convert the machine example hmp file to binary files
# prefix2<-"machine_cov_"
# convertHMP2Binary("machine1.hmp",prefix2)
# stateIdxDf(prefix1)
# stateIdxDf(prefix2)

## convert the machine example with a single dummy node to a hmp file
#convertBinary2HMP("machine2_")  # error since using scope = 3 not supported in hmp files
