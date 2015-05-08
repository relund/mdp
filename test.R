library(MDP)
prefix<-"hmdp_"
mdp<-loadMDP(prefix, verbose = T)
g<-policyIteAve(mdp, "Reward", "Time", maxIte = 5)
getPolicyW(mdp, "Reward", sId = 0)
getPolicy(mdp)

policyIteDiscount(mdp, "Reward", "Time", rate=0.1, rateBase = 365)
p<-getPolicyW(mdp, "Reward")
ids<-getIdSStages(mdp,"0")
p[ids+1,]
valueIte(mdp, "Reward", dur = "Time", rate = 0.1, rateBase = 365, times = 20000, termValues = 92290, eps=1e-04)
p<-getPolicyW(mdp, "Reward")
ids<-getIdSStages(mdp,"0")
p[ids+1,]

prefix="h1_"
do.call(file.remove,list(list.files(pattern = prefix)))
randomHMDP(prefix, levels=1, timeHorizon=c(Inf), states=c(10), actions=c(2,2), rewards=c(0,100), durations=c(1,10))

prefix="h2_"
do.call(file.remove,list(list.files(pattern = prefix)))
randomHMDP(prefix, levels=3, timeHorizon=c(Inf,3,4), states=c(2,4,5), actions=c(2,6), childProcessPr = 0.5, rewards=c(0,100), durations=c(1,10))

prefix="h3_"
do.call(file.remove,list(list.files(pattern = prefix)))
randomHMDP(prefix, levels=2, timeHorizon=c(Inf,6,4), states=c(2,34,5), actions=c(2,6), childProcessPr = 0.9, rewards=c(0,50), durations=c(1,10))

prefix<-"h1_"
mdp1<-loadMDP(prefix, verbose = T)
policyIteAve(mdp1, "Reward", "Duration")
policyIteDiscount(mdp1, "Reward", "Duration", rate=0.1, rateBase = 365)
p<-getPolicyW(mdp1, "Reward")
a<-actionInfo(prefix)
termValues<-rep(0,mdp1$founderStatesLast)
valueIte(mdp1, "Reward", dur = "Duration", rate = 0.1, rateBase = 365, times = 20000, termValues = termValues, eps=1e-03)
p<-getPolicyW(mdp1, "Reward")
ids<-getIdSStages(mdp1,"0")
p[ids+1,]

prefix<-"h2_"
mdp2<-loadMDP(prefix, verbose = T)
policyIteAve(mdp2, "Reward", "Duration")
policyIteDiscount(mdp2, "Reward", "Duration", rate=0.1, rateBase = 365)
p<-getPolicyW(mdp2, "Reward")
ids<-getIdSStages(mdp2,"0")
p[ids+1,]
a<-actionInfo(prefix)


prefix<-"h3_"
mdp3<-loadMDP(prefix, verbose = T)
policyIteAve(mdp3, "Reward", "Duration")
policyIteDiscount(mdp3, "Reward", "Duration", rate=0.1, rateBase = 365)
p<-getPolicyW(mdp3, "Reward")
ids<-getIdSStages(mdp3,"0")
p[ids+1,]
a<-actionInfo(prefix)

prefix<-"proc1_"
#mdp4<-loadMDP(prefix, verbose = T)
actionInfo(prefix)


## Testing MDP2 package

# internal module methods
# mdp<-new(HMDP,"hmdp_")
# mdp$externalProcess
# mdp$wNames
# mdp$timeHorizon
# mdp$levels
# mdp$getLog()
# mdp$valueIte(1, times=100, 0.001, 0, 1, 0, 0, 0, 1)
# mdp$getLog()


library(MDP2)
prefix<-"hmdp_"
binInfoStates(prefix)
binInfoActions(prefix)
mdp<-loadMDP(prefix, verbose = T)
g<-policyIteAve(mdp, "Reward", "Time", maxIte = 5)
policy<-getPolicy(mdp)


l<-infoMDP(mdp, sId=c(9,15), withHarc = F, withDF = F)
#infoMDP(mdp, sId=c(9,78))  # gives an error
infoMDP(mdp, stateStr = c("0,1","0,0,0,1,0") )
#infoMDP(mdp, stateStr = c("ads","0,0,0,1,0") )  # gives an error
infoMDP(mdp, stageStr = c("0","0,0,0,1"), withHarc = T )
infoMDP(mdp, withHarc = T)
#infoMDP(mdp, stageStr = c("0","0,e") )    # no error just ignore the last
l<-infoMDP(mdp, stageStr = c("0","0,0,0,1"), withHarc = F, withDF = F)



# Test large files
library(MDP2)
prefix<-"tmp/test_"
system('tasklist /fi "IMAGENAME eq rsession.exe"')
mdp<-loadMDP(prefix)
system('tasklist /fi "IMAGENAME eq rsession.exe"')
rm(list = ls())
detach("package:MDP2", unload=TRUE)

library(MDP)
prefix<-"tmp/test_"
system('tasklist /fi "IMAGENAME eq rsession.exe"')
mdp<-loadMDP(prefix)
system('tasklist /fi "IMAGENAME eq rsession.exe"')
rm(list = ls())
detach("package:MDP", unload=TRUE)

# Output MDP2:
# Read binary files (2.99925 sec.)
# Build the HMDP (10.6766 sec.)
# 
# Checking MDP and found no errors (0.0731138 sec.)
# 
# Image Name                     PID Session Name        Session#    Mem Usage
# ========================= ======== ================ =========== ============
# rsession.exe                  8856 Console                    1    751.264 K

# Output MDP:
# Cpu time for reading the binary files: 5.44425 sec.
# Cpu time for checking MDP: 694.543 sec.
# Cpu time for building state-expanded hypergraph 68.0378 sec.
# 
# Image Name                     PID Session Name        Session#    Mem Usage
# ========================= ======== ================ =========== ============
# rsession.exe                  8856 Console                    1  1.670.328 K


# Test very large files
library(MDP2)
prefix<-"tmp/test1_"
system('tasklist /fi "IMAGENAME eq rsession.exe"')
mdp<-loadMDP(prefix)
system('tasklist /fi "IMAGENAME eq rsession.exe"')
rm(list = ls())
detach("package:MDP2", unload=TRUE)

library(MDP)
prefix<-"tmp/test1_"
system('tasklist /fi "IMAGENAME eq rsession.exe"')
mdp<-loadMDP(prefix)
system('tasklist /fi "IMAGENAME eq rsession.exe"')
rm(list = ls())
detach("package:MDP", unload=TRUE)

# Output MDP2:
# Read binary files (31.8353 sec.)
# Build the HMDP (81.9055 sec.)
# 
# Checking MDP and found no errors (0.692772 sec.)
# 
# Image Name                     PID Session Name        Session#    Mem Usage
# ========================= ======== ================ =========== ============
# rsession.exe                 10524 Console                    1  5.555.296 K

# Output MDP:
# Cpu time for reading the binary files: 43.1743 sec.
# Cpu time for checking MDP: 42103 sec.
# Cpu time for building state-expanded hypergraph 1095.56 sec.
# 
# Image Name                     PID Session Name        Session#    Mem Usage
# ========================= ======== ================ =========== ============
# rsession.exe                 10524 Console                    1 14.121.232 K


# Test getPolicy
library(MDP2)
prefix<-"tmp/test2_"
#tmp<-binInfoStates(prefix)
#head(tmp)
mdp<-loadMDP(prefix)
g<-policyIteAve(mdp, "Reward", "Time", maxIte = 1)
p<-getPolicy(mdp, sId=1) 


## Testing on randomly generated HMDPs
for (i in 1:5) {
   prefix=paste("tmp/rand",i,"_",sep="")
   do.call(file.remove,list(list.files(pattern = prefix)))
   randomHMDP(prefix, levels=sample(2:4,1), timeHorizon=c(Inf,5,10,10,10), states=c(2,5,10,15,20), actions=c(1,3), 
              childProcessPr = 0.5, externalProcessPr=0.5, rewards=c(0,100), durations=c(1,5) )
}

