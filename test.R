library(MDP)
prefix<-"hmdp_"
mdp<-load_mdp(prefix, verbose = T)
g<-policyIteAve(mdp, "Reward", "Time", max_ite = 5)
get_policy_w(mdp, "Reward", s_id = 0)
get_policy(mdp)

policyIteDiscount(mdp, "Reward", "Time", rate=0.1, rate_base = 365)
p<-get_policy_w(mdp, "Reward")
ids<-getIdSStages(mdp,"0")
p[ids+1,]
run_value_ite(mdp, "Reward", dur = "Time", rate = 0.1, rate_base = 365, times = 20000, term_values = 92290, eps=1e-04)
p<-get_policy_w(mdp, "Reward")
ids<-getIdSStages(mdp,"0")
p[ids+1,]

prefix="h1_"
do.call(file.remove,list(list.files(pattern = prefix)))
random_hmdp(prefix, levels=1, time_horizon=c(Inf), states=c(10), actions=c(2,2), rewards=c(0,100), durations=c(1,10))

prefix="h2_"
do.call(file.remove,list(list.files(pattern = prefix)))
random_hmdp(prefix, levels=3, time_horizon=c(Inf,3,4), states=c(2,4,5), actions=c(2,6), child_process_pr = 0.5, rewards=c(0,100), durations=c(1,10))

prefix="h3_"
do.call(file.remove,list(list.files(pattern = prefix)))
random_hmdp(prefix, levels=2, time_horizon=c(Inf,6,4), states=c(2,34,5), actions=c(2,6), child_process_pr = 0.9, rewards=c(0,50), durations=c(1,10))

prefix<-"h1_"
mdp1<-load_mdp(prefix, verbose = T)
policyIteAve(mdp1, "Reward", "Duration")
policyIteDiscount(mdp1, "Reward", "Duration", rate=0.1, rate_base = 365)
p<-get_policy_w(mdp1, "Reward")
a<-action_info(prefix)
term_values<-rep(0,mdp1$founder_states_last)
run_value_ite(mdp1, "Reward", dur = "Duration", rate = 0.1, rate_base = 365, times = 20000, term_values = term_values, eps=1e-03)
p<-get_policy_w(mdp1, "Reward")
ids<-getIdSStages(mdp1,"0")
p[ids+1,]

prefix<-"h2_"
mdp2<-load_mdp(prefix, verbose = T)
policyIteAve(mdp2, "Reward", "Duration")
policyIteDiscount(mdp2, "Reward", "Duration", rate=0.1, rate_base = 365)
p<-get_policy_w(mdp2, "Reward")
ids<-getIdSStages(mdp2,"0")
p[ids+1,]
a<-action_info(prefix)


prefix<-"h3_"
mdp3<-load_mdp(prefix, verbose = T)
policyIteAve(mdp3, "Reward", "Duration")
policyIteDiscount(mdp3, "Reward", "Duration", rate=0.1, rate_base = 365)
p<-get_policy_w(mdp3, "Reward")
ids<-getIdSStages(mdp3,"0")
p[ids+1,]
a<-action_info(prefix)

prefix<-"proc1_"
#mdp4<-load_mdp(prefix, verbose = T)
action_info(prefix)


## Testing MDP2 package

# internal module methods
# mdp<-new(HMDP,"hmdp_")
# mdp$externalProcess
# mdp$w_names
# mdp$time_horizon
# mdp$levels
# mdp$getLog()
# mdp$run_value_ite(1, times=100, 0.001, 0, 1, 0, 0, 0, 1)
# mdp$getLog()


library(MDP2)
prefix<-"hmdp_"
binInfoStates(prefix)
binInfoActions(prefix)
mdp<-load_mdp(prefix, verbose = T)
g<-policyIteAve(mdp, "Reward", "Time", max_ite = 5)
get_policy(mdp)
run_value_ite(mdp, "Reward", "Time", rate = 0.1)
get_policy(mdp)


l<-infoMDP(mdp, s_id=c(9,15), with_harc = F, with_df = F)
#infoMDP(mdp, s_id=c(9,78))  # gives an error
infoMDP(mdp, state_str = c("0,1","0,0,0,1,0") )
#infoMDP(mdp, state_str = c("ads","0,0,0,1,0") )  # gives an error
infoMDP(mdp, stage_str = c("0","0,0,0,1"), with_harc = T )
infoMDP(mdp, with_harc = T)
#infoMDP(mdp, stage_str = c("0","0,e") )    # no error just ignore the last
l<-infoMDP(mdp, stage_str = c("0","0,0,0,1"), with_harc = F, with_df = F)



# Test large files
library(MDP2)
prefix<-"tmp/test_"
system('tasklist /fi "IMAGENAME eq rsession.exe"')
mdp<-load_mdp(prefix)
system('tasklist /fi "IMAGENAME eq rsession.exe"')
rm(list = ls())
detach("package:MDP2", unload=TRUE)

library(MDP)
prefix<-"tmp/test_"
system('tasklist /fi "IMAGENAME eq rsession.exe"')
mdp<-load_mdp(prefix)
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
mdp<-load_mdp(prefix)
system('tasklist /fi "IMAGENAME eq rsession.exe"')
rm(list = ls())
detach("package:MDP2", unload=TRUE)

library(MDP)
prefix<-"tmp/test1_"
system('tasklist /fi "IMAGENAME eq rsession.exe"')
mdp<-load_mdp(prefix)
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


# Test get_policy
library(MDP2)
prefix<-"tmp/test2_"
#tmp<-binInfoStates(prefix)
#head(tmp)
mdp<-load_mdp(prefix)
g<-policyIteAve(mdp, "Reward", "Time", max_ite = 1)
p<-get_policy(mdp, s_id=1) 


## Testing on randomly generated HMDPs
for (i in 1:3) {
   prefix=paste("tmp/rand",i,"_",sep="")
   do.call(file.remove,list(list.files(pattern = prefix)))
   random_hmdp(prefix, levels=sample(1:4,1), time_horizon=c(Inf,5,5,5,5), states=c(3,3,4,5,6), actions=c(2,2), 
              child_process_pr = 1, externalProcessPr=1, rewards=c(0,100), durations=c(1,5) )
   mdp<-load_mdp(prefix, verbose = T)
   g<-policyIteAve(mdp, "Reward", "Duration", max_ite = 10)
}

i=0
prefix=paste("tmp/rand",i,"_",sep="")
do.call(file.remove,list(list.files(pattern = prefix)))
random_hmdp(prefix, levels=3, time_horizon=c(5,5,5), states=c(3,3,4), actions=c(1,1), 
           child_process_pr = 1, externalProcessPr=1, rewards=c(0,100), durations=c(1,5) )
mdp<-load_mdp(prefix, verbose = T)
g<-policyIteAve(mdp, "Reward", "Duration", max_ite = 10)
run_value_ite(mdp, "Reward", "Duration")


prefix=paste("tmp/rand1")
do.call(file.remove,list(list.files(pattern = prefix)))
random_hmdp(prefix, levels=3, time_horizon=c(Inf,5,10,3,3), states=c(2,5,10,10,10), actions=c(1,3), 
           child_process_pr = 0.5, externalProcessPr=0.1, rewards=c(0,100), durations=c(1,5) )


prefix<-"tmp/rand2_"
mdp<-load_mdp(prefix, verbose = T)
g<-policyIteAve(mdp, "Reward", "Duration", max_ite = 4)
p<-get_policy(mdp, s_id=1) 


random_hmdp("test_", levels=2)
mdp<-load_mdp("test_", verbose = T)
policyIteAve(mdp, "Reward", "Duration", max_ite = 10)
calcWeights(mdp,"Reward","average","Duration")
pol<-get_policy(mdp)
newPolicy<-pol[,c(1,3)]
newPolicy$a_idx<-0
set_policy(mdp,policy = newPolicy)
get_policy(mdp)
calcWeights(mdp,"Reward","average","Duration")



mdp1<-load_mdp("hmdp_")
policyIteAve(mdp1, "Reward", "Time")
save_mdp(mdp1,"test_")
binInfoStates("hmdp_")
binInfoStates("test_")
binInfoActions("hmdp_")
binInfoActions("test_")


mdp2<-load_mdp("test_")
policyIteAve(mdp2, "Reward", "Time")
