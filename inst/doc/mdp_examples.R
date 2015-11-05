## ----Install r-forge, echo=TRUE, eval=FALSE------------------------------
#  install.packages("MDP2",repos="http://r-forge.r-project.org")

## ----Install github, echo=TRUE, eval=FALSE-------------------------------
#  install_github("relund/mdp")

## ----Load package,echo=TRUE----------------------------------------------
library(MDP2)

## ----Package help, echo=TRUE, eval=FALSE---------------------------------
#  ?MDP2

## ----Generate replacement model,echo=TRUE--------------------------------
prefix<-"machine_"
w <- binaryMDPWriter(prefix)
w$setWeights(c("Net reward"))
w$process()
	w$stage()   # stage n=0
		w$state(label="Dummy")          # v=(0,0)
			w$action(label="buy", weights=-100, prob=c(1,0,0.7, 1,1,0.3), end=T)
		w$endState()
	w$endStage()
	w$stage()   # stage n=1
		w$state(label="good")           # v=(1,0)
			w$action(label="mt", weights=55, prob=c(1,0,1), end=T)
			w$action(label="nmt", weights=70, prob=c(1,0,0.6, 1,1,0.4), end=T)
		w$endState()
		w$state(label="average")        # v=(1,1)
			w$action(label="mt", weights=40, prob=c(1,0,1), end=T)
			w$action(label="nmt", weights=50, prob=c(1,1,0.6, 1,2,0.4), end=T)
		w$endState()
	w$endStage()
	w$stage()   # stage n=2
		w$state(label="good")           # v=(2,0)
			w$action(label="mt", weights=55, prob=c(1,0,1), end=T)
			w$action(label="nmt", weights=70, prob=c(1,0,0.5, 1,1,0.5), end=T)
		w$endState()
		w$state(label="average")        # v=(2,1)
			w$action(label="mt", weights=40, prob=c(1,0,1), end=T)
			w$action(label="nmt", weights=50, prob=c(1,1,0.5, 1,2,0.5), end=T)
		w$endState()
		w$state(label="not working")    # v=(2,2)
			w$action(label="mt", weights=30, prob=c(1,0,1), end=T)
			w$action(label="rep", weights=5, prob=c(1,3,1), end=T)
		w$endState()
	w$endStage()
	w$stage()   # stage n=3
		w$state(label="good")           # v=(3,0)
			w$action(label="mt", weights=55, prob=c(1,0,1), end=T)
			w$action(label="nmt", weights=70, prob=c(1,0,0.2, 1,1,0.8), end=T)
		w$endState()
		w$state(label="average")        # v=(3,1)
			w$action(label="mt", weights=40, prob=c(1,0,1), end=T)
			w$action(label="nmt", weights=50, prob=c(1,1,0.2, 1,2,0.8), end=T)
		w$endState()
		w$state(label="not working")    # v=(3,2)
			w$action(label="mt", weights=30, prob=c(1,0,1), end=T)
			w$action(label="rep", weights=5, prob=c(1,3,1), end=T)
		w$endState()
		w$state(label="replaced")       # v=(3,3)
			w$action(label="Dummy", weights=0, prob=c(1,3,1), end=T)
		w$endState()
	w$endStage()
	w$stage()   # stage n=4
		w$state(label="good", end=T)        # v=(4,0)
		w$state(label="average", end=T)     # v=(4,1)
		w$state(label="not working", end=T) # v=(4,2)
		w$state(label="replaced", end=T)    # v=(4,3)
	w$endStage()
w$endProcess()
w$closeWriter()

## ----Load model (machine rep),echo=TRUE----------------------------------
mdp<-loadMDP(prefix)    # load the model
mdp                     # general info

## ----Info (machine rep),echo=TRUE----------------------------------------
info<-infoMDP(mdp)
info$stateDF
info$actionDF

## ----Optimize MDP (machine rep),echo=TRUE--------------------------------
wLbl<-"Net reward"             # the weight we want to optimize
scrapValues<-c(30,10,5,0)     # scrap values (the values of the 4 states at stage 4)
valueIte(mdp, wLbl, termValues=scrapValues)

## ----Get policy,echo=TRUE------------------------------------------------
getPolicy(mdp)

## ----Set policy (machine rep),echo=TRUE,eval=TRUE------------------------
policy<-data.frame(sId=c(8,11),aIdx=c(0,0))
setPolicy(mdp, policy)

## ----Calc reward (machine rep),echo=TRUE---------------------------------
calcWeights(mdp, wLbl, termValues=scrapValues)
getPolicy(mdp)    

## ----Generate sow MDP, echo=TRUE-----------------------------------------
prefix="sow_"
w<-binaryMDPWriter(prefix)
w$setWeights(c("Duration", "Net reward", "Piglets"))
w$process()
	w$stage()
		w$state(label="Small litter")
			w$action(label="Keep",weights=c(1,10000,8),prob=c(1,0,0.6, 1,1,0.3, 1,2,0.1))
			w$endAction()
			w$action(label="Replace",weights=c(1,9000,8),prob=c(1,0,1/3, 1,1,1/3, 1,2,1/3))
			w$endAction()
		w$endState()
		w$state(label="Average litter")
			w$action(label="Keep",weights=c(1,12000,11),prob=c(1,0,0.2, 1,1,0.6, 1,2,0.2))
			w$endAction()
			w$action(label="Replace",weights=c(1,11000,11),prob=c(1,0,1/3, 1,1,1/3, 1,2,1/3))
			w$endAction()
		w$endState()
		w$state(label="Big litter")
			w$action(label="Keep",weights=c(1,14000,14),prob=c(1,0,0.1, 1,1,0.3, 1,2,0.6))
			w$endAction()
			w$action(label="Replace",weights=c(1,13000,14),prob=c(1,0,1/3, 1,1,1/3, 1,2,1/3))
			w$endAction()
		w$endState()
	w$endStage()
w$endProcess()
w$closeWriter()

## ----Model info (sow rep), echo=TRUE-------------------------------------
mdp<-loadMDP(prefix)    # load the model
info<-infoMDP(mdp)
info$stateDF
info$actionDF

## ----Value ite (sow rep), echo=TRUE--------------------------------------
## solve the MDP using value iteration
wLbl<-"Net reward"         # the weight we want to optimize
durLbl<-"Duration"         # the duration/time label
rate<-0.1               # discount rate
rateBase<-1             # rate base
valueIte(mdp, wLbl, durLbl, rate, rateBase, maxIte = 10000, eps = 0.00001)
getPolicy(mdp)     # optimal policy for each sId

## ----Value ite steps (sow rep), echo=TRUE, results='hide'----------------
termValues<-c(0,0,0)
iterations<-1:211
df<-data.frame(n=iterations,a1=NA,V1=NA,D1=NA,a2=NA,V2=NA,D2=NA,a3=NA,V3=NA,D3=NA)
for (i in iterations) {
	valueIte(mdp, wLbl, durLbl, rate, rateBase, maxIte = 1, eps = 0.00001, termValues)
	a<-getPolicy(mdp)
	res<-rep(NA,10)
	res[1]<-i
	res[2]<-a$actionLabel[1]
	res[3]<-round(a$weight[1],2)
	res[4]<-round(a$weight[1]-termValues[1],2)
	res[5]<-a$actionLabel[2]
	res[6]<-round(a$weight[2],2)
	res[7]<-round(a$weight[2]-termValues[2],2)
	res[8]<-a$actionLabel[3]
	res[9]<-round(a$weight[3],2)
	res[10]<-round(a$weight[3]-termValues[3],2)
	df[i,]<-res
	termValues<-a$weight
}

## ------------------------------------------------------------------------
df[c(1:3,51:53,151:153,210:211),]

## ----Policy ite discount (sow rep), echo=TRUE----------------------------
policyIteDiscount(mdp, wLbl, durLbl, rate, rateBase)
policy<-getPolicy(mdp)
rpo<-calcRPO(mdp, wLbl, iA=c(0,0,0), criterion="discount", dur=durLbl, rate=rate, rateBase=rateBase)
policy<-merge(policy,rpo)
policy

## ----Policy ite ave reward over time (sow rep),echo=true-----------------
g<-policyIteAve(mdp, wLbl, durLbl)
policy<-getPolicy(mdp)
rpo<-calcRPO(mdp, wLbl, iA=c(0,0,0), criterion="average", dur=durLbl, rate=rate, rateBase=rateBase)
policy<-merge(policy,rpo)
policy

## ----Policy ite ave reward over litter(sow rep), echo=TRUE---------------
durLbl<-"Piglets"
g<-policyIteAve(mdp, wLbl, dur=durLbl)
policy<-getPolicy(mdp)
rpo<-calcRPO(mdp, wLbl, iA=c(0,0,0), criterion="average", dur=durLbl, rate=rate, rateBase=rateBase)
policy<-merge(policy,rpo)
policy

## ----Policy ite discount (sow rep) again, echo=TRUE----------------------
policyIteDiscount(mdp, wLbl, durLbl, rate, rateBase)
policy<-getPolicy(mdp)
rpo<-calcRPO(mdp, wLbl, iA=c(0,0,0), criterion="discount", dur=durLbl, rate=rate, rateBase=rateBase)
policy<-merge(policy,rpo)
policy

## ----Piglets/time (sow rep), echo=TRUE-----------------------------------
calcWeights(mdp, w="Piglets", criterion="average", dur = "Duration")

## ----Reward/piglet (sow rep), echo=TRUE----------------------------------
calcWeights(mdp, w="Net reward", criterion="average", dur = "Piglets")

## ----Generate cow MDP functions,echo=true--------------------------------
cowDf<-read.csv("mdp_example_files/cow.csv")
head(cowDf)

lev1W<-function(s0Idx,n1Idx,s1Idx,a1Lbl) {
	r<-subset(cowDf,s0==s0Idx & n1==n1Idx & s1==s1Idx & label==a1Lbl)
	return(as.numeric(r[5:7]))
}
lev1W(2,2,1,'Keep')     # good genetic merit, lactation 2, avg yield, keep action

lev1Pr<-function(s0Idx,n1Idx,s1Idx,a1Lbl) {
	r<-subset(cowDf,s0==s0Idx & n1==n1Idx & s1==s1Idx & label==a1Lbl)
	return(as.numeric(r[8:16]))
}
lev1Pr(2,2,1,'Replace') # good genetic merit, lactation 2, avg yield, replace action

## ----Generate cow MDP,echo=true------------------------------------------
lblS0<-c('Bad genetic level','Avg genetic level','Good genetic level')
lblS1<-c('Low yield','Avg yield','High yield')
prefix<-"cow_"
w<-binaryMDPWriter(prefix)
w$setWeights(c("Duration", "Net reward", "Yield"))
w$process()
	w$stage()   # stage 0 at founder level
		for (s0 in 0:2) {
			w$state(label=lblS0[s0+1])   # state at founder
				w$action(label="Keep", weights=c(0,0,0), prob=c(2,0,1))   # action at founder
					w$process()
						w$stage()   # dummy stage at level 1
							 w$state(label="Dummy")
								w$action(label="Dummy", weights=c(0,0,0), prob=c(1,0,1/3, 1,1,1/3, 1,2,1/3))
								w$endAction()
							 w$endState()
						w$endStage()
						for (d1 in 1:4) {
							w$stage()   # stage at level 1
								for (s1 in 0:2) {
									w$state(label=lblS1[s1+1])
										if (d1!=4) {
											w$action(label="Keep", weights=lev1W(s0,d1,s1,"Keep"), prob=lev1Pr(s0,d1,s1,"Keep"))
											w$endAction()
										}
										w$action(label="Replace", weights=lev1W(s0,d1,s1,"Replace"), prob=lev1Pr(s0,d1,s1,"Replace"))
										w$endAction()
									w$endState()
								}
							w$endStage()
						}
					w$endProcess()
				w$endAction()
			w$endState()
		}
	w$endStage()
w$endProcess()
w$closeWriter()

## ----Optimize (cow)------------------------------------------------------
## solve under discount criterion
mdp<-loadMDP(prefix)
wLbl<-"Net reward"         # the weight we want to optimize (net reward)
durLbl<-"Duration"         # the duration/time label
rate<-0.1               # discount rate
rateBase<-1             # rate base, i.e. given a duration of t the rate is
policyIteDiscount(mdp, wLbl, durLbl, rate, rateBase)
policy<-getPolicy(mdp, stateStr = TRUE)
rpo<-calcRPO(mdp, wLbl, iA=rep(0,42), criterion="discount", dur=durLbl, rate=rate, rateBase=rateBase)
policy<-merge(policy,rpo)
policy

## ----Delete bin, include=FALSE-------------------------------------------
#do.call(file.remove,list(list.files(pattern = ".bin")))

