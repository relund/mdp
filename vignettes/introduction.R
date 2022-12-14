## ----setup, include=FALSE---------------------------------------------------------------
library(knitr)
options(formatR.arrow = TRUE, width=90) # , scipen=999, digits=5
#thm <- knit_theme$get("edit-kwrite")   # whitengrey, bright, print, edit-flashdevelop, edit-kwrite
#knit_theme$set(thm)
opts_chunk$set(fig.align='center', 
               fig.width=10, fig.height=5, 
               fig.show='hold', 
               out.extra='style="max-width:100%;"',
               # tidy = TRUE,
               prompt=T,
               comment=NA,
               cache=F, 
               background="red")

## ----Install github, echo=TRUE, eval=FALSE----------------------------------------------
#  install_github("relund/mdp")

## ----Load package, echo=TRUE, warning=FALSE, results='hide', message=FALSE--------------
library(MDP2)

## ----Package help, echo=TRUE, eval=FALSE------------------------------------------------
#  ?MDP2

## ----parameters,  include=FALSE---------------------------------------------------------
N<-5; Cf<- -10; Cp<-c(0,-7,-7,-5) # use negative numbers since the MDP optimize based on rewards
Q <- matrix(c(
   0.90, 0.10, 0, 0, 0,
   0, 0.80, 0.10, 0.05, 0.05,
   0, 0, 0.70, 0.10, 0.20,
   0, 0, 0, 0.50, 0.50), nrow=4, byrow=T) 

## ----Qtable, results='asis', echo=FALSE-------------------------------------------------
rownames(Q)<-1:4
colnames(Q)<-1:5
knitr::kable(Q, row.names = T)

## ----states, include=FALSE--------------------------------------------------------------
states<-data.frame(id=1:N-1,label=paste0("i=",1:N), stringsAsFactors = F)
states

## ----transPr, include=FALSE-------------------------------------------------------------
# transform state to id
state2Id<-function(i) return(i-1)

# input state i and action a
transPr<-function(a,i) {
   if (a==0) {
      pr<-Q[i,]
      iN<-which(pr>0)
      pr<-pr[iN]       # only consider trans pr > 0
   }
   if (a>0) {
      pr<-1
      iN<-1
   }
   return(list(pr=pr,id=state2Id(iN)))
}
transPr(0,1)

## ----buildMDP1, include=FALSE-----------------------------------------------------------
# Build the model which is stored in a set of binary files
w<-binaryMDPWriter("hct611-1_")
w$setWeights(c("Duration","Net reward"))
w$process()
   w$stage()
      w$state(label="i=1")
         dat<-transPr(0,1)
         w$action(label="no repair", weights=c(1,0), pr=dat$pr, id=dat$id, end=T)
      w$endState()
      for (ii in 2:(N-1) ) {
         w$state(label=states$label[ii])
            dat<-transPr(0,ii)
            w$action(label="no repair", weights=c(1,0), pr=dat$pr, id=dat$id, end=T)
            dat<-transPr(1,ii)
            w$action(label="preventive repair", weights=c(1,Cp[ii]), pr=dat$pr, id=dat$id, end=T)
         w$endState()
      }
      w$state(label=paste0("i=",N))
         dat<-transPr(2,N)
         w$action(label="forced repair", weights=c(2,Cf), pr=dat$pr, id=dat$id, end=T)
      w$endState()
   w$endStage()
w$endProcess()
w$closeWriter()

## ----plotHgf, echo=FALSE, results='hide', message=FALSE---------------------------------
mdp<-loadMDP("hct611-1_")
dat <- getInfo(mdp, withHarc = TRUE)
stateDF<-dat$df
stateDF$label<-paste0(c(1:N,1:N)-1,": i=",c(1:N,1:N) )
#stateDF$label[1:N]<-""
stateDF$gId<-c(seq(2,10,by=2),seq(1,9,by=2))
actionDF<-dat$harcDF
actionDF$lwd<-0.5
actionDF$lty[actionDF$label=="no repair"]<-1
actionDF$lty[actionDF$label=="preventive repair"]<-1
actionDF$lty[actionDF$label=="forced repair"]<-1
actionDF$col[actionDF$label=="no repair"]<-"darkorange1"
actionDF$col[actionDF$label=="preventive repair"]<-"deepskyblue3"
actionDF$col[actionDF$label=="forced repair"]<-"chartreuse4"
actionDF$highlight<-FALSE
par(mai=c(0,0,0,0))
plotHypergraph(gridDim=c(N,2), states = stateDF, actions = actionDF)

## ----view_param, eval=FALSE, ref.label='parameters', echo=TRUE--------------------------
#  N<-5; Cf<- -10; Cp<-c(0,-7,-7,-5) # use negative numbers since the MDP optimize based on rewards
#  Q <- matrix(c(
#     0.90, 0.10, 0, 0, 0,
#     0, 0.80, 0.10, 0.05, 0.05,
#     0, 0, 0.70, 0.10, 0.20,
#     0, 0, 0, 0.50, 0.50), nrow=4, byrow=T)

## ----view_states, eval=TRUE, ref.label='states', echo=TRUE------------------------------
states<-data.frame(id=1:N-1,label=paste0("i=",1:N), stringsAsFactors = F)
states

## ----view_transPr, eval=TRUE, ref.label='transPr', echo=TRUE----------------------------
# transform state to id
state2Id<-function(i) return(i-1)

# input state i and action a
transPr<-function(a,i) {
   if (a==0) {
      pr<-Q[i,]
      iN<-which(pr>0)
      pr<-pr[iN]       # only consider trans pr > 0
   }
   if (a>0) {
      pr<-1
      iN<-1
   }
   return(list(pr=pr,id=state2Id(iN)))
}
transPr(0,1)

## ----view_buldMDP1, eval=FALSE, ref.label='buildMDP1', echo=TRUE, tidy=FALSE------------
#  # Build the model which is stored in a set of binary files
#  w<-binaryMDPWriter("hct611-1_")
#  w$setWeights(c("Duration","Net reward"))
#  w$process()
#     w$stage()
#        w$state(label="i=1")
#           dat<-transPr(0,1)
#           w$action(label="no repair", weights=c(1,0), pr=dat$pr, id=dat$id, end=T)
#        w$endState()
#        for (ii in 2:(N-1) ) {
#           w$state(label=states$label[ii])
#              dat<-transPr(0,ii)
#              w$action(label="no repair", weights=c(1,0), pr=dat$pr, id=dat$id, end=T)
#              dat<-transPr(1,ii)
#              w$action(label="preventive repair", weights=c(1,Cp[ii]), pr=dat$pr, id=dat$id, end=T)
#           w$endState()
#        }
#        w$state(label=paste0("i=",N))
#           dat<-transPr(2,N)
#           w$action(label="forced repair", weights=c(2,Cf), pr=dat$pr, id=dat$id, end=T)
#        w$endState()
#     w$endStage()
#  w$endProcess()
#  w$closeWriter()

## ----load-------------------------------------------------------------------------------
mdp<-loadMDP("hct611-1_")
mdp # overall info
info<-getInfo(mdp)  # more detailed info
info$df

## ----solve1_ave-------------------------------------------------------------------------
# Optimal policy under average reward per time unit criterion
runPolicyIteAve(mdp,"Net reward","Duration")
getPolicy(mdp)

## ----solve1_discount--------------------------------------------------------------------
# Optimal policy under expected discounted reward criterion (use both policy and value ite)
runPolicyIteDiscount(mdp,"Net reward","Duration", discountFactor = 0.5)
getPolicy(mdp)
runValueIte(mdp,"Net reward","Duration", discountFactor = 0.5, eps = 1e-10, maxIte = 1000)
getPolicy(mdp)

## ----buildMDP2--------------------------------------------------------------------------
## define probability matrices
P<-list()
# a=1 (no repair)
P[[1]]<-as.matrix(rbind(Q,0))
# a=2 (preventive repair)
Z <- matrix(0, nrow = N, ncol = N)
Z[2,1]<-Z[3,1]<-Z[4,1]<-1
P[[2]]<-Z
# a=3 (forced repair)
Z <- matrix(0, nrow = N, ncol = N)
Z[5,1]<-1
P[[3]]<-Z
# reward 6x3 matrix with one column for each action
R <- matrix(0, nrow = N, ncol = 3)
R[2:4,2]<-Cp[2:4]
R[5,3]<-Cf
# state lengths
D <- matrix(1, nrow = N, ncol = 3)
D[5,3]<-2

# build model
w<-binaryMDPWriter("hct611-2_")
w$setWeights(c("Duration","Net reward"))
w$process(P,R,D)
w$closeWriter()

## ----solve2-----------------------------------------------------------------------------
mdp<-loadMDP("hct611-2_")
runPolicyIteAve(mdp,"Net reward","Duration")
getPolicy(mdp)

## ----buildMDP3, include=FALSE-----------------------------------------------------------
prefix<-"machine1_"
w <- binaryMDPWriter(prefix)
w$setWeights(c("Net reward"))
w$process()
	w$stage()   # stage n=0
		w$state(label="Dummy")       
			w$action(label="buy", weights=-100, pr=c(0.7,0.3), id=c(0,1), end=TRUE)
		w$endState()
	w$endStage()
	w$stage()   # stage n=1
		w$state(label="good")           
			w$action(label="mt", weights=55, pr=1, id=0, end=TRUE)
			w$action(label="nmt", weights=70, pr=c(0.6,0.4), id=c(0,1), end=TRUE)
		w$endState()
		w$state(label="average")        
			w$action(label="mt", weights=40, pr=1, id=0, end=TRUE)
			w$action(label="nmt", weights=50, pr=c(0.6,0.4), id=c(1,2), end=TRUE)
		w$endState()
	w$endStage()
	w$stage()   # stage n=2
		w$state(label="good")          
			w$action(label="mt", weights=55, pr=1, id=0, end=TRUE)
			w$action(label="nmt", weights=70, pr=c(0.5,0.5), id=c(0,1), end=TRUE)
		w$endState()
		w$state(label="average")       
			w$action(label="mt", weights=40, pr=1, id=0, end=TRUE)
			w$action(label="nmt", weights=50, pr=c(0.5,0.5), id=c(1,2), end=TRUE)
		w$endState()
		w$state(label="not working")    
			w$action(label="mt", weights=30, pr=1, id=0, end=TRUE)
			w$action(label="rep", weights=5, pr=1, id=3, end=TRUE)
		w$endState()
	w$endStage()
	w$stage()   # stage n=3
		w$state(label="good")           
			w$action(label="mt", weights=55, pr=1, id=0, end=TRUE)
			w$action(label="nmt", weights=70, pr=c(0.2,0.8), id=c(0,1), end=TRUE)
		w$endState()
		w$state(label="average")       
			w$action(label="mt", weights=40, pr=1, id=0, end=TRUE)
			w$action(label="nmt", weights=50, pr=c(0.2,0.8), id=c(1,2), end=TRUE)
		w$endState()
		w$state(label="not working")    
			w$action(label="mt", weights=30, pr=1, id=0, end=TRUE)
			w$action(label="rep", weights=5, pr=1, id=3, end=TRUE)
		w$endState()
		w$state(label="replaced")       
			w$action(label="Dummy", weights=0, pr=1, id=3, end=TRUE)
		w$endState()
	w$endStage()
	w$stage()   # stage n=4
		w$state(label="good", end=TRUE)        
		w$state(label="average", end=TRUE)     
		w$state(label="not working", end=TRUE) 
		w$state(label="replaced", end=TRUE)   
	w$endStage()
w$endProcess()
w$closeWriter()

## ----plotHgf3, echo=FALSE---------------------------------------------------------------
scrapValues<-c(30,10,5,0)   # scrap values (the values of the 4 states at stage 4)
mdp<-loadMDP("machine1_", getLog = FALSE)
dat<-getInfo(mdp, withHarc = TRUE)
stateDF<-dat$df
stateDF$gId<-c(5,10,15,20,4,9,14,19,3,8,13,2,7,1)
actionDF<-dat$harcDF
actionDF$lwd<-0.5
actionDF$lwd<-1
actionDF$col<-"deepskyblue3"
actionDF$highlight<-FALSE
par(mai=c(0,0.1,0,0.1))
plotHypergraph(gridDim=c(4,5), states = stateDF, actions = actionDF, radx = 0.06, marX = 0.06)

## ----view_buildMDP3, eval=FALSE, ref.label='buildMDP3', echo=TRUE, tidy=FALSE-----------
#  prefix<-"machine1_"
#  w <- binaryMDPWriter(prefix)
#  w$setWeights(c("Net reward"))
#  w$process()
#  	w$stage()   # stage n=0
#  		w$state(label="Dummy")
#  			w$action(label="buy", weights=-100, pr=c(0.7,0.3), id=c(0,1), end=TRUE)
#  		w$endState()
#  	w$endStage()
#  	w$stage()   # stage n=1
#  		w$state(label="good")
#  			w$action(label="mt", weights=55, pr=1, id=0, end=TRUE)
#  			w$action(label="nmt", weights=70, pr=c(0.6,0.4), id=c(0,1), end=TRUE)
#  		w$endState()
#  		w$state(label="average")
#  			w$action(label="mt", weights=40, pr=1, id=0, end=TRUE)
#  			w$action(label="nmt", weights=50, pr=c(0.6,0.4), id=c(1,2), end=TRUE)
#  		w$endState()
#  	w$endStage()
#  	w$stage()   # stage n=2
#  		w$state(label="good")
#  			w$action(label="mt", weights=55, pr=1, id=0, end=TRUE)
#  			w$action(label="nmt", weights=70, pr=c(0.5,0.5), id=c(0,1), end=TRUE)
#  		w$endState()
#  		w$state(label="average")
#  			w$action(label="mt", weights=40, pr=1, id=0, end=TRUE)
#  			w$action(label="nmt", weights=50, pr=c(0.5,0.5), id=c(1,2), end=TRUE)
#  		w$endState()
#  		w$state(label="not working")
#  			w$action(label="mt", weights=30, pr=1, id=0, end=TRUE)
#  			w$action(label="rep", weights=5, pr=1, id=3, end=TRUE)
#  		w$endState()
#  	w$endStage()
#  	w$stage()   # stage n=3
#  		w$state(label="good")
#  			w$action(label="mt", weights=55, pr=1, id=0, end=TRUE)
#  			w$action(label="nmt", weights=70, pr=c(0.2,0.8), id=c(0,1), end=TRUE)
#  		w$endState()
#  		w$state(label="average")
#  			w$action(label="mt", weights=40, pr=1, id=0, end=TRUE)
#  			w$action(label="nmt", weights=50, pr=c(0.2,0.8), id=c(1,2), end=TRUE)
#  		w$endState()
#  		w$state(label="not working")
#  			w$action(label="mt", weights=30, pr=1, id=0, end=TRUE)
#  			w$action(label="rep", weights=5, pr=1, id=3, end=TRUE)
#  		w$endState()
#  		w$state(label="replaced")
#  			w$action(label="Dummy", weights=0, pr=1, id=3, end=TRUE)
#  		w$endState()
#  	w$endStage()
#  	w$stage()   # stage n=4
#  		w$state(label="good", end=TRUE)
#  		w$state(label="average", end=TRUE)
#  		w$state(label="not working", end=TRUE)
#  		w$state(label="replaced", end=TRUE)
#  	w$endStage()
#  w$endProcess()
#  w$closeWriter()

## ----load3------------------------------------------------------------------------------
mdp<-loadMDP("machine1_")
mdp # overall info
info<-getInfo(mdp)  # more detailed info
info$df

## ----solve3-----------------------------------------------------------------------------
scrapValues<-c(30,10,5,0)   # scrap values (the values of the 4 states at stage 4)
runValueIte(mdp, "Net reward" , termValues=scrapValues)
getPolicy(mdp)

## ----plotPolicy3, echo=FALSE, results='hide', message=FALSE-----------------------------
library(magrittr)
actionDF <- dplyr::left_join(getPolicy(mdp), dat$harcDF, by = c("sId" = "head", "actionLabel" = "label")) %>% 
   dplyr::filter(aIdx >= 0) %>% 
   dplyr::select(head = sId, contains("tail"), label = actionLabel)
actionDF$lwd<-1
actionDF$col<-"deepskyblue3"
actionDF$highlight<-FALSE
par(mai=c(0,0.1,0,0.1))
plotHypergraph(gridDim=c(4,5), states = stateDF, actions = actionDF, radx = 0.06, marX = 0.06)

## ----Set policy (machine rep),echo=TRUE,eval=TRUE---------------------------------------
policy<-data.frame(sId=c(8,11),aIdx=c(0,0))
setPolicy(mdp, policy)
getPolicy(mdp)

## ----Calc reward (machine rep),echo=TRUE------------------------------------------------
runCalcWeights(mdp, "Net reward", termValues=scrapValues)
getPolicy(mdp)    

## ----Generate cow MDP functions,echo=TRUE-----------------------------------------------
cowDf<-read.csv("vignette_files/cow.csv")
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

## ----Generate cow MDP,echo=TRUE, tidy=FALSE---------------------------------------------
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
								w$action(label="Dummy", weights=c(0,0,0), 
								         prob=c(1,0,1/3, 1,1,1/3, 1,2,1/3), end=TRUE)
							 w$endState()
						w$endStage()
						for (d1 in 1:4) {
							w$stage()   # stage at level 1
								for (s1 in 0:2) {
									w$state(label=lblS1[s1+1])
										if (d1!=4) {
											w$action(label="Keep", weights=lev1W(s0,d1,s1,"Keep"), 
											         prob=lev1Pr(s0,d1,s1,"Keep"), end=TRUE)
										}
										w$action(label="Replace", weights=lev1W(s0,d1,s1,"Replace"), 
										         prob=lev1Pr(s0,d1,s1,"Replace"), end=TRUE)
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

## ----plotHMDP, echo=FALSE, results='hide', message=FALSE--------------------------------
mdp<-loadMDP(prefix)
dat<-getInfo(mdp, withHarc = TRUE)
stateDF<-dat$df
stateDF$label[stateDF$label==""]<-c("Bad","Avg","Good")
stateDF$label[stateDF$label=="Low yield"]<-"L"
stateDF$label[stateDF$label=="Avg yield"]<-"A"
stateDF$label[stateDF$label=="High yield"]<-"H"
stateDF$label[stateDF$label=="Dummy"]<-"D"
stateDF$label[stateDF$label=="Bad genetic level"]<-"Bad"
stateDF$label[stateDF$label=="Avg genetic level"]<-"Avg"
stateDF$label[stateDF$label=="Good genetic level"]<-"Good"
stateDF$gId <- NA
stateDF$gId[1:3]<-c(7,14,21)
stateDF$gId[43:45]<-c(1,8,15)
getGId<-function(process,stage,state) {
   if (process==0) start=23
   if (process==1) start=51
   if (process==2) start=79
   return(start + stage + 7*state)
}
idx<-43
for (process in 0:2)
   for (stage in 0:4)
      for (state in 0:2) {
         if (stage==0 & state>0) break
         idx<-idx-1
         #cat(idx,process,stage,state,getGId(process,stage,state),"\n")
         stateDF$gId[idx]<-getGId(process,stage,state)
      }
actionDF<-dat$harcDF
actionDF$label[actionDF$label=="Replace"]<-"R"
actionDF$label[actionDF$label=="Keep"]<-"K"
actionDF$label[actionDF$label=="Dummy"]<-"D"
actionDF$lwd<-0.5
actionDF$lty[actionDF$label=="K"]<-1
actionDF$lty[actionDF$label=="R"]<-1
actionDF$lty[actionDF$label=="D"]<-1
actionDF$col[actionDF$label=="K"]<-"darkorange1"
actionDF$col[actionDF$label=="R"]<-"deepskyblue3"
actionDF$col[actionDF$label=="D"]<-"black"
actionDF$col[67:69]<-"black"
actionDF$highlight<-FALSE
actionDF$label<-""
par(mai=c(0,0,0,0))
plotHypergraph(gridDim=c(14,7), states = stateDF, actions = actionDF, cex = 0.8)

## ----Optimize (cow), tidy.opts=list(comment=FALSE)--------------------------------------
## solve under discount criterion
mdp<-loadMDP(prefix)
wLbl<-"Net reward"         # the weight we want to optimize (net reward)
durLbl<-"Duration"         # the duration/time label
runPolicyIteDiscount(mdp, wLbl, durLbl, rate=0.1)
getPolicy(mdp)
# rpo<-calcRPO(mdp, wLbl, iA=rep(0,42), criterion="discount", dur=durLbl, rate=rate, rateBase=rateBase)
# policy<-merge(policy,rpo)
# policy

## ----plotPolicy, echo=FALSE, results='hide', message=FALSE------------------------------
actionDF <- dplyr::left_join(getPolicy(mdp), dat$harcDF, by = c("sId" = "head", "actionLabel" = "label")) %>% 
   dplyr::filter(aIdx >= 0) %>% 
   dplyr::select(head = sId, contains("tail"), label = actionLabel)
# actionDF<-cbind(dat$actionDF,dat$harcDF)
# actionDF<-merge(actionDF,getPolicy(mdp))[,c("head","tail2","tail3","tail4","label","stateLabel")]
actionDF$label[actionDF$label=="Replace"]<-"R"
actionDF$label[actionDF$label=="Keep"]<-"K"
actionDF$label[actionDF$label=="Dummy"]<-"D"
actionDF$lwd<-0.5
actionDF$lty[actionDF$label=="K"]<-1
actionDF$lty[actionDF$label=="R"]<-1
actionDF$lty[actionDF$label=="D"]<-1
actionDF$col[actionDF$label=="K"]<-"darkorange1"
actionDF$col[actionDF$label=="R"]<-"deepskyblue3"
actionDF$col[actionDF$label=="D"]<-"black"
actionDF$col[35:37]<-"black"
actionDF$highlight<-FALSE
actionDF$label<-""
par(mai=c(0,0,0,0))
plotHypergraph(gridDim=c(14,7), states = stateDF, actions = actionDF, cex = 0.8)

## ----avePerLac, tidy.opts=list(comment=FALSE)-------------------------------------------
wLbl<-"Net reward"         # the weight we want to optimize (net reward)
durLbl<-"Duration"         # the duration/time label
runPolicyIteAve(mdp, wLbl, durLbl)
getPolicy(mdp)

## ----Piglets/time (sow rep), echo=TRUE--------------------------------------------------
runCalcWeights(mdp, w=wLbl, criterion="average", dur = "Yield")

## ----Reward/piglet (sow rep), echo=TRUE-------------------------------------------------
runCalcWeights(mdp, w="Yield", criterion="average", dur = durLbl)

## ----Delete bin, include=FALSE----------------------------------------------------------
do.call(file.remove,list(list.files(pattern = ".bin")))

