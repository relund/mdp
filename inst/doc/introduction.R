## ----setup, include=FALSE---------------------------------------------------------------
library(knitr)
options(formatR.arrow = TRUE, width=90) # , scipen=999, digits=5
#thm <- knit_theme$get("edit-kwrite")   # whitengrey, bright, print, edit-flashdevelop, edit-kwrite
#knit_theme$set(thm)
opts_chunk$set(fig.align='center', 
               fig.width=10, fig.height=5, 
               fig.show='hold', 
               out.extra='style="max-width:100%;"',
               tidy = TRUE,
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
dat<-infoMDP(mdp,withHarc = TRUE)
stateDF<-dat$stateDF
stateDF$label<-paste0(c(1:N,1:N)-1,": i=",c(1:N,1:N) )
#stateDF$label[1:N]<-""
stateDF$gId<-c(seq(2,10,by=2),seq(1,9,by=2))
actionDF<-dat$harcDF
actionDF$label<-dat$actionDF$label
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

## ----view_transPr, eval=FALSE, ref.label='transPr', echo=TRUE---------------------------
#  # transform state to id
#  state2Id<-function(i) return(i-1)
#  
#  # input state i and action a
#  transPr<-function(a,i) {
#     if (a==0) {
#        pr<-Q[i,]
#        iN<-which(pr>0)
#        pr<-pr[iN]       # only consider trans pr > 0
#     }
#     if (a>0) {
#        pr<-1
#        iN<-1
#     }
#     return(list(pr=pr,id=state2Id(iN)))
#  }
#  transPr(0,1)

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
info<-infoMDP(mdp)  # more detailed info
info$actionDF
info$stateDF

## ----solve1-----------------------------------------------------------------------------
# Optimal policy under average reward per time unit criterion
policyIteAve(mdp,"Net reward","Duration")
getPolicy(mdp)
# Optimal policy under expected discounted reward criterion (use both policy and value ite)
policyIteDiscount(mdp,"Net reward","Duration", discountFactor = 0.9)
getPolicy(mdp)
valueIte(mdp,"Net reward","Duration", discountFactor = 0.9, eps = 1e-10, maxIte = 1000)
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
policyIteAve(mdp,"Net reward","Duration")
getPolicy(mdp)

