## ----ini, echo=FALSE, results='hide', message=FALSE-----------------------------------------------
library(MDP2)
library(knitr)
options(width=100)
opts_knit$set(formatR.arrow=F, concordance=TRUE, global.par=TRUE)
#options(width = 120)
par(mar=c(0,0,0,0))
opts_chunk$set(out.width='100%', fig.width=15, fig.height=7, fig.path='graphic/', fig.align='center', fig.show='hold', fig.retina = 1)

## ----HMDP, tidy=FALSE-----------------------------------------------------------------------------
# function for creating subprocess proc1
Proc1<-function(jumpActions=TRUE) {
   w$process()
      w$stage()   # stage n=0
         w$state(label="good")         
            w$action(label="mt", weights=c(55,1), prob=c(1,0,1), end=TRUE)
            w$action(label="nmt", weights=c(70,1), prob=c(1,0,0.6, 1,1,0.4), end=TRUE)
         w$endState()
         w$state(label="average")        
            w$action(label="mt", weights=c(40,1), prob=c(1,0,1), end=TRUE)
            w$action(label="nmt", weights=c(50,1), prob=c(1,1,0.6, 1,2,0.4), end=TRUE)
         w$endState()
      w$endStage()
      w$stage()   # stage n=1
         w$state(label="good")        
            if (jumpActions) w$action(label="rep", weights=c(30,1), prob=c(0,0,1), end=TRUE)
         w$endState()
         w$state(label="average")     
            if (jumpActions) w$action(label="rep", weights=c(10,1), prob=c(0,0,1), end=TRUE)
         w$endState()
         w$state(label="not working") 
            if (jumpActions) w$action(label="rep", weights=c(5,1), prob=c(0,0,1), end=TRUE)
         w$endState()
      w$endStage()
   w$endProcess()
}


# function for creating subprocess proc2
Proc2<-function(jumpActions=TRUE) {
   w$process()
      w$stage()   # stage n=0
         w$state(label="Dummy")          
            w$action(label="buy", weights=c(-100,1), prob=c(1,0,0.7, 1,1,0.3), end=TRUE)
         w$endState()
      w$endStage()
      w$stage()   # stage n=1
         w$state(label="good")           
            w$action(label="mt", weights=c(55,1), prob=c(1,0,1), end=TRUE)
            w$action(label="nmt", weights=c(70,1), prob=c(1,0,0.6, 1,1,0.4), end=TRUE)
         w$endState()
         w$state(label="average")        
            w$action(label="mt", weights=c(40,1), prob=c(1,0,1), end=TRUE)
            w$action(label="nmt", weights=c(50,1), prob=c(1,1,0.6, 1,2,0.4), end=TRUE)
         w$endState()
      w$endStage()
      w$stage()   # stage n=2
         w$state(label="good")          
            w$action(label="mt", weights=c(55,1), prob=c(1,0,1), end=TRUE)
            w$action(label="nmt", weights=c(70,1), prob=c(1,0,0.5, 1,1,0.5), end=TRUE)
         w$endState()
         w$state(label="average")        
            w$action(label="mt", weights=c(40,1), prob=c(1,0,1), end=TRUE)
            w$action(label="nmt", weights=c(50,1), prob=c(1,1,0.5, 1,2,0.5), end=TRUE)
         w$endState()
         w$state(label="not working")    
            w$action(label="mt", weights=c(30,1), prob=c(1,0,1), end=TRUE)
            w$action(label="rep", weights=c(5,1), prob=c(1,2,1), end=TRUE)    
         w$endState()
      w$endStage()
      w$stage()   # stage n=3
         w$state(label="good")          
            w$action(label="mt", weights=c(55,1), prob=c(1,0,1), end=TRUE)
            w$action(label="nmt", weights=c(70,1), prob=c(1,0,0.2, 1,1,0.8), end=TRUE)
         w$endState()
         w$state(label="average")        
            w$action(label="mt", weights=c(40,1), prob=c(1,0,1), end=TRUE)
            w$action(label="nmt", weights=c(50,1), prob=c(1,1,0.2, 1,2,0.8), end=TRUE)
         w$endState()
         w$state(label="not working")    
            w$action(label="mt", weights=c(30,1), prob=c(1,0,1), end=TRUE)
            w$action(label="rep", weights=c(5,1), prob=c(1,1,1), end=TRUE)
         w$endState()
      w$endStage() 
      w$stage()   # stage n=4
         w$state(label="good")        
            if (jumpActions) w$action(label="rep", weights=c(30,1), prob=c(0,1,1), end=TRUE)   
         w$endState()
         w$state(label="average")     
            if (jumpActions) w$action(label="rep", weights=c(10,1), prob=c(0,1,1), end=TRUE)
         w$endState()
         w$state(label="not working") 
            if (jumpActions) w$action(label="rep", weights=c(5,1), prob=c(0,1,1), end=TRUE)
         w$endState()
      w$endStage()
   w$endProcess()
}

# build the HMDP (without external processes)
prefix<-"hmdp_"
w<-binaryMDPWriter(prefix)
w$setWeights(c("Reward","Time"))
w$process() # founder process with a single state
   w$stage()
      w$state(label="M0")
         w$action(label="A0",weights=c(0,1),prob=c(2,0,1))
            w$process() # level 1 process with 3 stages
               w$stage() # 1. stage with one state
                  w$state(label="D")
                     w$action(label="A0",weights=c(0,1),prob=c(1,0,0.5,1,1,0.5), end=T)
                     w$action(label="A1",weights=c(0,1),prob=c(2,0,0.5,2,1,0.5))
                        Proc1()
                     w$endAction()
                  w$endState()
               w$endStage()
               w$stage()
                  w$state(label="C0")
                     w$action(label="A0",weights=c(0,0),prob=c(2,0,1))
                        Proc1()
                     w$endAction()
                  w$endState()
                  w$state(label="C1")
                     w$action(label="A0",weights=c(0,1),prob=c(2,0,1))
                        Proc2()
                     w$endAction()
                  w$endState()
               w$endStage()
               w$stage()
                  w$state(label="C0")
                     w$action(label="A0",weights=c(4,1),prob=c(0,0,1))
                     w$endAction()
                  w$endState()
                  w$state(label="C1")
                     w$action(label="A0",weights=c(1,1),prob=c(0,0,1))
                     w$endAction()
                  w$endState()
               w$endStage()
            w$endProcess()
         w$endAction()
      w$endState()
   w$endStage()
w$endProcess()
w$closeWriter()

## ----plotHMDP, echo=FALSE, message=FALSE, warning=FALSE, results='hide'---------------------------
prefix<-"hmdp_"
mdpFull<-loadMDP(prefix)

# assign states to grid nodes in the plot
lst<-infoMDP(mdpFull, asStrings = F, withDF = T, withHarc = T)
states<-lst$stateDF
states$label[1]<-'M0'
states$label<-states$sId
states$gId<-NA
getGId<-function(x,y) return((x-1)*gridDim[2]+y)   # grid id given grid coordinates (x=row, y=col)
gridDim<-c(9,12)
for (i in 1:length(states$sId)) {
   if (is.na(states$a0[i])) {
      y<-states$n0[i]*(gridDim[2]-1)+1
      x<-states$s0[i]+1
      #if (states$s0[i]==0) x<-1 else x<-7
   } else  if (is.na(states$a1[i])) {  # at level 1
      y<-ifelse(states$n1[i]==0,2, ifelse(states$n1[i]==1,5,11) )
      x<-states$s1[i]+2
   } else {  # at level 2
      x<-ifelse(states$s1[i]==0, 4, 7)
      y<-ifelse(states$n1[i]==0, 2, 5) + 1
      y<-y+states$n2[i]
      x<-x+states$s2[i]
   }
   states$gId[i]<-getGId(x,y)
}
states<-states[,c('sId','gId','label')]


# assign actions
actions<-lst$harcDF   # an action for each row with head in the first column and the rest tails
colnames(actions)[1]<-"sId"
tmp<-lst$actionDF
actions$label = paste(tmp$label, tmp$w.1, sep=" ") 
actions$lwd<-0.5
actions$lty<-1
actions$col<-"black"
actions$highlight<-F

plotHypergraph(gridDim = gridDim, showGrid = F, devOff = F, states = states, actions = actions )
#dev.copy(pdf,"hmdp.pdf", width=18, height=8)
#dev.off()

## ----proc12---------------------------------------------------------------------------------------

w<-binaryMDPWriter("proc1_")
w$setWeights(c("Reward","Time"))
Proc1(jumpActions = F)
w$closeWriter()

w<-binaryMDPWriter("proc2_")
w$setWeights(c("Reward","Time"))
Proc2(jumpActions = F)
w$closeWriter()

## ----plotProc1, echo=FALSE, results='hide', fig.width=15, fig.height=10---------------------------
prefix<-"proc1_"
mdp1<-loadMDP(prefix)

# assign states to grid nodes in the plot
lst<-infoMDP(mdp1, asStrings = F, withDF = T, withHarc = T)
states<-lst$stateDF
states$label<-states$sId
states$gId<-NA
getGId<-function(x,y) return((x-1)*gridDim[2]+y)   # grid id given grid coordinates (x=row, y=col)
gridDim<-c(3,2)
for (i in 1:length(states$sId)) {
   y<-states$n0[i]+1
   x<-states$s0[i]+1
   states$gId[i]<-getGId(x,y)
}
states<-states[,c('sId','gId','label')]

# assign actions
actions<-lst$harcDF   # an action for each row with head in the first column and the rest tails
colnames(actions)[1]<-"sId"
tmp<-lst$actionDF
actions$label = paste(tmp$label, tmp$w.1, sep=" ") 
actions$lwd<-0.5
actions$lty<-1
actions$col<-"black"
actions$highlight<-F
plotHypergraph(gridDim = gridDim, showGrid = F, devOff = F, states = states, actions = actions )
#dev.copy(pdf,"proc1.pdf", width=6, height=6)
#dev.off()

## ----plotProc2, echo=FALSE, results='hide', fig.width=12, fig.height=6----------------------------
prefix<-"proc2_"
mdp2<-loadMDP(prefix)

# assign states to grid nodes in the plot
lst<-infoMDP(mdp2, asStrings = F, withDF = T, withHarc = T)
states<-lst$stateDF
states$label<-states$sId
states$gId<-NA
getGId<-function(x,y) return((x-1)*gridDim[2]+y)   # grid id given grid coordinates (x=row, y=col)
gridDim<-c(3,5)
for (i in 1:length(states$sId)) {
   y<-states$n0[i]+1
   x<-states$s0[i]+1
   states$gId[i]<-getGId(x,y)
}
states<-states[,c('sId','gId','label')]

# assign actions
actions<-lst$harcDF   # an action for each row with head in the first column and the rest tails
colnames(actions)[1]<-"sId"
tmp<-lst$actionDF
actions$label = paste(tmp$label, tmp$w.1, sep=" ") 
actions$lwd<-0.5
actions$lty<-1
actions$col<-"black"
actions$highlight<-F
plotHypergraph(gridDim = gridDim, showGrid = F, devOff = F, states = states, actions = actions )
#dev.copy(pdf,"proc2.pdf", width=7, height=5)
#dev.off()

## ----buildExternal, size="tiny"-------------------------------------------------------------------
# build the HMDP (without external processes)
prefix<-"ext_"
w<-binaryMDPWriter(prefix)
w$setWeights(c("Reward","Time"))
w$process() # founder process with a single state
   w$stage()
      w$state(label="M0")
         w$action(label="A0",weights=c(0,1),prob=c(2,0,1))
            w$process() # level 1 process with 3 stages
               w$stage() # 1. stage with one state
                  w$state(label="D")
                     w$action(label="A0",weights=c(0,1),prob=c(1,0,0.5,1,1,0.5), end=T)
                        w$includeProcess(prefix="proc1_", label="A1", weights=c(0,1),prob=c(2,0,0.5,2,1,0.5), termStates=3)
                           w$stage()   # last stage in in external process
                              w$state(label="good")       
                                 w$action(label="rep", weights=c(30,1), prob=c(0,0,1), end=TRUE)
                              w$endState()
                              w$state(label="average")     
                                 w$action(label="rep", weights=c(10,1), prob=c(0,0,1), end=TRUE)
                              w$endState()
                              w$state(label="not working") 
                                 w$action(label="rep", weights=c(5,1), prob=c(0,0,1), end=TRUE)
                              w$endState()
                           w$endStage()
                        w$endIncludeProcess()
                  w$endState()
               w$endStage()
               w$stage()
                  w$state(label="C0")
                     w$includeProcess(prefix="proc1_", label="A0",weights=c(0,0),prob=c(2,0,1), termStates=3)
                        w$stage()   # last stage in in external process
                           w$state(label="good")        
                              w$action(label="rep", weights=c(30,1), prob=c(0,0,1), end=TRUE)
                           w$endState()
                           w$state(label="average")     
                              w$action(label="rep", weights=c(10,1), prob=c(0,0,1), end=TRUE)
                           w$endState()
                           w$state(label="not working") 
                              w$action(label="rep", weights=c(5,1), prob=c(0,0,1), end=TRUE)
                           w$endState()
                        w$endStage()
                     w$endIncludeProcess()
                  w$endState()
                  w$state(label="C1")
                     w$includeProcess(prefix="proc2_", label="A0",weights=c(0,1),prob=c(2,0,1), termStates=3)
                        w$stage()   # last stage in in external process
                           w$state(label="good")       
                              w$action(label="rep", weights=c(30,1), prob=c(0,1,1), end=TRUE)
                           w$endState()
                           w$state(label="average")    
                              w$action(label="rep", weights=c(10,1), prob=c(0,1,1), end=TRUE)
                           w$endState()
                           w$state(label="not working") 
                              w$action(label="rep", weights=c(5,1), prob=c(0,1,1), end=TRUE)
                           w$endState()
                        w$endStage()
                     w$endIncludeProcess()
                  w$endState()
               w$endStage()
               w$stage()
                  w$state(label="C0")
                     w$action(label="A0",weights=c(4,1),prob=c(0,0,1))
                     w$endAction()
                  w$endState()
                  w$state(label="C1")
                     w$action(label="A0",weights=c(1,1),prob=c(0,0,1))
                     w$endAction()
                  w$endState()
               w$endStage()
            w$endProcess()
         w$endAction()
      w$endState()
   w$endStage()
w$endProcess()
w$closeWriter()

## ----bin------------------------------------------------------------------------------------------
fileN<-"ext_externalProcesses.bin"
readBin(fileN, character(),n=file.info(fileN)$size)

## ----plotExternalHMDP, echo=FALSE, results='hide'-------------------------------------------------
prefix<-"ext_"
mdpExt<-loadMDP(prefix, verbose = T)

# assign states to grid nodes in the plot
lst<-infoMDP(mdpExt, asStrings = F, withDF = T, withHarc = T)
states<-lst$stateDF
states$label[1]<-'M0'
states$label<-states$sId
states$gId<-NA
getGId<-function(x,y) return((x-1)*gridDim[2]+y)   # grid id given grid coordinates (x=row, y=col)
gridDim<-c(9,12)
for (i in 1:length(states$sId)) {
   if (is.na(states$a0[i])) {
   y<-states$n0[i]*(gridDim[2]-1)+1
      x<-states$s0[i]+1
      #if (states$s0[i]==0) x<-1 else x<-7
   } else  if (is.na(states$a1[i])) {  # at level 1
      y<-ifelse(states$n1[i]==0,2, ifelse(states$n1[i]==1,5,11) )
      x<-states$s1[i]+2
   } else {  # at level 2
      x<-ifelse(states$s1[i]==0, 4, 7)
      y<-ifelse(states$n1[i]==0, 2, 5) + 1
      y<-y+states$n2[i]
      if(states$s1[i]==1 & y==7) y<-10
      x<-x+states$s2[i]
   }
   states$gId[i]<-getGId(x,y)
}
states<-states[,c('sId','gId','label')]

# assign actions
actions<-lst$harcDF   # an action for each row with head in the first column and the rest tails
colnames(actions)[1]<-"sId"
tmp<-lst$actionDF
actions$label = paste(tmp$label, tmp$w.1, sep=" ") 
actions$lwd<-0.5
actions$lty<-1
actions$col<-"black"
actions$highlight<-F

plotHypergraph(gridDim = gridDim, showGrid = F, devOff = F, states = states, actions = actions )
#dev.copy(pdf,"ext.pdf", width=18, height=8)
#dev.off()

## ----stageStr-------------------------------------------------------------------------------------
infoMDP(mdpExt, stageStr = mdpExt$external$stageStr)$stateDF

## ----testHMDP-------------------------------------------------------------------------------------
gFull<-policyIteAve(mdpFull, "Reward", "Time")
gExt<-policyIteAve(mdpExt, "Reward", "Time")
gFull == gExt

## ----testExt--------------------------------------------------------------------------------------
idx <- data.frame(full=c(0:5,14:18,20:28), ext=0:19)
polFull<-getPolicy(mdpFull,sId = idx$full)
polExt<-getPolicy(mdpExt, sId = idx$ext)
polFull
polFull$weight == polExt$weight

## ----policyExt------------------------------------------------------------------------------------
# process 1 optimal policy - external at stage "0,0,0,0,0,1,0"
lastStage<-"0,0,0,0,0,1,1"
termValues <- getPolicy(mdpExt, stageStr = lastStage)$weight
valueIte(mdp1, 'Reward', 'Time', termValues=termValues, g=gExt)
polFull<-getPolicy(mdpFull,sId = 22:26)
polExt<-getPolicy(mdp1)
cbind(polFull,polExt)

# process 1 optimal policy - external at stage "0,0,0,1,0,0,0"
lastStage<-"0,0,0,1,0,0,1"
termValues <- getPolicy(mdpExt, stageStr = lastStage)$weight
valueIte(mdp1, 'Reward', 'Time', termValues=termValues, g=gExt)
polFull<-getPolicy(mdpFull,sId = 15:18)
polExt<-getPolicy(mdp1, sId=0:3)
cbind(polFull,polExt)

# process 2 optimal policy - external at stage "0,0,0,1,1,0,0"
lastStage<-"0,0,0,1,1,0,1"
termValues <- getPolicy(mdpExt, stageStr = lastStage)$weight
valueIte(mdp2, 'Reward', 'Time', termValues=termValues, g=gExt)
polFull<-getPolicy(mdpFull,sId = 3:14)
polExt<-getPolicy(mdp2)
cbind(polFull,polExt)

## ----policyExt getPolicy--------------------------------------------------------------------------
getPolicy(mdpExt, external = mdpExt$external$stageStr, w = 'Reward', dur = 'Time', g = gExt)

## ----build----------------------------------------------------------------------------------------
buildHMDP()

## ----bin c++--------------------------------------------------------------------------------------
fileN<-"ext_externalProcesses.bin"
readBin(fileN, character(),n=file.info(fileN)$size)

## ----policyIte------------------------------------------------------------------------------------
prefix<-"ext_"
mdp<-loadMDP(prefix, verbose = T)
policyIteAve(mdp, "Reward", "Time", maxIte = 5)
getPolicy(mdp)

## ----Delete bin, include=FALSE--------------------------------------------------------------------
#unlink("./graphi*", recursive = T, force = T)
#do.call(file.remove,list(list.files(pattern = ".bin")))

