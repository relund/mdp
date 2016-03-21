## Excercise 6.7 in Tijms, H.C., "A first course in stochastic models", John Wiley & Sons Ltd, 2003.
## The semi-MDP is specified using binaryMDPWriter and actions with index and pr

MVal<-5
K<-200
a<-c(60,30,50,25,100)
P<-matrix(c(
   0.5,0.25,0.375,0.3,0.50,
   0.3,0.50,0.375,0.5,0.25,
   0.2,0.25,0.250,0.2,0.25), ncol=MVal, byrow=T)
sumP<-matrix(0,nrow=MVal+2,ncol=MVal)    # sumP[stock-1,town] = sum of pr at and above stock-1
for (r in 1:3) {
   for (c in 1:MVal) {
      sumP[r,c] <- sum(P[r:3,c])
   }
}

states<-expand.grid(s=0:MVal,j=1:5)    # define states in a data frame
states$label<-paste("(",states$s,",",states$j,")",sep="")
# set costs for each action
states$a0cost<-states$a1cost<-NA
for (i in 1:length(states$s)) {
   s<-states$s[i]
   j<-states$j[i]
   states$a0cost[i]<-K*sumP[(s+1)+1,j]
   states$a1cost[i]<-a[j]
}
# set trans pr matrix for each action
a0TransPr<-a1TransPr<-matrix(0, nrow=length(states$s), ncol=length(states$s))
for (i in 1:length(states$s)) {
   s<-states$s[i]
   j<-states$j[i]
   for (k in 1:length(states$s)) {
      h<-states$s[k]
      nextTown<-ifelse(j==5, 1, j+1)
      if (states$j[k]!=nextTown) next
      a0TransPr[i,k]<- ifelse(h==0, sumP[s+1,nextTown], ifelse(h<=s & s-h<3, P[s-h+1,nextTown], 0) )
      a1TransPr[i,k]<- ifelse(MVal-h<3, P[MVal-h+1,nextTown], 0)
   }
}


transPr<-function(a,rowId) {
   if (a==0) {
      idx<-which(a0TransPr[rowId,]>0)
      pr<-a0TransPr[rowId,idx]
      return(pr)
   }
   if (a==1) {
      idx<-which(a1TransPr[rowId,]>0)
      pr<-a1TransPr[rowId,idx]
      return(pr)
   }
   return(NULL)
}

idx<-function(a,rowId) {
   if (a==0) {
      idx<-which(a0TransPr[rowId,]>0)
      return(idx-1)
   }
   if (a==1) {
      idx<-which(a1TransPr[rowId,]>0)
      return(idx-1)
   }
   return(NULL)
}

w<-binaryMDPWriter("hct67v2_", getLog = FALSE)
w$setWeights(c("Duration","Net reward"))
w$process()
   w$stage()
      for (ii in 1:length(states$j)) {
         j<-states$j[ii]; s<-states$s[ii]
         w$state(label=states$label[ii])
            w$action(label="0", weights=c(1, -states$a0cost[ii]), pr=transPr(0,ii), index=idx(0,ii), end=T)
            w$action(label="1", weights=c(1, -states$a1cost[ii]), pr=transPr(1,ii), index=idx(1,ii), end=T)
         w$endState()
      }
   w$endStage()
w$endProcess()
w$closeWriter()
