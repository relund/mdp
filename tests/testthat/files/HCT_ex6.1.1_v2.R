## Example 6.1.1 in Tijms, H.C., "A first course in stochastic models", John Wiley & Sons Ltd, 2003.
## The semi-MDP is specified using binaryMDPWriter and actions with id and pr


N<-5; Cf<- -10; Cp<-c(0,-7,-7,-5) # use negative numbers since optimize reward
Q <- matrix(c(
   0.90, 0.10, 0, 0, 0,
   0, 0.80, 0.10, 0.05, 0.05,
   0, 0, 0.70, 0.10, 0.20,
   0, 0, 0, 0.50, 0.50), nrow=4, byrow=T) 
# note that we index from 0, i.e. state N correspond to index N-1 (in j col)
transPr0<-data.frame(i=rep(1:(N-1), times=N), j=rep(1:N,each=4)-1, pr=as.vector(Q))
transPr0<-transPr0[transPr0$pr>0,]  # remove transitions with zero pr
transPr1<-data.frame(i=2:(N-1), j=1-1, pr=rep(1, times=N-2))
getTransPr<-function(a,i) {
   if (a==0) return(transPr0$pr[transPr0$i==i])
   if (a==1) return(transPr1$pr[transPr1$i==i])
   return(stop("Error"))
}
getIdx<-function(a,i) {
   if (a==0) return(transPr0$j[transPr0$i==i])
   if (a==1) return(transPr1$j[transPr1$i==i])
   return(stop("Error"))
}

w<-binaryMDPWriter("hct611v2_", getLog = FALSE)
w$setWeights(c("Duration","Net reward"))
w$process()
   w$stage()
      w$state(label="i=1")
         w$action(label="no repair", weights=c(1,0), id=getIdx(a=0,i=1), pr=getTransPr(a=0,i=1), end=T)
      w$endState()
      for (ii in 2:(N-1) ) {
      w$state(label=paste("i=",ii,sep=""))
         w$action(label="no repair", weights=c(1,0), id=getIdx(a=0,i=ii), pr=getTransPr(a=0,i=ii), end=T)
         w$action(label="preventive repair", weights=c(1,Cp[ii]), id=getIdx(a=1,i=ii), pr=getTransPr(a=1,i=ii), end=T)
      w$endState()
      }
      w$state(label=paste("i=",N,sep=""))
         w$action(label="forced repair", weights=c(2,Cf), id=0, pr=1, end=T)
      w$endState()
   w$endStage()
w$endProcess()
w$closeWriter()

