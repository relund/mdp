## Excercise 6.4 in Tijms, H.C., "A first course in stochastic models", John Wiley & Sons Ltd, 2003.
## The semi-MDP is specified using binaryMDPWriter and actions with id and pr

p<-c(1/8, 1/2, 1/4, 1/8)
states<-0:4
a0 <- "nothing"
a1 <- "empty"

# return the cost
cost<-function(i,a) {
   if (a==0) {
      if (i<2) return(0)   # no excess waste
      k <- (4-i+1):3
      return(30*sum( (i+k-4)*p[k+1] ) )
   }
   if (a==1) {
      return(25 + 5*i)
   }
   return(NULL)
}

# return the trans pr vector (scp=1,sId,pr, scp=1, sId, pr, ...) 
transPr<-function(i,a) {
   pr<-NULL
   if (a==0) {
      if (i<4) for (j in i:3) pr<-c(pr,p[j-i+1])
      if (i>0) pr<-c(pr,sum(p[(4-i):3+1]))
   }
   if (a==1) {
      for (j in 0:3) pr<-c(pr,p[j+1])
   }
   return(pr)
} 

idx<-function(i,a) {
   pr<-NULL
   if (a==0) {
      if (i<4) for (j in i:3) pr<-c(pr,j)
      if (i>0) pr<-c(pr,4)
   }
   if (a==1) {
      for (j in 0:3) pr<-c(pr,j)
   }
   return(pr)
} 

# Build the model which is stored in a set of binary files
w<-binaryMDPWriter("hct64v2_", getLog = FALSE)
w$setWeights(c("Duration","Net reward"))
w$process()
   w$stage()
      w$state(label=0)
         w$action(label=a0, weights=c(1,-cost(0,0)), pr=transPr(0,0), id=idx(0,0), end=T)
      w$endState()
      for (i in 1:4 ) {
      w$state(label=i)
         w$action(label=a0, weights=c(1,-cost(i,0)), pr=transPr(i,0), id=idx(i,0), end=T)
         w$action(label=a1, weights=c(1,-cost(i,1)), pr=transPr(i,1), id=idx(i,1), end=T)
      w$endState()
      }
   w$endStage()
w$endProcess()
w$closeWriter()
