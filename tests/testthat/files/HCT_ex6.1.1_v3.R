## Example 6.1.1 in Tijms, H.C., "A first course in stochastic models", John Wiley & Sons Ltd, 2003.
## Specify the MDP using two different approaches
N<-5; Cf<- -10; Cp<-c(NA,-7,-7,-5) # use negative numbers since optimize reward
Q <- matrix(c(
   0.90, 0.10, 0, 0, 0,
   0, 0.80, 0.10, 0.05, 0.05,
   0, 0, 0.70, 0.10, 0.20,
   0, 0, 0, 0.50, 0.50), nrow=4, byrow=T)


## Using binaryMDPWriter nested style
# note that we index from 0, i.e. state N correspond to index N-1 (in j col). Or index i corresponds to state i+1
transPr0<-data.frame(i=rep(1:(N-1), times=N), scope=1, j=rep(1:N,each=4)-1, pr=as.vector(Q))
transPr0<-transPr0[transPr0$pr>0,]  # remove transitions with zero pr
transPr1<-data.frame(i=2:(N-1), scope=1, j=1-1, pr=rep(1, times=N-2))
transPr2<-data.frame(i=c(N,N+1), scope=1, j=c(N+1,1)-1, pr=c(1,1))
getTransPr<-function(a,i) {
   if (a==0) return(as.vector(as.matrix(t(transPr0[transPr0$i==i,c(2,3,4)]))) )
   if (a==1) return(as.vector(as.matrix(t(transPr1[transPr1$i==i,c(2,3,4)]))))
   if (a==2) return(as.vector(as.matrix(t(transPr2[transPr2$i==i,c(2,3,4)]))))
   return(stop("Error"))
}
# Build the model which is stored in a set of binary files
w<-binaryMDPWriter("hct611v3-1_", getLog = FALSE)
w$setWeights(c("Duration","Net reward"))
w$process()
   w$stage()
      w$state(label="i=1")
         w$action(label="no repair", weights=c(1,0), prob=getTransPr(a=0,i=1), end=T)
      w$endState()
      for (ii in 2:(N-1) ) {
         w$state(label=paste("i=",ii,sep=""))
            w$action(label="no repair", weights=c(1,0), prob=getTransPr(a=0,i=ii), end=T)
            w$action(label="preventive repair", weights=c(1,Cp[ii]), prob=getTransPr(a=1,i=ii), end=T)
         w$endState()
      }
      for (ii in N:(N+1) ) {
         w$state(label=paste("i=",ii,sep=""))
            cost<-ifelse(ii==N, Cf, 0)
            w$action(label="forced repair", weights=c(1,cost), prob=getTransPr(a=2,i=ii), end=T)
         w$endState()
      }
   w$endStage()
w$endProcess()
w$closeWriter()


## Using binaryMDPWriter where the MDP is specified using a list of matrices and a matrix of rewards
# specify transition pr for each action (one 6x6 matrix for each action)
P<-list()
# a=1 (no repair)
Z<-as.matrix(rbind(cbind(Q,0),0,0))
#Z[5,1]<-Z[6,1]<-1
P[[1]]<-Z
# a=2 (preventive repair)
Z <- matrix(0, nrow = N+1, ncol = N+1)
Z[2,1]<-Z[3,1]<-Z[4,1]<-1
P[[2]]<-Z
# a=3 (forced repair)
Z <- matrix(0, nrow = N+1, ncol = N+1)
Z[5,6]<-Z[6,1]<-1
P[[3]]<-Z
# reward 6x3 matrix with one column for each action
R <- matrix(0, nrow = N+1, ncol = 3)
R[2:4,2]<-Cp[2:4]
R[5,3]<-Cf

# make bin files
w<-binaryMDPWriter("hct611v3-2_", getLog = FALSE)
w$setWeights(c("Duration","Net reward"))
w$process(P,R)
w$closeWriter()


## Using binaryMDPWriter where the MDP is specified using a list of sparse matrices and a sparse matrix of rewards
# specify transition pr for each action (one sparse 6x6 matrix for each action)
#library(Matrix)
P<-list()
# a=1 (no repair)
Z<-Matrix::Matrix(rbind(cbind(Q,0),0,0))
#Z[5,1]<-Z[6,1]<-1
P[[1]]<-Z
# a=2 (preventive repair)
Z <- Matrix::Matrix(0, nrow = N+1, ncol = N+1)
Z[2,1]<-Z[3,1]<-Z[4,1]<-1
P[[2]]<-Z
# a=3 (forced repair)
Z <- Matrix::Matrix(0, nrow = N+1, ncol = N+1)
Z[5,6]<-Z[6,1]<-1
P[[3]]<-Z

# reward 6x3 matrix with one column for each action
R <- Matrix::Matrix(0, nrow = N+1, ncol = 3)
R[2:4,2]<-Cp[2:4]
R[5,3]<-Cf

w<-binaryMDPWriter("hct611v3-3_", getLog = FALSE)
w$setWeights(c("Duration","Net reward"))
w$process(P,R)
w$closeWriter()


## Using binaryMDPWriter where the semi-MDP is specified using a list of matrices, a matrix of 
## rewards and a matrix of durations.
# specify transition pr for each action (one 5x5 matrix for each action)
P<-list()
# a=1 (no repair)
Z<-as.matrix(rbind(Q,0))
#Z[5,1]<-Z[6,1]<-1
P[[1]]<-Z
# a=2 (preventive repair)
Z <- matrix(0, nrow = N, ncol = N)
Z[2,1]<-Z[3,1]<-Z[4,1]<-1
P[[2]]<-Z
# a=3 (forced repair)
Z <- matrix(0, nrow = N, ncol = N)
Z[5,1]<-1
P[[3]]<-Z
# reward 5x3 matrix with one column for each action
R <- matrix(NA, nrow = N, ncol = 3)
R[,1]<-c(0,0,0,0,NA)
R[2:4,2]<-Cp[2:4]
R[5,3]<-Cf
# duration 5x3 matrix with one column for each action
D<-matrix(1,nrow = nrow(R), ncol = ncol(R))
D[5,3]<-2
# make bin files
w<-binaryMDPWriter("hct611v3-4_", getLog = FALSE)
w$setWeights(c("Duration","Net reward"))
w$process(P,R,D)
w$closeWriter()











