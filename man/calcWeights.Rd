\name{calcWeights}
\alias{calcWeights}
\title{Calculate weights based on current policy.}
\usage{calcWeights(mdp, w, criterion=expected, dur, rate=0.1, rateBase=1,
    termValues)}
\description{Calculate weights based on current policy. Normally run after an optimal policy has been found.}
\value{Nothing.}
\author{Lars Relund \email{lars@relund.dk}}
\arguments{\item{mdp}{The MDP loaded using \link{loadMDP}.}
\item{w}{The label of the weight we consider.}
\item{criterion}{The criterion used. If \code{expected} used expected reward, if \code{discount} used discounted rewards, if \code{average} use average rewards.}
\item{dur}{The label of the duration/time such that discount rates can be calculated.}
\item{rate}{The interest rate.}
\item{rateBase}{The time-horizon the rate is valid over.}
\item{termValues}{The terminal values used (values of the last stage in the MDP).}}
\examples{# Create the small machine repleacement problem used as an example in L.R.
# Nielsen and A.R. Kristensen. Finding the K best policies in a finite-horizon
# Markov decision process. European Journal of Operational Research,
# 175(2):1164-1179, 2006. doi:10.1016/j.ejor.2005.06.011.

## Create the MDP using a dummy replacement node
prefix<-"machine1_"
w <- binaryMDPWriter(prefix)
w$setWeights(c("Net reward"))
w$process()
	w$stage()   # stage n=0
		w$state(label="Dummy")          # v=(0,0)
			w$action(label="buy", weights=-100, prob=c(1,0,0.7, 1,1,0.3), end=TRUE)
		w$endState()
	w$endStage()
	w$stage()   # stage n=1
		w$state(label="good")           # v=(1,0)
			w$action(label="mt", weights=55, prob=c(1,0,1), end=TRUE)
			w$action(label="nmt", weights=70, prob=c(1,0,0.6, 1,1,0.4), end=TRUE)
		w$endState()
		w$state(label="average")        # v=(1,1)
			w$action(label="mt", weights=40, prob=c(1,0,1), end=TRUE)
			w$action(label="nmt", weights=50, prob=c(1,1,0.6, 1,2,0.4), end=TRUE)
		w$endState()
	w$endStage()
	w$stage()   # stage n=2
		w$state(label="good")           # v=(2,0)
			w$action(label="mt", weights=55, prob=c(1,0,1), end=TRUE)
			w$action(label="nmt", weights=70, prob=c(1,0,0.5, 1,1,0.5), end=TRUE)
		w$endState()
		w$state(label="average")        # v=(2,1)
			w$action(label="mt", weights=40, prob=c(1,0,1), end=TRUE)
			w$action(label="nmt", weights=50, prob=c(1,1,0.5, 1,2,0.5), end=TRUE)
		w$endState()
		w$state(label="not working")    # v=(2,2)
			w$action(label="mt", weights=30, prob=c(1,0,1), end=TRUE)
			w$action(label="rep", weights=5, prob=c(1,3,1), end=TRUE)
		w$endState()
	w$endStage()
	w$stage()   # stage n=3
		w$state(label="good")           # v=(3,0)
			w$action(label="mt", weights=55, prob=c(1,0,1), end=TRUE)
			w$action(label="nmt", weights=70, prob=c(1,0,0.2, 1,1,0.8), end=TRUE)
		w$endState()
		w$state(label="average")        # v=(3,1)
			w$action(label="mt", weights=40, prob=c(1,0,1), end=TRUE)
			w$action(label="nmt", weights=50, prob=c(1,1,0.2, 1,2,0.8), end=TRUE)
		w$endState()
		w$state(label="not working")    # v=(3,2)
			w$action(label="mt", weights=30, prob=c(1,0,1), end=TRUE)
			w$action(label="rep", weights=5, prob=c(1,3,1), end=TRUE)
		w$endState()
		w$state(label="replaced")       # v=(3,3)
			w$action(label="Dummy", weights=0, prob=c(1,3,1), end=TRUE)
		w$endState()
	w$endStage()
	w$stage()   # stage n=4
		w$state(label="good", end=TRUE)        # v=(4,0)
		w$state(label="average", end=TRUE)     # v=(4,1)
		w$state(label="not working", end=TRUE) # v=(4,2)
		w$state(label="replaced", end=TRUE)    # v=(4,3)
	w$endStage()
w$endProcess()
w$closeWriter()

## Some info about the model
stateIdxDf(prefix)      # states of the MDP with labels returned as a data frame
actionInfo(prefix)      # all action information of the MDP returned in a single data frame

## Load the model into memory
mdp<-loadMDP(prefix)
mdp

## Perform value iteration
w<-"Net reward"             # label of the weight we want to optimize
scrapValues<-c(30,10,5,0)   # scrap values (the values of the 4 states at stage 4)
valueIte(mdp, w, termValues=scrapValues)

## Print the optimal policy
policy<-getPolicy(mdp, labels=TRUE)     # optimal policy for each sId
states<-stateIdxDf(prefix)              # information about the states
policy<-merge(states,policy)            # merge the two data frames
policyW<-getPolicyW(mdp, w)             # the optimal rewards of the policy
policy<-merge(policy,policyW)           # add the rewards
policy

## Calculate the weights of the policy always to maintain
policy<-data.frame(sId=states$sId,iA=0)
setPolicy(mdp, policy)
calcWeights(mdp, w, termValues=scrapValues)
policy<-getPolicy(mdp, labels=TRUE)     # optimal policy for each sId
states<-stateIdxDf(prefix)              # information about the states
policy<-merge(states,policy)            # merge the two data frames
policyW<-getPolicyW(mdp, w)             # the optimal rewards of the policy
policy<-merge(policy,policyW)           # add the rewards
policy

## Modify the MDP in memory: remove the maintain action in the states of stage 1
removeAction(mdp, sId=1, iA=0)  # remove action 0 at the state with sId=1
removeAction(mdp, sId=2, iA=0)

## Perform value iteration on the modified MDP
valueIte(mdp, w, termValues=scrapValues)
policy<-getPolicy(mdp, labels=TRUE)     # optimal policy for each sId
states<-stateIdxDf(prefix)              # information about the states
policy<-merge(states,policy)            # merge the two data frames
policyW<-getPolicyW(mdp, w)             # the optimal rewards of the policy
policy<-merge(policy,policyW)           # add the rewards
policy

resetActions(mdp)   # reset the MDP such that all actions are used

## Modify the weight of action 'buy'
setActionWeight(mdp, w=-50, sId=0, iA=0, wLbl=w)

## Perform value iteration on the modified MDP
valueIte(mdp, w, termValues=scrapValues)
policy<-getPolicy(mdp, labels=TRUE)     # optimal policy for each sId
states<-stateIdxDf(prefix)              # information about the states
policy<-merge(states,policy)            # merge the two data frames
policyW<-getPolicyW(mdp, w)             # the optimal rewards of the policy
policy<-merge(policy,policyW)           # add the rewards
policy






# The example given in L.R. Nielsen and A.R. Kristensen. Finding the K best
# policies in a finite-horizon Markov decision process. European Journal of
# Operational Research, 175(2):1164-1179, 2006. doi:10.1016/j.ejor.2005.06.011,
# does actually not have any dummy replacement node as in the MDP above. The same
# model can be created using a single dummy node at the end of the process.

## Create the MDP using a single dummy node
prefix<-"machine2_"
w <- binaryMDPWriter(prefix)
w$setWeights(c("Net reward"))
w$process()
	w$stage()   # stage n=0
		w$state(label="Dummy")          # v=(0,0)
			w$action(label="buy", weights=-100, prob=c(1,0,0.7, 1,1,0.3), end=TRUE)
		w$endState()
	w$endStage()
	w$stage()   # stage n=1
		w$state(label="good")           # v=(1,0)
			w$action(label="mt", weights=55, prob=c(1,0,1), end=TRUE)
			w$action(label="nmt", weights=70, prob=c(1,0,0.6, 1,1,0.4), end=TRUE)
		w$endState()
		w$state(label="average")        # v=(1,1)
			w$action(label="mt", weights=40, prob=c(1,0,1), end=TRUE)
			w$action(label="nmt", weights=50, prob=c(1,1,0.6, 1,2,0.4), end=TRUE)
		w$endState()
	w$endStage()
	w$stage()   # stage n=2
		w$state(label="good")           # v=(2,0)
			w$action(label="mt", weights=55, prob=c(1,0,1), end=TRUE)
			w$action(label="nmt", weights=70, prob=c(1,0,0.5, 1,1,0.5), end=TRUE)
		w$endState()
		w$state(label="average")        # v=(2,1)
			w$action(label="mt", weights=40, prob=c(1,0,1), end=TRUE)
			w$action(label="nmt", weights=50, prob=c(1,1,0.5, 1,2,0.5), end=TRUE)
		w$endState()
		w$state(label="not working")    # v=(2,2)
			w$action(label="mt", weights=30, prob=c(1,0,1), end=TRUE)
			w$action(label="rep", weights=5, prob=c(3,12,1), end=TRUE)     # transition to the node with sId=12 (Dummy)
		w$endState()
	w$endStage()
	w$stage()   # stage n=3
		w$state(label="good")           # v=(3,0)
			w$action(label="mt", weights=55, prob=c(1,0,1), end=TRUE)
			w$action(label="nmt", weights=70, prob=c(1,0,0.2, 1,1,0.8), end=TRUE)
		w$endState()
		w$state(label="average")        # v=(3,1)
			w$action(label="mt", weights=40, prob=c(1,0,1), end=TRUE)
			w$action(label="nmt", weights=50, prob=c(1,1,0.2, 1,2,0.8), end=TRUE)
		w$endState()
		w$state(label="not working")    # v=(3,2)
			w$action(label="mt", weights=30, prob=c(1,0,1), end=TRUE)
			w$action(label="rep", weights=5, prob=c(3,12,1), end=TRUE)
		w$endState()
	w$endStage()
	w$stage()   # stage n=4
		w$state(label="good")        # v=(4,0)
			w$action(label="rep", weights=30, prob=c(1,0,1), end=TRUE)
		w$endState()
		w$state(label="average")     # v=(4,1)
			w$action(label="rep", weights=10, prob=c(1,0,1), end=TRUE)
		w$endState()
		w$state(label="not working") # v=(4,2)
			w$action(label="rep", weights=5, prob=c(1,0,1), end=TRUE)
		w$endState()
	w$endStage()
	w$stage()   # stage n=5
		w$state(label="Dummy", end=TRUE)        # v=(5,0)
	w$endStage()
w$endProcess()
w$closeWriter()

## Some info about the model
stateIdxDf(prefix)      # states of the MDP with labels returned as a data frame
actionInfo(prefix)      # all action information of the MDP returned in a single data frame

## Have a look at the state-expanded hypergraph
mdp<-loadMDP(prefix)
hypergf(mdp)

## Find optimal policy
w<-"Net reward"
valueIte(mdp, w, termValues=0)
policy<-getPolicy(mdp, labels=TRUE)     # optimal policy for each sId
states<-stateIdxDf(prefix)              # information about the states
policy<-merge(states,policy)            # merge the two data frames
policyW<-getPolicyW(mdp, w)             # the optimal rewards of the policy
policy<-merge(policy,policyW)           # add the rewards
policy}
