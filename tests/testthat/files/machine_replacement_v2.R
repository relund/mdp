# The example given in L.R. Nielsen and A.R. Kristensen. Finding the K best
# policies in a finite-horizon Markov decision process. European Journal of
# Operational Research, 175(2):1164-1179, 2006. doi:10.1016/j.ejor.2005.06.011,
# does actually not have any dummy replacement node as in the MDP above. The same
# model can be created using a single dummy node at the end of the process.

## Create the MDP using a single dummy node
prefix<-"machine2_"
w <- binaryMDPWriter(prefix, getLog = FALSE)
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
