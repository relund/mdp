\name{binaryMDPWriter}
\alias{binaryMDPWriter}
\title{Function for writing an HMDP model to binary files.}
\usage{binaryMDPWriter(prefix="", binNames=c("stateIdx.bin","stateIdxLbl.bin","actionIdx.bin",
"actionIdxLbl.bin","actionWeight.bin","actionWeightLbl.bin","transProb.bin"))}
\description{Function for writing an HMDP model to binary files. The function defineds
subfunctions which can be used to define an HMDP model saved in a set of binary
files.}
\details{Binary files are efficent for storing large models. Compared to the HMP (XML)
format the binary files use less storage space and loading the model is faster.

The functions which can be used are:
\item{\code{setWeights(labels, ...)}: }{Set the labels of the weights used in the actions.
\code{labels} is a vector of label names, \code{...} are not used.
The function must be called before starting building the model.}
\item{\code{process()}: }{Starts a (sub)process.}
\item{\code{endProcess()}: }{Ends a (sub)process.}
\item{\code{stage(label=NULL)}: }{Starts a stage. Currently \code{label} are not used in the binary format.}
\item{\code{endStage()}: }{Ends a (sub)process.}
\item{\code{state(label=NULL)}: }{Starts a state. Returns (invisible) the states index number sIdx.}
\item{\code{endState()}: }{Ends a stage.}
\item{\code{action(label=NULL, weights, prob, ...)}: }{Starts an action. Parameter \code{weights} must be a vector of action weights,
\code{prob} must contain triples of (scope,idx,pr) (see the description of actionIdx.bin below), \code{...} is currently not used.}
\item{\code{endAction()}: }{Ends an action.}
\item{\code{closeWriter()}: }{Close the writer. Must be called when the model description has finished.}

Seven binary files are created using the following format:
\item{stateIdx.bin: }{File of integers containing the indexes defining all states in the format
"d0 s0 -1 d0 s0 a0 d1 s1 -1 d0 s0 a0 d1 s1 a1 d2 s2 -1 d0 s0 ...". Here -1 is
used to indicate that a new state is considered (new line).}
\item{stateIdxLbl.bin: }{File of characters in the format "sIdx label sIdx label ..." Here
sIdx corresponds to the index/line number in stateIdxLbl.bin (index starts from 0).
Note no delimiter is used.}
\item{actionIdx.bin: }{File of integers containing the indexes defining all actions in the format
"sIdx scope idx scope idx scope idx -1 sIdx scope idx scope idx -1 sIdx scope -1 ...".
sIdx corresponds to the index/line number in stateIdx.bin (index starts from 0).
Next pairs
(scope idx) will follow indicating the possible transitions. Scope can be 4 values:
2 - A transition to a child process (stage zero in the child process), 1 - A transition
to next stage in the current process, 0 - A transition to the next stage in the father
process. Here idx in the pair denote the index of the state at the stage considered,
e.g. if scope=1 and idx=2 we consider state number 3 at next stage in the current
process. Finally, if scope = 3 then a transition to a state specified by it's state sIdx
is given. That is, if scope=3 and idx=5 then
we have a transition to the state specified at line 6 in stateIdxLbl.bin.
This is usefull when considering shared child processes.}
\item{actionIdxLbl.bin: }{File of characters in the format "aIdx label aIdx label ..." Here
aIdx corresponds to the index/line number in actionIdx.bin (index starts from 0).
Note no delimiter is used.}
\item{actionWeight.bin: }{File of doubles containing the weights of the actions in the format
"c1 c2 c3 c1 c2 c3 ..." assuming three weights for each action.}
\item{actionWeightLbl.bin: }{File of characters containing the labels of the
weights in the format "lable1 label2 label3" assuming three weights for each action. }
\item{transProb.bin: }{File of doubles containing the probabilities of the transitions
defined in actions in actionIdx.bin. The format is
"p1 p2 p3 -1 p1 -1 p1 p2 -1 ...". Here -1 is
used to indicate that a new action is considered (new line).}}
\value{A list of functions.}
\author{Lars Relund \email{lars@relund.dk}}
\note{Note all indexes are starting from zero (C/C++ style).}
\arguments{\item{prefix}{A character string with the prefix added to \code{binNames}.}
\item{binNames}{A character vector of length 7 giving the names of the binary
files storing the model.}}
\examples{# Create a small HMDP with two levels
w<-binaryMDPWriter()
w$setWeights(c("Duration","Net reward","Items"))
w$process()
	w$stage()
		w$state(label="M0")
			w$action(label="A0",weights=c(0,0,0),prob=c(2,0,1))
				w$process()
					w$stage()
						w$state(label="D")
							w$action(label="A0",weights=c(0,0,1),prob=c(1,0,0.5,1,1,0.5))
							w$endAction()
						w$endState()
					w$endStage()
					w$stage()
						w$state(label="C0")
							w$action(label="A0",weights=c(0,0,0),prob=c(1,0,1))
							w$endAction()
							w$action(label="A1",weights=c(1,2,1),prob=c(1,0,0.5,1,1,0.5))
							w$endAction()
						w$endState()
						w$state(label="C1")
							w$action(label="A0",weights=c(0,0,0),prob=c(1,0,1))
							w$endAction()
							w$action(label="A1",weights=c(1,2,1),prob=c(1,0,0.5,1,1,0.5))
							w$endAction()
						w$endState()
					w$endStage()
					w$stage()
						w$state(label="C0")
							w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1))
							w$endAction()
						w$endState()
						w$state(label="C1")
							w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1))
							w$endAction()
						w$endState()
					w$endStage()
				w$endProcess()
			w$endAction()
			w$action(label="A1",weights=c(0,0,0),prob=c(2,0,1))
				w$process()
					w$stage()
						w$state(label="D")
							w$action(label="A0",weights=c(0,0,1),prob=c(1,0,1))
							w$endAction()
						w$endState()
					w$endStage()
					w$stage()
						w$state(label="C0")
							w$action(label="A0",weights=c(0,0,0),prob=c(1,0,1))
							w$endAction()
							w$action(label="A1",weights=c(1,2,1),prob=c(1,0,0.5,1,1,0.5))
							w$endAction()
						w$endState()
					w$endStage()
					w$stage()
						w$state(label="C0")
							w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1))
							w$endAction()
						w$endState()
						w$state(label="C1")
							w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1))
							w$endAction()
							w$action(label="A1",weights=c(0,10,5),prob=c(0,0,0.5,0,1,0.5))
							w$endAction()
						w$endState()
					w$endStage()
				w$endProcess()
			w$endAction()
		w$endState()
		w$state(label="M1")
			w$action(label="A0",weights=c(0,0,0),prob=c(2,0,1))
				w$process()
					w$stage()
						w$state(label="D")
							w$action(label="A0",weights=c(0,0,1),prob=c(1,0,0.5,1,1,0.5))
							w$endAction()
						w$endState()
					w$endStage()
					w$stage()
						w$state(label="C0")
							w$action(label="A0",weights=c(0,0,0),prob=c(1,0,1))
							w$endAction()
						w$endState()
						w$state(label="C1")
							w$action(label="A0",weights=c(0,0,0),prob=c(1,0,1))
							w$endAction()
						w$endState()
					w$endStage()
					w$stage()
						w$state(label="C0")
							w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1))
							w$endAction()
						w$endState()
						w$state(label="C1")
							w$action(label="A0",weights=c(1,4,0),prob=c(0,0,1))
							w$endAction()
						w$endState()
					w$endStage()
				w$endProcess()
			w$endAction()
		w$endState()
	w$endStage()
w$endProcess()
w$closeWriter()

stateIdxDf()
actionIdxDf()
actionWeightMat()
transProbMat()
a<-actionInfo()
a                   # ordered by action id
a[order(a$sId),]    # ordered by state id}
