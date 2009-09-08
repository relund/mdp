\name{actionIdxMat}
\alias{actionIdxMat}
\title{Info about the actions in the HMDP model under consideration.}
\usage{actionIdxMat(prefix, file=actionIdx.bin)}
\description{Info about the actions in the HMDP model under consideration.}
\value{A matrix with columns (aId, ...) where
aId is the action row id and ... are alternating pairs (scp, idx), one for each
possible transition where scp is the scope that can be 4 values:
2 - A transition to a child process (stage zero in the child process), 1 - A transition
to next stage in the current process, 0 - A transition to the next stage in the father
process. the idx in the pair denote the index of the state at the stage considered.
Finally, if scope = 3 then a transition to the state with sId = idx is considered.}
\author{Lars Relund \email{lars@relund.dk}}
\arguments{\item{prefix}{A character string with the prefix added to til file(s).}
\item{file}{The HMDP binary file containing the description under consideration.}}
\examples{wDir<-getwd()
setwd(system.file("models", package = "MDP"))

prefix<-"machine1_"
stateIdxMat(prefix)
stateIdxDf(prefix)
actionIdxMat(prefix)
actionIdxDf(prefix)
actionWeightMat(prefix)
weightNames(prefix)
transProbMat(prefix)
a<-actionInfo(prefix)
a[order(a$sId),]

setwd(wDir)}
