\name{infoMat}
\title{Info matrices or data frames of the state and action space.}
\usage{stateIdxMat(file=stateIdx.bin)\cr
stateIdxDf(file="stateIdx.bin", labels="stateIdxLbl.bin")\cr
transProbMat(file="transProb.bin")\cr
actionIdxMat(file="actionIdx.bin")\cr
actionIdxDf(file="actionIdx.bin", labels="actionIdxLbl.bin")\cr
actionInfo(file="actionIdx.bin" , weightFile="actionWeight.bin",\cr
transPrFile="transProb.bin", labels="actionIdxLbl.bin")\cr
actionWeightMat(file="actionWeight.bin",labels="actionWeightLbl.bin")\cr}
\description{Info matrices or data frames of the state and action space. Functions that
generate matrices with info about the HMDP model under consideration.}
\details{\code{stateIdxMat} return a matrix with columns (sId, d0, s0, a0, ...) where
sId is the state row id, d0 the index of the stage at level 0, s0 the index
of the state and a0 the index of the action. Moreover, if the HMDP has more
than one level columns (d1, s1, a1, ...) are added.

\code{stateIdxDf} return a data frame with the same columns as in
\code{stateIdxMat} plus another column containing the labels.

\code{actionIdxMat} return a matrix with columns (aId, ...) where
aId is the action row id and ... are alternating pairs (scp, idx), one for each
possible transition where scp is the scope that can be 4 values:
2 - A transition to a child process (stage zero in the child process), 1 - A transition
to next stage in the current process, 0 - A transition to the next stage in the father
process. the idx in the pair denote the index of the state at the stage considered.
Finally, if scope = 3 then a transition to the state with sId = idx is considered.

\code{actionIdxDf} return a data frame with the same columns as in
\code{actionIdxMat} plus another column containing the labels.

\code{actionWeightMat} return a matrix with columns (aId, ...) where
aId is the action row id and ... are the weights of the action.

\code{transProbMat} return a matrix with columns (aId, ...) where
aId is the action row id and ... are the probabilities of the action.

\code{actionInfo} return a matrix with columns from \code{actionIdxMat},
\code{actionCostMat} and \code{transProbMat} if labels is NULL. If labels
not are NULL then a data frame are returned with a label column too.}
\alias{stateIdxMat}
\alias{stateIdxDf}
\alias{actionIdxMat}
\alias{actionIdxDf}
\alias{actionCostMat}
\alias{transProbMat}
\author{Lars Relund \email{lars@relund.dk}}
\value{Matrix or data frame.}
\arguments{\item{prefix}{A character string with the prefix added to til file(s).}
\item{file}{The HMDP binary file containing the description under consideration.}
\item{labels}{The HMDP binary file containing the labels under consideration.}
\item{costFile}{The HMDP binary file containing the action costs.}
\item{transPrFile}{The HMDP binary file containing the transition probabilities.}}
\examples{stateIdxMat()
stateIdxDf()
actionIdxMat()
actionIdxDf()
actionWeightMat()
transProbMat()
a<-actionInfo()
a[order(a$sId),]}
