\name{actionInfo}
\alias{actionInfo}
\title{Info about the actions in the HMDP model under consideration.}
\usage{actionInfo(prefix, file=actionIdx.bin, weightFile=actionWeight.bin,
    transPrFile=transProb.bin, labels=actionIdxLbl.bin)}
\description{Info about the actions in the HMDP model under consideration.}
\value{A matrix with columns from \code{actionIdxMat},
\code{actionCostMat} and \code{transProbMat} if labels is NULL. If labels
not are NULL then a data frame are returned with a label column too.}
\author{Lars Relund \email{lars@relund.dk}}
\arguments{\item{prefix}{A character string with the prefix added to til file(s).}
\item{file}{The HMDP binary file containing the description under consideration.}
\item{labels}{The HMDP binary file containing the labels under consideration.}
\item{weightFile}{The HMDP binary file containing the action costs.}
\item{transPrFile}{The HMDP binary file containing the transition probabilities.}}
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
