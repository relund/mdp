\name{actionWeightMat}
\alias{actionWeightMat}
\title{Info about the weights of the actions in the HMDP model under consideration.}
\usage{actionWeightMat(prefix="", file="actionWeight.bin",
    labels="actionWeightLbl.bin")}
\description{Info about the weights of the actions in the HMDP model under consideration.}
\value{A matrix with columns (aId, ...) where
aId is the action row id and ... are the weights of the action.}
\author{Lars Relund \email{lars@relund.dk}}
\arguments{\item{prefix}{A character string with the prefix added to til file(s).}
\item{file}{The HMDP binary file containing the description under consideration.}
\item{labels}{The HMDP binary file containing the labels under consideration.}}
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
