\name{actionIdxDf}
\alias{actionIdxDf}
\title{Info about the actions in the HMDP model under consideration.}
\usage{actionIdxDf(prefix, file=actionIdx.bin, labels=actionIdxLbl.bin)}
\description{Info about the actions in the HMDP model under consideration.}
\value{A data frame with the same columns as in
\code{actionIdxMat} plus another column containing the labels.}
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
