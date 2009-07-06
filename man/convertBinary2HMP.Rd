\name{convertBinary2HMP}
\alias{convertBinary2HMP}
\title{Convert a HMDP model stored in binary format to a hmp (xml) file.}
\usage{convertBinary2HMP(binNames=c("stateIdx.bin","stateIdxLbl.bin","actionIdx.bin",
"actionIdxLbl.bin","actionWeight.bin","actionWeightLbl.bin","transProb.bin")
,out='r_convert.hmp', duration=1)}
\description{Convert a HMDP model stored in binary format to a hmp (xml) file.}
\details{The function simply parse the binary files and create hmp files using
the \link{hmpMDPWriter}.}
\value{NULL (invisible).}
\author{Lars Relund \email{lars@relund.dk}}
\note{Note all indexes are starting from zero (C/C++ style).}
\seealso{convertHMP2Binary.}
\arguments{\item{binNames}{A character vector of length 7 giving the names of the binary files storing the model.}
\item{out}{The name of the hmp file (e.g. mdp.hmp).}
\item{duration}{Weight number storing the duration.}}
\examples{f = system.file("inst/models", "mdp.xml", package = "MDP")
convertHMP2Binary("mdp.hmp")
stateIdxDf()
actionInfo()}
