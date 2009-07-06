\name{convertHMP2Binary}
\alias{convertHMP2Binary}
\title{Convert a HMDP model stored in a hmp (xml) file to binary file format.}
\usage{convertHMP2Binary(file)}
\description{Convert a HMDP model stored in a hmp (xml) file to binary file format.}
\details{The function simply parse the hmp file and create binary files using
the \link{binaryMDPWriter}.}
\value{NULL (invisible).}
\author{Lars Relund \email{lars@relund.dk}}
\note{Note all indexes are starting from zero (C/C++ style).}
\seealso{\link{binaryMDPWriter}.}
\arguments{\item{file}{The name of the hmp file (e.g. mdp.hmp).}}
\examples{f = system.file("inst/models", "mdp.xml", package = "MDP")
convertHMP2Binary("mdp.hmp")
stateIdxDf()
actionInfo()}
