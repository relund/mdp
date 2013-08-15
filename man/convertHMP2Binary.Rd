\name{convertHMP2Binary}
\alias{convertHMP2Binary}
\title{Convert a HMDP model stored in a hmp (xml) file to binary file format.}
\usage{
  convertHMP2Binary(file, prefix = "")
}
\arguments{
  \item{file}{The name of the hmp file (e.g. mdp.hmp).}

  \item{prefix}{A character string with the prefix which
  will be added to the binary files.}
}
\value{
  NULL (invisible).
}
\description{
  The function simply parse the hmp file and create binary
  files using the \link{binaryMDPWriter}.
}
\note{
  Note all indexes are starting from zero (C/C++ style).
}
\examples{
wDir <- getwd()
setwd(system.file("models", package = "MDP"))

## convert the machine example to a hmp file
prefix1<-"machine1_"
convertBinary2HMP(prefix1, duration=NULL)
# have a look at the hmp file
xmlTreeParse("machine1_converted.hmp",useInternalNodes=TRUE)

## convert the machine example hmp file to binary files
prefix2<-"machine_cov_"
convertHMP2Binary("machine1.hmp",prefix2)
stateIdxDf(prefix1)
stateIdxDf(prefix2)

## convert the machine example with a single dummy node to a hmp file
#convertBinary2HMP("machine2_")  # error since using scope = 3 not supported in hmp files
}
\author{
  Lars Relund \email{lars@relund.dk}
}
\seealso{
  \link{binaryMDPWriter}.
}

