\name{loadMDP}
\alias{loadMDP}
\title{Create the HMDP defined in the binary files.}
\usage{loadMDP<-function(binNames=c("stateIdx.bin","stateIdxLbl.bin","actionIdx.bin",
"actionIdxLbl.bin","actionWeight.bin","actionWeightLbl.bin","transProb.bin"), eps = 0.00001)}
\description{Create the HMDP defined in the binary files. The model are created in memory
using the external C++ library.}
\value{A list containing binNames and a pointer \code{ptr} to the model.}
\author{Lars Relund \email{lars@relund.dk}}
\arguments{\item{binNames}{A character vector of length 7 giving the names of the binary
files storing the model.}}
