\name{internal}
\title{Internal functions.}
\usage{.deleteHMDP(p)\cr
.checkWDurIdx(iW, iDur, wLth) \cr
.checkWIdx(iW, wLth)\cr}
\description{Internal functions. These should not be used except you know what you are doing}
\details{\code{stateIdxMat} return a matrix with columns (sId, d0, s0, a0, ...) where
sId is the state row id, d0 the index of the stage at level 0, s0 the index
of the state and a0 the index of the action. Moreover, if the HMDP has more
than one level columns (d1, s1, a1, ...) are added.}
\alias{.deleteHMDP}
\alias{.checkWDurIdx}
\alias{.checkWIdx}
\author{Lars Relund \email{lars@relund.dk}}
\value{Nothing.}
\arguments{\item{p}{External pointer to the model.}
\item{iW}{Vector of weight indices.}
\item{iDur}{The HMDP binary file containing the action costs.}
\item{wLth}{Number of weights in the model.}}
