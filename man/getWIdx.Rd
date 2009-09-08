\name{getWIdx}
\alias{getWIdx}
\title{Return the index of a weight in the model.}
\usage{getWIdx(mdp, wLbl)}
\description{Return the index of a weight in the model. Note that index always start from zero (C++ style), i.e. the first weight, the first state at a stage etc has index 0.}
\author{Lars Relund \email{lars@relund.dk}}
\value{The index (integer).}
\arguments{\item{mdp}{The MDP loaded using \link{loadMDP}.}
\item{wLbl}{The label/string of the weight.}}
