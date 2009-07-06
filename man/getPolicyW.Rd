\name{getPolicyW}
\alias{getPolicyW}
\title{Get parts of the optimal policy weights.}
\usage{getPolicyW(mdp, iW, sId=1:mdp$states - 1)}
\description{Get parts of the optimal policy weights.}
\value{The weights (matrix (if \code{labels = FALSE}) otherwise data frame).}
\author{Lars Relund \email{lars@relund.dk}}
\arguments{\item{mdp}{The MDP loaded using \link{loadMDP}.}
\item{iW}{Vector of weight indices.}
\item{sId}{Vector of id's of the states we want to retrive.}
\item{labels}{If true return policy labels otherwise return action index.}}
