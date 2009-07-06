\name{getPolicy}
\alias{getPolicy}
\title{Get parts of the optimal policy.}
\usage{getPolicy(mdp, sId=1:mdp$states - 1, labels=FALSE)}
\description{Get parts of the optimal policy.}
\value{The policy (matrix (if \code{labels = FALSE}) otherwise data frame).}
\author{Lars Relund \email{lars@relund.dk}}
\arguments{\item{mdp}{The MDP loaded using \link{loadMDP}.}
\item{sId}{Vector of id's of the states we want to retrive.}
\item{labels}{If true return policy labels otherwise return action index.}}
