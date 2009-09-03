\name{policyIteAve}
\alias{policyIteAve}
\title{Perform policy iteration (average criterion) on the MDP.}
\usage{policyIteAve(mdp, w, dur)}
\description{Perform policy iteration (average criterion) on the MDP.}
\details{The policy can afterwards be recieved using functions \code{getPolicy} and \code{getPolicyW}.}
\value{The optimal gain (g) calculated.}
\author{Lars Relund \email{lars@relund.dk}}
\seealso{\code{\link{getPolicy}}, \code{\link{getPolicyW}}.}
\arguments{\item{mdp}{The MDP loaded using \link{loadMDP}.}
\item{w}{The label of the weight we optimize.}
\item{dur}{The label of the duration/time such that discount rates can be calculated.}}
