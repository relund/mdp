\name{policyIteDiscount}
\alias{policyIteDiscount}
\title{Perform policy iteration (discount criterion) on the MDP.}
\usage{policyIteDiscount(mdp, iW, iDur, rate=0.1, rateBase=365)}
\description{Perform policy iteration (discount criterion) on the MDP.}
\details{The policy can afterwards be recieved using functions \code{getPolicy} and \code{getPolicyW}.}
\value{Nothing.}
\author{Lars Relund \email{lars@relund.dk}}
\seealso{\code{\link{getPolicy}}, \code{\link{getPolicyW}}.}
\arguments{\item{mdp}{The MDP loaded using \link{loadMDP}.}
\item{iW}{index we want to optimize with respect to.}
\item{iDur}{Index of duration/time such that discount rates can be calculated.}
\item{rate}{The interest rate.}
\item{rateBase}{The time-horizon the rate is valid over.}}
