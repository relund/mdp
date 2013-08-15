\name{policyIteDiscount}
\alias{policyIteDiscount}
\title{Perform policy iteration (discount criterion) on the MDP.}
\usage{
  policyIteDiscount(mdp, w, dur, rate = 0.1, rateBase = 1)
}
\arguments{
  \item{mdp}{The MDP loaded using \link{loadMDP}.}

  \item{w}{The label of the weight we optimize.}

  \item{dur}{The label of the duration/time such that
  discount rates can be calculated.}

  \item{rate}{The interest rate.}

  \item{rateBase}{The time-horizon the rate is valid over.}
}
\value{
  Nothing.
}
\description{
  The policy can afterwards be received using functions
  \code{getPolicy} and \code{getPolicyW}.
}
\author{
  Lars Relund \email{lars@relund.dk}
}
\seealso{
  \code{\link{getPolicy}}, \code{\link{getPolicyW}}.
}

