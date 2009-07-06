\name{calcWeights}
\alias{calcWeights}
\title{Calculate weights based on current policy.}
\usage{calcWeights(mdp, iW, criterion=expected, iDur=0, rate=0.1,
    rateBase=365, g=0)}
\description{Calculate weights based on current policy. Normally run after an optimal policy has been found.}
\value{Nothing.}
\author{Lars Relund \email{lars@relund.dk}}
\arguments{\item{mdp}{The MDP loaded using \link{loadMDP}.}
\item{iW}{Vector of weight indices we want to calc weights for.}
\item{criterion}{The criterion used. If \code{expected} used expected reward, if \code{discount} used discounted rewards, if \code{average} use average rewards.}
\item{iDur}{Index of duration such that discount rates can be calculated.}
\item{rate}{The interest rate.}
\item{rateBase}{The time-horizon the rate is valid over.}
\item{g}{The optimal gain (g) calculated (used if \code{criterion = "average"}).}}
