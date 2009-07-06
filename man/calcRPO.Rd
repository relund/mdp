\name{calcRPO}
\alias{calcRPO}
\title{Calculate the rentention payoff (RPO) for some states.}
\usage{calcRPO(mdp, iW, iA, sId=1:mdp$states - 1, criterion=expected, iDur=0,
    rate=0.1, rateBase=365, g=0)}
\description{Calculate the rentention payoff (RPO) for some states.}
\details{The RPO is defined as the difference between the difference between
the weight of the state when using action \code{iA} and the maximum
weight of the node when using another predecessor different from \code{iA}.}
\value{The rpo (matrix/data frame).}
\author{Lars Relund \email{lars@relund.dk}}
\arguments{\item{mdp}{The MDP loaded using \link{loadMDP}.}
\item{iW}{Weight index we want to calculate RPO for.}
\item{iA}{The action index we calculate the RPO with respect to.}
\item{sId}{Vector of id's of the states we want to retrive.}
\item{criterion}{The criterion used. If \code{expected} used expected reward, if \code{discount} used discounted rewards, if \code{average} use average rewards.}
\item{iDur}{Index of duration such that discount rates can be calculated.}
\item{rate}{The interest rate.}
\item{rateBase}{The time-horizon the rate is valid over.}
\item{g}{The optimal gain (g) calculated (used if \code{criterion = "average"}).}}
