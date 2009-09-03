\name{calcRPO}
\alias{calcRPO}
\title{Calculate the rentention payoff (RPO) or opportunity cost for some states.}
\usage{calcRPO(mdp, w, iA, sId=1:mdp$states - 1, criterion=expected, dur=0,
    rate=0.1, rateBase=1, g=0)}
\description{Calculate the rentention payoff (RPO) or opportunity cost for some states.}
\details{The RPO is defined as the difference between
the weight of the state when using action \code{iA} and the maximum
weight of the node when using another predecessor different from \code{iA}.}
\value{The rpo (matrix/data frame).}
\author{Lars Relund \email{lars@relund.dk}}
\arguments{\item{mdp}{The MDP loaded using \link{loadMDP}.}
\item{w}{The label of the weight we calculate RPO for.}
\item{iA}{The action index we calculate the RPO with respect to.}
\item{sId}{Vector of id's of the states we want to retrive.}
\item{criterion}{The criterion used. If \code{expected} used expected reward, if \code{discount} used discounted rewards, if \code{average} use average rewards.}
\item{dur}{The label of the duration/time such that discount rates can be calculated.}
\item{rate}{The interest rate.}
\item{rateBase}{The time-horizon the rate is valid over.}
\item{g}{The optimal gain (g) calculated (used if \code{criterion = "average"}).}}
