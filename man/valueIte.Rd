\name{valueIte}
\alias{valueIte}
\title{Perform value iteration on the MDP.}
\usage{valueIte(mdp, iW, iDur, rate=0.1, rateBase=365, times=10, eps=1e-05,
    termValues)}
\description{Perform value iteration on the MDP.}
\details{If the MDP has a finite time-horizon then arguments \code{times} and \code{eps}
are ignored.}
\value{NULL (invisible)}
\author{Lars Relund \email{lars@relund.dk}}
\references{[1] Puterman, M.; Markov Decision Processes, Wiley-Interscience, 1994.}
\arguments{\item{mdp}{The MDP loaded using \link{loadMDP}.}
\item{iW}{Index of the weight we optimize.}
\item{iDur}{Index of duration/time such that discount rates can be calculated.}
\item{rate}{Interest rate.}
\item{rateBase}{The time-horizon the rate is valid over.}
\item{times}{The max number of times value iteration is performed.}
\item{eps}{Stopping criterion. If max(w(t)-w(t+1))<epsilon then stop the algorithm, i.e the policy becomes epsilon optimal (see [1] p161).}
\item{termValues}{The terminal values used (values of the last states in the MDP.}}
