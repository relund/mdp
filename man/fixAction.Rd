\name{fixAction}
\alias{fixAction}
\title{Fix the action of a state.}
\usage{fixAction(mdp, sId, iA)}
\description{Fix the action of a state. That is, the other actions are removed from the HMDP.}
\details{The actions can be reset using \code{resetActions}.}
\value{Nothing.}
\author{Lars Relund \email{lars@relund.dk}}
\seealso{\code{\link{resetActions}}, \code{\link{removeAction}}.}
\arguments{\item{mdp}{The MDP loaded using \link{loadMDP}.}
\item{sId}{The state id of the state we want to fix the action for.}
\item{iA}{The action index of the state.}}
