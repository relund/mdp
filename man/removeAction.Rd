\name{removeAction}
\alias{removeAction}
\title{Remove the action of a state from the HMDP.}
\usage{removeAction(mdp, sId, iA)}
\description{Remove the action of a state from the HMDP.}
\details{The actions can be reset using \code{resetActions}.}
\value{Nothing.}
\author{Lars Relund \email{lars@relund.dk}}
\seealso{\code{\link{resetActions}}, \code{\link{fixAction}}.}
\arguments{\item{mdp}{The MDP loaded using \link{loadMDP}.}
\item{sId}{The state id of the state we want to remove the action for.}
\item{iA}{The action index of the state.}}
