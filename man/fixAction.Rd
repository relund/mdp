\name{fixAction}
\alias{fixAction}
\title{Fix the action of a state. That is, the other actions are removed from the HMDP.}
\usage{
  fixAction(mdp, sId, iA)
}
\arguments{
  \item{mdp}{The MDP loaded using \link{loadMDP}.}

  \item{sId}{The state id of the state we want to fix the
  action for.}

  \item{iA}{The action index of the state.}
}
\value{
  Nothing.
}
\description{
  The actions can be reset using \code{resetActions}.
}
\author{
  Lars Relund \email{lars@relund.dk}
}
\seealso{
  \code{\link{resetActions}}, \code{\link{removeAction}}.
}

