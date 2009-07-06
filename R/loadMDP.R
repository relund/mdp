#' Create the HMDP defined in the binary files. The model are created in memory
#' using the external C++ library.
#'
#' @usage loadMDP<-function(binNames=c("stateIdx.bin","stateIdxLbl.bin","actionIdx.bin",
#'    "actionIdxLbl.bin","actionWeight.bin","actionWeightLbl.bin","transProb.bin"), eps = 0.00001)
#' @param binNames A character vector of length 7 giving the names of the binary
#'     files storing the model.
#' @return A list containing binNames and a pointer \code{ptr} to the model.
#' @author Lars Relund \email{lars@@relund.dk}
loadMDP<-function(binNames=c("stateIdx.bin","stateIdxLbl.bin","actionIdx.bin",
	"actionIdxLbl.bin","actionWeight.bin","actionWeightLbl.bin","transProb.bin"), eps = 0.00001) {
	ptm <- proc.time()
	p<-.Call("MDP_NewHMDP", binNames, .deleteHMDP)
	cpu <- (proc.time() - ptm)[2]
	cat("Cpu for reading the binary files: ", cpu, "s\n", sep="")
	.Call("MDP_Check",p,as.numeric(eps))
	str<-.Call("MDP_GetLog",p)
	cat(str)
	if (length(grep("error",str, ignore.case = TRUE))>0) return(invisible(NULL))
	.Call("MDP_BuildHMDP",p)
	cat(.Call("MDP_GetLog",p))
	timeHorizon = .Call("MDP_GetTimeHorizon", p)
	if (timeHorizon>=1000000000) timeHorizon = Inf
	states <- .Call("MDP_GetStates", p)
	actions <- .Call("MDP_GetActions", p)
	levels <- .Call("MDP_GetLevels", p)
	weightNames <- .Call("MDP_GetWeightNames", p)
	v<-list(binNames=binNames, timeHorizon=timeHorizon, states=states,
		actions=actions, levels=levels, weightNames=weightNames, ptr=p)
	class(v)<-c("MDP:C++")
	return(v)
}


#' Internal functions. These should not be used except you know what you are doing
#'
#' \code{stateIdxMat} return a matrix with columns (sId, d0, s0, a0, ...) where
#' sId is the state row id, d0 the index of the stage at level 0, s0 the index
#' of the state and a0 the index of the action. Moreover, if the HMDP has more
#' than one level columns (d1, s1, a1, ...) are added.
#'
#' @usage .deleteHMDP(p)\cr
#'      .checkWDurIdx(iW, iDur, wLth) \cr
#'      .checkWIdx(iW, wLth)\cr
#' @aliases .deleteHMDP .checkWDurIdx .checkWIdx
#' @param p External pointer to the model.
#' @param iW Vector of weight indices.
#' @param iDur The HMDP binary file containing the action costs.
#' @param wLth Number of weights in the model.
#' @author Lars Relund \email{lars@@relund.dk}
#' @return Nothing.
#' @name internal
.deleteHMDP <- function(p) {
	.Call("MDP_DeleteHMDP", p);
	invisible()
}

.checkWDurIdx<-function(iW, iDur, wLth) {
	if (lenght(iW)!=1) stop("Index iW must be of length one!",call. = FALSE)
	if (lenght(iDue)!=1) stop("Index iDur must be of length one!",call. = FALSE)
	if (iW==iDur) stop("Indices iW and iDur must not be the same!",call. = FALSE)
	if (iW>wLth-1) stop("Index iW must be less than ",wLth,"!",call. = FALSE)
	if (iDur>wLth-1) stop("Index iDur must be less than ",wLth,"!",call. = FALSE)
	if (iW<0) stop("Index iW must be greater or equal zero!",call. = FALSE)
	if (iDur<0) stop("Index iDur must be greater or equal zero!",call. = FALSE)
	invisible()
}

.checkWIdx<-function(iW, wLth) {
	if (max(iW)>wLth-1) stop("Index iW must be less than ",wLth,"!",call. = FALSE)
	if (min(iW)<0) stop("Index iW must be greater or equal zero!",call. = FALSE)
	invisible()
}



#' Perform value iteration on the MDP.
#'
#' If the MDP has a finite time-horizon then arguments \code{times} and \code{eps}.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param iW Index of the weight we optimize.
#' @param iDur Index of duration/time such that discount rates can be calculated.
#' @param rate Interest rate.
#' @param rateBase The time-horizon the rate is valid over.
#' @param times The max number of times value iteration is performed.
#' @param eps Stopping criterion. If max(w(t)-w(t+1))<epsilon then stop the algorithm, i.e the policy becomes epsilon optimal (see [1] p161).
#' @return NULL (invisible)
#' @author Lars Relund \email{lars@@relund.dk}
#' @references [1] Puterman, M.; Markov Decision Processes, Wiley-Interscience, 1994.
valueIte<-function(mdp, iW, iDur, rate = 0.1, rateBase = 365, times = 10, eps = 0.00001) {
	.checkWDurIdx(iW,iDur,length(mdp$weightNames))
	if (mdp$timeHorizon>=Inf) .Call("MDP_ValueIteInfDiscount", mdp$ptr, as.integer(times),
		as.numeric(eps), as.integer(iW), as.integer(iDur), as.numeric(rate),
		as.numeric(rateBase))
	else .Call("MDP_ValueIteFiniteDiscount", mdp$ptr, as.integer(iW),
		as.integer(iDur), as.numeric(rate), as.numeric(rateBase))
	cat(.Call("MDP_GetLog",mdp$ptr))
	invisible(NULL)
}

#' Get parts of the optimal policy.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param sId Vector of id's of the states we want to retrive.
#' @param labels If true return policy labels otherwise return action index.
#' @return The policy (matrix (if \code{labels = FALSE}) otherwise data frame).
#' @author Lars Relund \email{lars@@relund.dk}
getPolicy<-function(mdp, sId = 1:mdp$states-1, labels=FALSE) {
	if (max(sId)>=mdp$states | min(sId)<0)
		stop("Out of range (sId). Need to be a subset of 0, ...,",mdp$states-1,"!")
	if (!labels) {
		policy<-.Call("MDP_GetPolicyIdx", mdp$ptr, as.integer(sId))
		policy<-cbind(sId=sId, iA = policy)
	} else {
		policy<-.Call("MDP_GetPolicyLabel", mdp$ptr, as.integer(sId))
		policy<-data.frame(sId=sId, aLabel=policy)
	}
	return(policy)
}


#' Get parts of the optimal policy weights.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param iW Vector of weight indices.
#' @param sId Vector of id's of the states we want to retrive.
#' @param labels If true return policy labels otherwise return action index.
#' @return The weights (matrix (if \code{labels = FALSE}) otherwise data frame).
#' @author Lars Relund \email{lars@@relund.dk}
getPolicyW<-function(mdp, iW, sId = 1:mdp$states-1) {
	.checkWIdx(iW, length(mdp$weightNames))
	if (max(sId)>=mdp$states | min(sId)<0)
		stop("Out of range (sId). Need to be a subset of 0, ...,",mdp$states-1,"!")
	policy<-.Call("MDP_GetPolicyW", mdp$ptr, as.integer(sId), as.integer(iW))
	colnames(policy)<-paste("w",iW,sep="")
	policy<-cbind(sId=sId, policy)
	return(policy)
}


#' Perform policy iteration (discount criterion) on the MDP.
#'
#' The policy can afterwards be recieved using functions \code{getPolicy} and \code{getPolicyW}.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param iW index we want to optimize with respect to.
#' @param iDur Index of duration/time such that discount rates can be calculated.
#' @param rate The interest rate.
#' @param rateBase The time-horizon the rate is valid over.
#' @return Nothing.
#' @author Lars Relund \email{lars@@relund.dk}
#' @seealso \code{\link{getPolicy}}, \code{\link{getPolicyW}}.
policyIteDiscount<-function(mdp, iW, iDur, rate = 0.1, rateBase = 365) {
	.checkWDurIdx(iW,iDur,length(mdp$weightNames))
	.Call("MDP_PolicyIteDiscount", mdp$ptr, as.integer(iW),
		as.integer(iDur), as.numeric(rate), as.numeric(rateBase))
	cat(.Call("MDP_GetLog",mdp$ptr))
	invisible()
}


#' Perform policy iteration (average criterion) on the MDP.
#'
#' The policy can afterwards be recieved using functions \code{getPolicy} and \code{getPolicyW}.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param iW Weight index we want to optimize with respect to.
#' @param iDur Index of duration such that discount rates can be calculated.
#' @return The optimal gain (g) calculated.
#' @author Lars Relund \email{lars@@relund.dk}
#' @seealso \code{\link{getPolicy}}, \code{\link{getPolicyW}}.
policyIteAve<-function(mdp, iW, iDur) {
	.checkWDurIdx(iW,iDur,length(mdp$weightNames))
	g<-.Call("MDP_PolicyIteAve", mdp$ptr, as.integer(iW),
		as.integer(iDur))
	cat(.Call("MDP_GetLog",mdp$ptr))
	return(g)
}


#' Calculate the rentention payoff (RPO) for some states.
#'
#' The RPO is defined as the difference between the difference between
#' the weight of the state when using action \code{iA} and the maximum
#' weight of the node when using another predecessor different from \code{iA}.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param iW Weight index we want to calculate RPO for.
#' @param iA  The action index we calculate the RPO with respect to.
#' @param sId Vector of id's of the states we want to retrive.
#' @param criterion The criterion used. If \code{expected} used expected reward, if \code{discount} used discounted rewards, if \code{average} use average rewards.
#' @param iDur Index of duration such that discount rates can be calculated.
#' @param rate The interest rate.
#' @param rateBase The time-horizon the rate is valid over.
#' @param g The optimal gain (g) calculated (used if \code{criterion = "average"}).
#' @return The rpo (matrix/data frame).
#' @author Lars Relund \email{lars@@relund.dk}
calcRPO<-function(mdp, iW, iA, sId = 1:mdp$states-1, criterion="expected", iDur = 0, rate = 0.1, rateBase = 365, g = 0) {
	.checkWIdx(iW,length(mdp$weightNames))
	if (max(sId)>=mdp$states | min(sId)<0)
		stop("Out of range (sId). Need to be a subset of 0, ...,",mdp$states-1,"!")
	rpo<-NA
	if (criterion=="expected") rpo<-.Call("MDP_CalcRPO", mdp$ptr, as.integer(iW),
		as.integer(iA), as.integer(sId))
	if (criterion=="discount") rpo<-.Call("MDP_CalcRPODiscount", mdp$ptr, as.integer(iW),
		as.integer(iA), as.integer(sId), as.integer(iDur), as.numeric(rate),
		as.numeric(rateBase))
	if (criterion=="average") rpo<-.Call("MDP_CalcRPOAve", mdp$ptr, as.integer(iW),
		as.integer(iA), as.integer(sId), as.integer(iDur), as.numeric(g))
	rpo<-cbind(sId=sId, rpo=rpo)
	return(rpo)
}


#' Calculate weights based on current policy. Normally run after an optimal policy has been found.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param iW Vector of weight indices we want to calc weights for.
#' @param criterion The criterion used. If \code{expected} used expected reward, if \code{discount} used discounted rewards, if \code{average} use average rewards.
#' @param iDur Index of duration such that discount rates can be calculated.
#' @param rate The interest rate.
#' @param rateBase The time-horizon the rate is valid over.
#' @param g The optimal gain (g) calculated (used if \code{criterion = "average"}).
#' @return Nothing.
#' @author Lars Relund \email{lars@@relund.dk}
calcWeights<-function(mdp, iW, criterion="expected", iDur = 0, rate = 0.1, rateBase = 365, g = 0) {
	.checkWIdx(iW,length(mdp$weightNames))
	if (criterion=="expected") .Call("MDP_CalcWeights", mdp$ptr, as.integer(iW))
	if (criterion=="discount") .Call("MDP_CalcWeightsDiscount", mdp$ptr, as.integer(iW), as.integer(iDur),
		as.numeric(rate), as.numeric(rateBase))
	if (criterion=="average") .Call("MDP_CalcWeightsAve", mdp$ptr, as.integer(iW), as.integer(iDur),
		as.numeric(g))
	invisible(NULL)
}


#' Fix the action of a state. That is, the other actions are removed from the HMDP.
#'
#' The actions can be reset using \code{resetActions}.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param sId The state id of the state we want to fix the action for.
#' @param iA  The action index of the state.
#' @return Nothing.
#' @author Lars Relund \email{lars@@relund.dk}
#' @seealso \code{\link{resetActions}}, \code{\link{removeAction}}.
fixAction<-function(mdp, sId, iA) {
	.Call("MDP_FixAction", mdp$ptr, as.integer(sId), as.integer(iA))
	invisible(NULL)
}


#' Remove the action of a state from the HMDP.
#'
#' The actions can be reset using \code{resetActions}.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param sId The state id of the state we want to remove the action for.
#' @param iA  The action index of the state.
#' @return Nothing.
#' @author Lars Relund \email{lars@@relund.dk}
#' @seealso \code{\link{resetActions}}, \code{\link{fixAction}}.
removeAction<-function(mdp, sId, iA) {
	.Call("MDP_RemoveAction", mdp$ptr, as.integer(sId), as.integer(iA))
	invisible(NULL)
}


#' Reset the actions of a state.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param sId The state id of the state we want to reset the actions for.
#' @return Nothing.
#' @author Lars Relund \email{lars@@relund.dk}
#' @seealso \code{\link{resetActions}}, \code{\link{fixAction}}.
resetActions<-function(mdp, sId) {
	.Call("MDP_ResetActions", mdp$ptr)
	invisible(NULL)
}


#' Set the action of a state to be in the current policy.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param sId The state id of the state.
#' @param iA  The action index of the state.
#' @return Nothing.
#' @author Lars Relund \email{lars@@relund.dk}
setPolicyAction<-function(mdp, sId, iA) {
	.Call("MDP_SetPolicyAction", mdp$ptr, as.integer(sId), as.integer(iA))
	invisible(NULL)
}


#' Set the weight of a state.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param w The weight.
#' @param sId The state id of the state.
#' @param iW Weight index where set the weight.
#' @return Nothing.
#' @author Lars Relund \email{lars@@relund.dk}
setStateWeight<-function(mdp, w, sId, iW) {
	.Call("MDP_SetStateW", mdp$ptr, as.numeric(w), as.integer(sId), as.integer(iW))
	invisible(NULL)
}


# Build the HMDP in memory. That is, the state-expanded hypergraph is created.
#
# @param mdp The MDP loaded using \link{loadMDP}.
# @return NULL (invisible)
# @author Lars Relund \email{lars@@relund.dk}
#buildMDP<-function(mdp) {
#    str<-.Call("MDP_BuildHMDP",mdp$ptr)
#    cat(str)
#    invisible(NULL)
#}
