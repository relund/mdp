#' Create the HMDP defined in the binary files. The model are created in memory
#' using the external C++ library.
#'
#' @usage loadMDP(prefix="",
#'   binNames=c("stateIdx.bin","stateIdxLbl.bin","actionIdx.bin","actionIdxLbl.bin","actionWeight.bin","actionWeightLbl.bin","transProb.bin"),
#'   eps = 0.00001)
#' @param prefix A character string with the prefix added to \code{binNames}. Used to identify a specific model.
#' @param binNames A character vector of length 7 giving the names of the binary
#'     files storing the model.
#' @param eps The sum of the transition probabilities must at most differ eps from one.
#' @return A list containing relevant information about the model and a pointer \code{ptr} to the model object in memory.
#' @author Lars Relund \email{lars@@relund.dk}
#' @example pkg/tests/machine.Rex
loadMDP<-function(prefix="", binNames=c("stateIdx.bin","stateIdxLbl.bin","actionIdx.bin",
	"actionIdxLbl.bin","actionWeight.bin","actionWeightLbl.bin","transProb.bin"), eps = 0.00001)
{
	binNames<-paste(prefix,binNames,sep="")
	ptm <- proc.time()
	p<-.Call("MDP_NewHMDP", binNames, .deleteHMDP, PACKAGE="MDP")
	cpu <- (proc.time() - ptm)[3]
	cat("Cpu for reading the binary files: ", cpu, "s\n", sep="")
	.Call("MDP_Check",p,as.numeric(eps), PACKAGE="MDP")
	str<-.Call("MDP_GetLog", p, PACKAGE="MDP")
	cat(str)
	if (length(grep("error",str, ignore.case = TRUE))>0) return(invisible(NULL))
	.Call("MDP_BuildHMDP",p, PACKAGE="MDP")
	cat(.Call("MDP_GetLog",p, PACKAGE="MDP"))
	timeHorizon = .Call("MDP_GetTimeHorizon", p, PACKAGE="MDP")
	if (timeHorizon>=1000000000) timeHorizon = Inf
	states <- .Call("MDP_GetStates", p, PACKAGE="MDP")
	founderStatesLast<-states[1]
	if (timeHorizon>=Inf) {
		states<-states[2]-states[1]
	} else {
		states<-states[2]
	}
	actions <- .Call("MDP_GetActions", p, PACKAGE="MDP")
	levels <- .Call("MDP_GetLevels", p, PACKAGE="MDP")
	weightNames <- .Call("MDP_GetWeightNames", p, PACKAGE="MDP")
	v<-list(binNames=binNames, timeHorizon=timeHorizon, states=states,
		founderStatesLast=founderStatesLast,
		actions=actions, levels=levels, weightNames=weightNames, ptr=p)
	class(v)<-c("MDP:C++")
	return(v)
}


#' Internal function. Remove the HMDP from memory. Should not be used except you know what you are doing
#'
#' @usage .deleteHMDP(p)
#'
#' @aliases .deleteHMDP
#' @param p External pointer to the model.
#' @author Lars Relund \email{lars@@relund.dk}
#' @return Nothing.
#' @name deleteHMDP
.deleteHMDP <- function(p) {
	.Call("MDP_DeleteHMDP", p, PACKAGE="MDP");
	invisible()
}


#' Internal function. Check if the indexes given are okay. Should not be used except you know what you are doing
#'
#' @aliases .checkWDurIdx
#' @param iW Index of the weight we want to optimize.
#' @param iDur Index of the duration/time.
#' @param wLth Number of weights in the model.
#' @author Lars Relund \email{lars@@relund.dk}
#' @return Nothing.
#' @name checkWDurIdx
.checkWDurIdx<-function(iW, iDur, wLth) {
	if (length(iW)!=1) stop("Index iW must be of length one!",call. = FALSE)
	if (iW>wLth-1) stop("Index iW must be less than ",wLth,"!",call. = FALSE)
	if (iW<0) stop("Index iW must be greater or equal zero!",call. = FALSE)
	if (!is.null(iDur)) {
		if (length(iDur)!=1) stop("Index iDur must be of length one!",call. = FALSE)
		if (iW==iDur) stop("Indices iW and iDur must not be the same!",call. = FALSE)
		if (iDur>wLth-1) stop("Index iDur must be less than ",wLth,"!",call. = FALSE)
		if (iDur<0) stop("Index iDur must be greater or equal zero!",call. = FALSE)
	}
	invisible()
}


#' Internal function. Check if the index of the weight is okay. Should not be used except you know what you are doing
#'
#' @aliases .checkWIdx
#' @param iW Index of the weight we want to optimize.
#' @param wLth Number of weights in the model.
#' @author Lars Relund \email{lars@@relund.dk}
#' @return Nothing.
#' @name checkWIdx
.checkWIdx<-function(iW, wLth) {
	if (max(iW)>wLth-1) stop("Index iW must be less than ",wLth,"!",call. = FALSE)
	if (min(iW)<0) stop("Index iW must be greater or equal zero!",call. = FALSE)
	invisible()
}


#' Return the index of a weight in the model. Note that index always start from zero (C++ style), i.e. the first weight, the first state at a stage etc has index 0.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param wLbl The label/string of the weight.
#' @author Lars Relund \email{lars@@relund.dk}
#' @return The index (integer).
getWIdx<-function(mdp, wLbl) {
	idx<-grepl(wLbl,mdp$weightNames)
	if (!any(idx)) # we do not have a match
		stop("The weight name does not seem to exist!", call.=FALSE)
	return(which(idx)-1)
}


#' Perform value iteration on the MDP.
#'
#' If the MDP has a finite time-horizon then arguments \code{times} and \code{eps}
#' are ignored.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param w The label of the weight we optimize.
#' @param dur The label of the duration/time such that discount rates can be calculated.
#' @param rate Interest rate.
#' @param rateBase The time-horizon the rate is valid over.
#' @param times The max number of times value iteration is performed.
#' @param eps Stopping criterion. If max(w(t)-w(t+1))<epsilon then stop the algorithm, i.e the policy becomes epsilon optimal (see [1] p161).
#' @param termValues The terminal values used (values of the last stage in the MDP).
#' @return NULL (invisible)
#' @author Lars Relund \email{lars@@relund.dk}
#' @references [1] Puterman, M.; Markov Decision Processes, Wiley-Interscience, 1994.
#' @example pkg/tests/machine.Rex
valueIte<-function(mdp, w, dur = NULL, rate = 0.1, rateBase = 1, times = 10, eps = 0.00001,
	termValues = NULL) {
	iW<-getWIdx(mdp,w)
	iDur<-NULL
	if (!is.null(dur)) iDur<-getWIdx(mdp,dur)
	.checkWDurIdx(iW,iDur,length(mdp$weightNames))
	if (is.null(termValues)) termValues<-rep(0,mdp$founderStatesLast)
	if (mdp$timeHorizon>=Inf) {
		if (is.null(iDur)) stop("A duration index must be specified under infinite time-horizon!")
		.Call("MDP_ValueIteInfDiscount", mdp$ptr, as.integer(times),
			as.numeric(eps), as.integer(iW), as.integer(iDur), as.numeric(rate),
			as.numeric(rateBase), as.numeric(termValues), PACKAGE="MDP")
	} else {
		if (!is.null(iDur)) .Call("MDP_ValueIteFiniteDiscount", mdp$ptr, as.integer(iW),
			as.integer(iDur), as.numeric(rate), as.numeric(rateBase), as.numeric(termValues), PACKAGE="MDP")
		if (is.null(iDur)) .Call("MDP_ValueIteFinite", mdp$ptr, as.integer(iW), as.numeric(termValues), PACKAGE="MDP")
	}
	cat(.Call("MDP_GetLog",mdp$ptr, PACKAGE="MDP"))
	invisible(NULL)
}

#' Get parts of the optimal policy.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param sId Vector of id's of the states we want to retrieve.
#' @param labels If true return action labels otherwise return action index.
#' @return The policy (matrix (if \code{labels = FALSE}) otherwise data frame).
#' @author Lars Relund \email{lars@@relund.dk}
#' @example pkg/tests/machine.Rex
getPolicy<-function(mdp, sId = 1:mdp$states-1, labels = FALSE) {
	maxS<-ifelse(mdp$timeHorizon>=Inf, mdp$states + mdp$founderStatesLast,mdp$states)
	if (max(sId)>=maxS | min(sId)<0)
		stop("Out of range (sId). Need to be a subset of 0,...,",maxS-1,"!")
	if (!labels) {
		policy<-.Call("MDP_GetPolicyIdx", mdp$ptr, as.integer(sId), PACKAGE="MDP")
		policy<-cbind(sId=sId, iA = policy)
	} else {
		policy<-.Call("MDP_GetPolicyLabel", mdp$ptr, as.integer(sId), PACKAGE="MDP")
		policy<-data.frame(sId=sId, aLabel=policy, stringsAsFactors=FALSE)
	}
	return(policy)
}


#' Get parts of the optimal policy weights.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param w The label of the weight we consider.
#' @param sId Vector of id's of the states we want to retrive.
#' @return The weights of the policy.
#' @author Lars Relund \email{lars@@relund.dk}
#' @example pkg/tests/machine.Rex
getPolicyW<-function(mdp, w, sId = 1:mdp$states-1) {
	iW<-getWIdx(mdp,w)
	.checkWIdx(iW, length(mdp$weightNames))
	maxS<-ifelse(mdp$timeHorizon>=Inf, mdp$states + mdp$founderStatesLast,mdp$states)
	if (max(sId)>=maxS | min(sId)<0)
		stop("Out of range (sId). Need to be a subset of 0,...,",maxS-1,"!")
	policy<-.Call("MDP_GetPolicyW", mdp$ptr, as.integer(sId), as.integer(iW), PACKAGE="MDP")
	colnames(policy)<-paste("w",iW,sep="")
	policy<-cbind(sId=sId, policy)
	return(policy)
}


#' Perform policy iteration (discount criterion) on the MDP.
#'
#' The policy can afterwards be received using functions \code{getPolicy} and \code{getPolicyW}.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param w The label of the weight we optimize.
#' @param dur The label of the duration/time such that discount rates can be calculated.
#' @param rate The interest rate.
#' @param rateBase The time-horizon the rate is valid over.
#' @return Nothing.
#' @author Lars Relund \email{lars@@relund.dk}
#' @seealso \code{\link{getPolicy}}, \code{\link{getPolicyW}}.
policyIteDiscount<-function(mdp, w, dur, rate = 0.1, rateBase = 1) {
	iW<-getWIdx(mdp,w)
	iDur<-getWIdx(mdp,dur)
	.checkWDurIdx(iW,iDur,length(mdp$weightNames))
	.Call("MDP_PolicyIteDiscount", mdp$ptr, as.integer(iW),
		as.integer(iDur), as.numeric(rate), as.numeric(rateBase), PACKAGE="MDP")
	cat(.Call("MDP_GetLog",mdp$ptr, PACKAGE="MDP"))
	invisible()
}


#' Perform policy iteration (average criterion) on the MDP.
#'
#' The policy can afterwards be recieved using functions \code{getPolicy} and \code{getPolicyW}.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param w The label of the weight we optimize.
#' @param dur The label of the duration/time such that discount rates can be calculated.
#' @return The optimal gain (g) calculated.
#' @author Lars Relund \email{lars@@relund.dk}
#' @seealso \code{\link{getPolicy}}, \code{\link{getPolicyW}}.
policyIteAve<-function(mdp, w, dur) {
	iW<-getWIdx(mdp,w)
	iDur<-getWIdx(mdp,dur)
	.checkWDurIdx(iW,iDur,length(mdp$weightNames))
	g<-.Call("MDP_PolicyIteAve", mdp$ptr, as.integer(iW),
		as.integer(iDur), PACKAGE="MDP")
	cat(.Call("MDP_GetLog",mdp$ptr, PACKAGE="MDP"))
	return(g)
}


#' Calculate the rentention payoff (RPO) or opportunity cost for some states.
#'
#' The RPO is defined as the difference between
#' the weight of the state when using action \code{iA} and the maximum
#' weight of the node when using another predecessor different from \code{iA}.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param w The label of the weight we calculate RPO for.
#' @param iA  The action index we calculate the RPO with respect to.
#' @param sId Vector of id's of the states we want to retrive.
#' @param criterion The criterion used. If \code{expected} used expected reward, if \code{discount} used discounted rewards, if \code{average} use average rewards.
#' @param dur The label of the duration/time such that discount rates can be calculated.
#' @param rate The interest rate.
#' @param rateBase The time-horizon the rate is valid over.
#' @param g The optimal gain (g) calculated (used if \code{criterion = "average"}).
#' @return The rpo (matrix/data frame).
#' @author Lars Relund \email{lars@@relund.dk}
calcRPO<-function(mdp, w, iA, sId = 1:mdp$states-1, criterion="expected", dur = 0, rate = 0.1, rateBase = 1, g = 0) {
	iW<-getWIdx(mdp,w)
	iDur<-getWIdx(mdp,dur)
	.checkWIdx(iW,length(mdp$weightNames))
	if (max(sId)>=mdp$states | min(sId)<0)
		stop("Out of range (sId). Need to be a subset of 0, ...,",mdp$states-1,"!")
	rpo<-NA
	if (criterion=="expected") rpo<-.Call("MDP_CalcRPO", mdp$ptr, as.integer(iW),
		as.integer(iA), as.integer(sId), PACKAGE="MDP")
	if (criterion=="discount") rpo<-.Call("MDP_CalcRPODiscount", mdp$ptr, as.integer(iW),
		as.integer(iA), as.integer(sId), as.integer(iDur), as.numeric(rate),
		as.numeric(rateBase), PACKAGE="MDP")
	if (criterion=="average") rpo<-.Call("MDP_CalcRPOAve", mdp$ptr, as.integer(iW),
		as.integer(iA), as.integer(sId), as.integer(iDur), as.numeric(g), PACKAGE="MDP")
	rpo<-cbind(sId=sId, rpo=rpo)
	return(rpo)
}


#' Calculate weights based on current policy. Normally run after an optimal policy has been found.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param w The label of the weight we consider.
#' @param criterion The criterion used. If \code{expected} used expected reward, if \code{discount} used discounted rewards, if \code{average} use average rewards.
#' @param dur The label of the duration/time such that discount rates can be calculated.
#' @param rate The interest rate.
#' @param rateBase The time-horizon the rate is valid over.
#' @param termValues The terminal values used (values of the last stage in the MDP).
#' @return Nothing.
#' @author Lars Relund \email{lars@@relund.dk}
#' @example pkg/tests/machine.Rex
calcWeights<-function(mdp, w, criterion="expected", dur = NULL, rate = 0.1, rateBase = 1, termValues=NULL) {
	iW<-getWIdx(mdp,w)
	if (!is.null(dur)) iDur<-getWIdx(mdp,dur)
	.checkWIdx(iW,length(mdp$weightNames))
	if (mdp$timeHorizon<Inf) {
		if (is.null(termValues)) stop("Terminal values must be specified under finite time-horizon!")
		if (criterion=="expected") .Call("MDP_CalcWeightsFinite", mdp$ptr, as.integer(iW), as.numeric(termValues), PACKAGE="MDP")
		if (criterion=="discount") .Call("MDP_CalcWeightsFiniteDiscount", mdp$ptr, as.integer(iW), as.integer(iDur),
			as.numeric(rate), as.numeric(rateBase), as.numeric(termValues), PACKAGE="MDP")
	} else {
		if (criterion=="discount") .Call("MDP_CalcWeightsInfDiscount", mdp$ptr, as.integer(iW), as.integer(iDur),
			as.numeric(rate), as.numeric(rateBase), PACKAGE="MDP")
		if (criterion=="average") return(.Call("MDP_CalcWeightsInfAve", mdp$ptr, as.integer(iW), as.integer(iDur), PACKAGE="MDP"))
	}
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
	.Call("MDP_FixAction", mdp$ptr, as.integer(sId), as.integer(iA), PACKAGE="MDP")
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
#' @example pkg/tests/machine.Rex
removeAction<-function(mdp, sId, iA) {
	.Call("MDP_RemoveAction", mdp$ptr, as.integer(sId), as.integer(iA), PACKAGE="MDP")
	invisible(NULL)
}


#' Reset the actions of a state.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @return Nothing.
#' @author Lars Relund \email{lars@@relund.dk}
#' @seealso \code{\link{resetActions}}, \code{\link{fixAction}}.
#' @example pkg/tests/machine.Rex
resetActions<-function(mdp) {
	.Call("MDP_ResetActions", mdp$ptr, PACKAGE="MDP")
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
	.Call("MDP_SetPolicyAction", mdp$ptr, as.integer(sId), as.integer(iA), PACKAGE="MDP")
	invisible(NULL)
}


#' Set the current policy.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param policy A matrix with sId in the first column and action index in the second
#' @return Nothing.
#' @author Lars Relund \email{lars@@relund.dk}
#' @example pkg/tests/machine.Rex
setPolicy<-function(mdp, policy) {
	policy<-as.matrix(policy)
	if (ncol(policy)!=2) stop("The policy must be a matrix with 2 columns!")
	.Call("MDP_SetPolicy", mdp$ptr, as.integer(policy), PACKAGE="MDP")
	invisible(NULL)
}


#' Set the weight of a state.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param w The weight.
#' @param sId The state id of the state.
#' @param wLbl The label of the weight we consider.
#' @return Nothing.
#' @author Lars Relund \email{lars@@relund.dk}
setStateWeight<-function(mdp, w, sId, wLbl) {
	iW<-getWIdx(mdp,wLbl)
	.Call("MDP_SetStateW", mdp$ptr, as.numeric(w), as.integer(sId), as.integer(iW), PACKAGE="MDP")
	invisible(NULL)
}

#' Set the weight of an action.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param w The weight.
#' @param sId The state id of the state.
#' @param iA The action index.
#' @param wLbl The label of the weight we consider.
#' @return Nothing.
#' @author Lars Relund \email{lars@@relund.dk}
#' @example pkg/tests/machine.Rex
setActionWeight<-function(mdp, w, sId, iA, wLbl) {
	iW<-getWIdx(mdp,wLbl)
	.Call("MDP_SetActionW", mdp$ptr, as.numeric(w), as.integer(sId), as.integer(iA), as.integer(iW), PACKAGE="MDP")
	invisible(NULL)
}

#' The state-expanded hypergraph as a matrix
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @return Return the hypergraph as a matrix. Each row contains a (h)arc with the first column denoting the head (sId) and the rest tails (sId).
#' @author Lars Relund \email{lars@@relund.dk}
#' @example pkg/tests/machine.Rex
hypergf<-function(mdp) {
	v<-.Call("MDP_HgfMatrix", mdp$ptr, PACKAGE="MDP")
	v<-v-1  # so sId starts from zero
	v[v < 0] <- NA
	v<-matrix(v,nrow=mdp$actions)
	v<-v[order(v[,1]),]
	return(v)
}
