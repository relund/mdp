#' Load the HMDP model defined in the binary files. The model are created in memory
#' using the external C++ library.
#'
#' @param prefix A character string with the prefix added to \code{binNames}. Used to identify a specific model.
#' @param binNames A character vector of length 7 giving the names of the binary
#'     files storing the model.
#' @param eps The sum of the transition probabilities must at most differ eps from one.
#' @param check Check if the MDP seems correct.
#' @param verbose More output when running algorithms.
#' @return A list containing relevant information about the model and a pointer \code{ptr} to the model rc object in memory.
#' @author Lars Relund \email{lars@@relund.dk}
#' @example tests/machine.Rex
#' @export
loadMDP<-function(prefix="", binNames=c("stateIdx.bin","stateIdxLbl.bin","actionIdx.bin",
	"actionIdxLbl.bin","actionWeight.bin","actionWeightLbl.bin","transProb.bin","externalProcesses.bin"),
	eps = 0.00001, check = TRUE, verbose=FALSE)
{
	binNames<-paste(prefix,binNames,sep="")
	if (!is.logical(verbose)) verbose = FALSE
	mdp<-new(HMDP, binNames, verbose)

	if (!mdp$okay) {
		message(mdp$getLog())
		rm(mdp)
		return(invisible(NULL))
	}
	else message(mdp$getLog())

	if (check) {
	   msg<-mdp$checkHMDP(as.numeric(eps))
      if (msg==2) {
         stop(mdp$getLog(), call. = FALSE)
         return(invisible(NULL))
      }
	   else message(mdp$getLog())
	}

	timeHorizon = mdp$timeHorizon
	if (timeHorizon>=1000000000) timeHorizon = Inf
	if (timeHorizon>=Inf) {
	   founderStatesLast<-mdp$getStateSizeStage("1")
		states<-mdp$getStateSize()-founderStatesLast
	} else {
	   founderStatesLast<-mdp$getStateSizeStage(as.character(timeHorizon-1))
		states<-mdp$getStateSize()
	}
	actions <- mdp$getActionSize()
	levels <- mdp$levels
	weightNames <- mdp$wNames
	v<-list(binNames=binNames, timeHorizon=timeHorizon, states=states,
		founderStatesLast=founderStatesLast,
		actions=actions, levels=levels, weightNames=weightNames, ptr=mdp)
	class(v)<-c("MDP:C++")
	return(v)
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
#' @export
getWIdx<-function(mdp, wLbl) {
	idx<-grepl(wLbl,mdp$weightNames)
	if (!any(idx)) # we do not have a match
		stop("The weight name does not seem to exist!", call.=FALSE)
	return(which(idx)-1)
}


#' Perform policy iteration (average reward criterion) on the MDP.
#'
#' The policy can afterwards be recieved using functions \code{getPolicy} and \code{getPolicyW}.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param w The label of the weight we optimize.
#' @param dur The label of the duration/time such that discount rates can be calculated.
#' @param maxIte Max number of iterations. If the model does not satisfy the unichain assumption the algorithm may loop.
#' @return The optimal gain (g) calculated.
#' @author Lars Relund \email{lars@@relund.dk}
#' @seealso \code{\link{getPolicy}}, \code{\link{getPolicyW}}.
#' @export
policyIteAve<-function(mdp, w, dur, maxIte=100) {
	iW<-getWIdx(mdp,w)
	iDur<-getWIdx(mdp,dur)
	.checkWDurIdx(iW,iDur,length(mdp$weightNames))
	g<-mdp$ptr$policyIte(1, as.integer(maxIte), as.integer(iW), as.integer(iDur), rate=0, rateBase=1)
	#message(mdp$ptr$getLog())
	cat(mdp$ptr$getLog())
	return(g)
}


#' Perform policy iteration (discounted reward criterion) on the MDP.
#'
#' The policy can afterwards be received using functions \code{getPolicy} and \code{getPolicyW}.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param w The label of the weight we optimize.
#' @param dur The label of the duration/time such that discount rates can be calculated.
#' @param rate The interest rate.
#' @param rateBase The time-horizon the rate is valid over.
#' @param maxIte Max number of iterations. If the model does not satisfy the unichain assumption the algorithm may loop.
#' @return Nothing.
#' @author Lars Relund \email{lars@@relund.dk}
#' @seealso \code{\link{getPolicy}}, \code{\link{getPolicyW}}.
#' @export
policyIteDiscount<-function(mdp, w, dur, rate = 0.1, rateBase = 1, maxIte = 100) {
	iW<-getWIdx(mdp,w)
	iDur<-getWIdx(mdp,dur)
	.checkWDurIdx(iW,iDur,length(mdp$weightNames))
	g<-mdp$ptr$policyIte(0, as.integer(maxIte), as.integer(iW), as.integer(iDur), rate, rateBase)
	cat(mdp$ptr$getLog())
	invisible()
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
#' @param maxIte The max number of iterations value iteration is performed.
#' @param eps Stopping criterion. If max(w(t)-w(t+1))<epsilon then stop the algorithm, i.e the policy becomes epsilon optimal (see [1] p161).
#' @param termValues The terminal values used (values of the last stage in the MDP).
#' @param g Average reward. If specified then do a single iteration using the opdate equations under average reward criterion with the specified g value.
#' @return NULL (invisible)
#' @author Lars Relund \email{lars@@relund.dk}
#' @references [1] Puterman, M.; Markov Decision Processes, Wiley-Interscience, 1994.
#' @example tests/machine.Rex
#' @export
valueIte<-function(mdp, w, dur = NULL, rate = 0.1, rateBase = 1, maxIte = 10, eps = 0.00001, termValues = NULL, g=NULL) {
	iW<-getWIdx(mdp,w)
	iDur<-NULL
	if (!is.null(dur)) iDur<-getWIdx(mdp,dur)
	.checkWDurIdx(iW,iDur,length(mdp$weightNames))
	if (is.null(termValues)) termValues<-rep(0,mdp$founderStatesLast)
	if (is.null(g)) {
   	if (mdp$timeHorizon>=Inf) {
   		if (is.null(iDur)) stop("A duration index must be specified under infinite time-horizon!")
   	   mdp$ptr$valueIte(0, as.integer(maxIte),
   			as.numeric(eps), as.integer(iW), as.integer(iDur), as.numeric(termValues),
   			as.numeric(0),	as.numeric(rate), as.numeric(rateBase) )
   	} else {
   		if (!is.null(iDur)) mdp$ptr$valueIte(0, as.integer(1),
                as.numeric(0), as.integer(iW), as.integer(iDur), as.numeric(termValues),
                as.numeric(0), as.numeric(rate), as.numeric(rateBase) )
   		if (is.null(iDur)) mdp$ptr$valueIte(2, as.integer(1),
               as.numeric(0), as.integer(iW), as.integer(0), as.numeric(termValues),
               as.numeric(0), as.numeric(0), as.numeric(1) )
   	}
	} else {  # value ite under ave reward criterion
	   if (is.null(iDur)) stop("A duration index must be specified under average reward criterion!")
	   mdp$ptr$valueIte(1, as.integer(1),
           as.numeric(eps), as.integer(iW), as.integer(iDur), as.numeric(termValues),
           as.numeric(g), as.numeric(0), as.numeric(1) )
	}
	cat(mdp$ptr$getLog())
	invisible(NULL)
}


#' Get parts of the optimal policy.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param sId Vector of id's of the states we want to retrieve.
#' @param stageStr Stage string. If specified then find sId based on the stage string.
#' @param stateLabels Add state labels.
#' @param actionLabels Add action labels of policy.
#' @param actionIdx Add action index.
#' @param rewards Add rewards calculated for each state.
#' @return The policy (data frame).
#' @author Lars Relund \email{lars@@relund.dk}
#' @example tests/machine.Rex
#' @export
getPolicy<-function(mdp, sId = ifelse(mdp$timeHorizon>=Inf, mdp$founderStatesLast+1,1):
                       ifelse(mdp$timeHorizon>=Inf, mdp$states + mdp$founderStatesLast,mdp$states)-1, 
                    stageStr = NULL, stateLabels = TRUE, actionLabels = TRUE, actionIdx = TRUE, rewards = TRUE) {
	if (!is.null(stageStr)) sId = mdp$ptr$getStateIdsStages(stageStr)
   maxS<-ifelse(mdp$timeHorizon>=Inf, mdp$states + mdp$founderStatesLast,mdp$states)
	if (max(sId)>=maxS | min(sId)<0)
		stop("Out of range (sId). Need to be a subset of 0,...,",maxS-1,"!")
   policy <- data.frame(sId = sId)
   if (stateLabels) policy = cbind(policy, stateLabel = mdp$ptr$getStateLabel(sId), stringsAsFactors = FALSE )
   if (actionIdx) policy = cbind(policy, aIdx = mdp$ptr$getPolicy(sId) )
   if (actionLabels) policy = cbind(policy, actionLabel = mdp$ptr$getPolicyLabel(sId), stringsAsFactors = FALSE )
   policy = cbind(policy, reward = mdp$ptr$getPolicyW(sId) )
	return(policy)
}



#' Information about the MDP
#' 
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param sId The id of the state(s) considered.
#' @param stateStr A character vector containing the index of the state(s) (e.g. "n0,s0,a0,n1,s1"). 
#'   Parameter \code{sId} are ignored if not NULL.
#' @param stageStr A character vector containing the index of the stage(s) (e.g. "n0,s0,a0,n1"). 
#'   Parameter \code{sId} and \code{idxS} are ignored if not NULL.
#' @param withDF Include two data frames with information about actions and states.
#' @param withHarc Include hyperarcs data frame. Each row contains a hyperarc with the first column denoting the
#'   head (sId) and the rest tails (sId).
#' @param asStrings Write state vector, transitions and probabilities as strings.
#'   
#' @return A list of states containing actions.
#' @author Lars Relund \email{lars@@relund.dk}
#' @example tests/machine.Rex
#' @export
infoMDP<-function(mdp, sId=1:ifelse(mdp$timeHorizon<Inf, mdp$states, mdp$states+mdp$founderStatesLast)-1,
                  stateStr=NULL, stageStr=NULL, withDF = TRUE, withHarc = FALSE, asStrings = TRUE) {
   if (!is.null(stageStr)) {
      sId<-mdp$ptr$getStateIdsStages(stageStr)
   }else {
      if (!is.null(stateStr)) sId<-mdp$ptr$getStateIdsStates(stateStr)
   }
   maxS<-ifelse(mdp$timeHorizon>=Inf, mdp$states + mdp$founderStatesLast,mdp$states)
   if (max(sId)>=maxS | min(sId)<0)
      stop("Out of range (sId). Need to be a subset of 0,...,",maxS-1,"!")
   l<-vector("list", length(sId))
   lapply(l,function(x) x<-list(sId=NULL,stateStr=NULL,label=NULL,actions=NULL))

   stateStr<-mdp$ptr$getStateStr(sId)
   labels<-mdp$ptr$getStateLabel(sId)
   for (i in 1:length(l)) {
      l[[i]]$sId <- sId[i]
      l[[i]]$stateStr <- stateStr[i]
      l[[i]]$label <- labels[i]
      l[[i]]$actions <- mdp$ptr$getActionInfo(sId[i])
   }
   if (withDF) {
      if (asStrings) {
         stateDF=ldply(
            .data=l,
            .fun = function(x) {
               data.frame(sId=x$sId, stateStr = x$stateStr, label = x$label)
            }
         )
         actionDF=ldply(
            .data = l,
            .fun = function(x) {
               ldply(
                  x$actions,
                  function(y) {
                     data.frame(sId = x$sId, aIdx=y$aIdx, label = y$label,
                                weights = paste(y$weights, collapse = ","),
                                trans=paste(y$trans,collapse = ","),
                                pr = paste(y$pr, collapse = ",") )
                  }
               )
            }
         )
      } 
      if (!asStrings) {
         stateDF=ldply(
            .data=l,
            .fun = function(x) { 
               s <- rbind( scan(text=x$stateStr,, sep=",", quiet = TRUE) )
               data.frame(sId=x$sId, label=x$label, s)
            }
         )
         levels<-(ncol(stateDF)-2) %/% 3 + 1
         if (levels==1) colnames(stateDF)<-c("sId","label",paste(c("n","s"),levels-1,sep=""))
         if (levels>1) colnames(stateDF)<-c("sId","label",paste(c("n","s","a"),rep(0:(levels-2),each=3),sep=""),paste(c("n","s"),levels-1,sep=""))
         maxSizeTrans = max(ldply(
            .data = l,
            .fun = function(x) {
               ldply(
                  x$actions,
                  function(y) {
                     w <- length(y$trans)
                  }
               )
            }
         ) )
         actionDF=ldply(
            .data = l,
            .fun = function(x) {
               ldply(
                  x$actions,
                  function(y) {
                     w <- rbind(y$weights)
                     t<-rep(NA,maxSizeTrans)
                     t[1:length(y$trans)] <- y$trans
                     t <- rbind(t)
                     pr <- rep(NA,maxSizeTrans)
                     pr[1:length(y$pr)] <- y$pr
                     pr <- rbind(t)
                     data.frame(sId = x$sId, aIdx=y$aIdx, label = y$label, w = w, trans = t, pr = pr)
                  }
               )
            }
         )
      }
   }
   if (withHarc) {
      harcDF=ldply(
         .data = l,
         .fun = function(x) {
            ldply(
               x$actions,
               function(y) {
                  rbind( c(x$sId, y$trans) )
               }
            )
         }
      )
      harcDF$.id<-NULL
      colnames(harcDF) <- paste("tail",colnames(harcDF),sep="")
      colnames(harcDF)[1] <- "head"
      l$harcDF <- harcDF
   }
   if (withDF) {
      l$stateDF = stateDF
      l$actionDF = actionDF
   }
   return(l)
}


# #' Calculate the rentention payoff (RPO) or opportunity cost for some states.
# #'
# #' The RPO is defined as the difference between
# #' the weight of the state when using action \code{iA} and the maximum
# #' weight of the node when using another predecessor different from \code{iA}.
# #'
# #' @param mdp The MDP loaded using \link{loadMDP}.
# #' @param w The label of the weight we calculate RPO for.
# #' @param iA  The action index we calculate the RPO with respect to.
# #' @param sId Vector of id's of the states we want to retrive.
# #' @param criterion The criterion used. If \code{expected} used expected reward, if \code{discount} used discounted rewards, if \code{average} use average rewards.
# #' @param dur The label of the duration/time such that discount rates can be calculated.
# #' @param rate The interest rate.
# #' @param rateBase The time-horizon the rate is valid over.
# #' @param g The optimal gain (g) calculated (used if \code{criterion = "average"}).
# #' @return The rpo (matrix/data frame).
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @export
# calcRPO<-function(mdp, w, iA, sId = 1:mdp$states-1, criterion="expected", dur = 0, rate = 0.1, rateBase = 1, g = 0) {
# 	iW<-getWIdx(mdp,w)
# 	iDur<-getWIdx(mdp,dur)
# 	.checkWIdx(iW,length(mdp$weightNames))
# 	if (max(sId)>=mdp$states | min(sId)<0)
# 		stop("Out of range (sId). Need to be a subset of 0, ...,",mdp$states-1,"!")
# 	rpo<-NA
# 	if (criterion=="expected") rpo<-.Call("MDP_CalcRPO", mdp$ptr, as.integer(iW),
# 		as.integer(iA), as.integer(sId), PACKAGE="MDP")
# 	if (criterion=="discount") rpo<-.Call("MDP_CalcRPODiscount", mdp$ptr, as.integer(iW),
# 		as.integer(iA), as.integer(sId), as.integer(iDur), as.numeric(rate),
# 		as.numeric(rateBase), PACKAGE="MDP")
# 	if (criterion=="average") rpo<-.Call("MDP_CalcRPOAve", mdp$ptr, as.integer(iW),
# 		as.integer(iA), as.integer(sId), as.integer(iDur), as.numeric(g), PACKAGE="MDP")
# 	rpo<-cbind(sId=sId, rpo=rpo)
# 	return(rpo)
# }
#
#
# #' Calculate weights based on current policy. Normally run after an optimal policy has been found.
# #'
# #' @param mdp The MDP loaded using \link{loadMDP}.
# #' @param w The label of the weight we consider.
# #' @param criterion The criterion used. If \code{expected} used expected reward, if \code{discount} used discounted rewards, if \code{average} use average rewards.
# #' @param dur The label of the duration/time such that discount rates can be calculated.
# #' @param rate The interest rate.
# #' @param rateBase The time-horizon the rate is valid over.
# #' @param termValues The terminal values used (values of the last stage in the MDP).
# #' @return Nothing.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @example tests/machine.Rex
# #' @export
# calcWeights<-function(mdp, w, criterion="expected", dur = NULL, rate = 0.1, rateBase = 1, termValues=NULL) {
# 	iW<-getWIdx(mdp,w)
# 	if (!is.null(dur)) iDur<-getWIdx(mdp,dur)
# 	.checkWIdx(iW,length(mdp$weightNames))
# 	if (mdp$timeHorizon<Inf) {
# 		if (is.null(termValues)) stop("Terminal values must be specified under finite time-horizon!")
# 		if (criterion=="expected") .Call("MDP_CalcWeightsFinite", mdp$ptr, as.integer(iW), as.numeric(termValues), PACKAGE="MDP")
# 		if (criterion=="discount") .Call("MDP_CalcWeightsFiniteDiscount", mdp$ptr, as.integer(iW), as.integer(iDur),
# 			as.numeric(rate), as.numeric(rateBase), as.numeric(termValues), PACKAGE="MDP")
# 	} else {
# 		if (criterion=="discount") .Call("MDP_CalcWeightsInfDiscount", mdp$ptr, as.integer(iW), as.integer(iDur),
# 			as.numeric(rate), as.numeric(rateBase), PACKAGE="MDP")
# 		if (criterion=="average") return(.Call("MDP_CalcWeightsInfAve", mdp$ptr, as.integer(iW), as.integer(iDur), PACKAGE="MDP"))
# 		if (criterion=="expected") .Call("MDP_CalcWeightsFinite", mdp$ptr, as.integer(iW), as.numeric(termValues), PACKAGE="MDP")
# 	}
# 	invisible(NULL)
# }
#
#
# #' Fix the action of a state. That is, the other actions are removed from the HMDP.
# #'
# #' The actions can be reset using \code{resetActions}.
# #'
# #' @param mdp The MDP loaded using \link{loadMDP}.
# #' @param sId The state id of the state we want to fix the action for.
# #' @param iA  The action index of the state.
# #' @return Nothing.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @seealso \code{\link{resetActions}}, \code{\link{removeAction}}.
# #' @export
# fixAction<-function(mdp, sId, iA) {
# 	.Call("MDP_FixAction", mdp$ptr, as.integer(sId), as.integer(iA), PACKAGE="MDP")
# 	invisible(NULL)
# }
#
#
# #' Remove the action of a state from the HMDP.
# #'
# #' The actions can be reset using \code{resetActions}.
# #'
# #' @param mdp The MDP loaded using \link{loadMDP}.
# #' @param sId The state id of the state we want to remove the action for.
# #' @param iA  The action index of the state.
# #' @return Nothing.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @seealso \code{\link{resetActions}}, \code{\link{fixAction}}.
# #' @example tests/machine.Rex
# #' @export
# removeAction<-function(mdp, sId, iA) {
# 	.Call("MDP_RemoveAction", mdp$ptr, as.integer(sId), as.integer(iA), PACKAGE="MDP")
# 	invisible(NULL)
# }
#
#
# #' Reset the actions of a state.
# #'
# #' @param mdp The MDP loaded using \link{loadMDP}.
# #' @return Nothing.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @seealso \code{\link{resetActions}}, \code{\link{fixAction}}.
# #' @example tests/machine.Rex
# #' @export
# resetActions<-function(mdp) {
# 	.Call("MDP_ResetActions", mdp$ptr, PACKAGE="MDP")
# 	invisible(NULL)
# }
#
#
# #' Set the action of a state to be in the current policy.
# #'
# #' @param mdp The MDP loaded using \link{loadMDP}.
# #' @param sId The state id of the state.
# #' @param iA  The action index of the state.
# #' @return Nothing.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @export
# setPolicyAction<-function(mdp, sId, iA) {
# 	.Call("MDP_SetPolicyAction", mdp$ptr, as.integer(sId), as.integer(iA), PACKAGE="MDP")
# 	invisible(NULL)
# }
#
#
# #' Set the current policy.
# #'
# #' @param mdp The MDP loaded using \link{loadMDP}.
# #' @param policy A matrix with sId in the first column and action index in the second
# #' @return Nothing.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @example tests/machine.Rex
# #' @export
# setPolicy<-function(mdp, policy) {
# 	policy<-as.matrix(policy)
# 	if (ncol(policy)!=2) stop("The policy must be a matrix with 2 columns!")
# 	.Call("MDP_SetPolicy", mdp$ptr, as.integer(policy), PACKAGE="MDP")
# 	invisible(NULL)
# }
#
#
# #' Set the weight of a state.
# #'
# #' @param mdp The MDP loaded using \link{loadMDP}.
# #' @param w The weight.
# #' @param sId The state id of the state.
# #' @param wLbl The label of the weight we consider.
# #' @return Nothing.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @export
# setStateWeight<-function(mdp, w, sId, wLbl) {
# 	iW<-getWIdx(mdp,wLbl)
# 	.Call("MDP_SetStateW", mdp$ptr, as.numeric(w), as.integer(sId), as.integer(iW), PACKAGE="MDP")
# 	invisible(NULL)
# }
#
# #' Set the weight of an action.
# #'
# #' @param mdp The MDP loaded using \link{loadMDP}.
# #' @param w The weight.
# #' @param sId The state id of the state.
# #' @param iA The action index.
# #' @param wLbl The label of the weight we consider.
# #' @return Nothing.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @example tests/machine.Rex
# #' @export
# setActionWeight<-function(mdp, w, sId, iA, wLbl) {
# 	iW<-getWIdx(mdp,wLbl)
# 	.Call("MDP_SetActionW", mdp$ptr, as.numeric(w), as.integer(sId), as.integer(iA), as.integer(iW), PACKAGE="MDP")
# 	invisible(NULL)
# }

#
# #' Return ids for states in a stage.
# #'
# #' @param mdp The MDP loaded using \link{loadMDP}.
# #' @param stages A char vector of index in the form "n0,s0,a0,n1", i.e. 3*level+1 elements in the string.
# #' @return A vector of ids for the states.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @example tests/machine.Rex
# #' @export
# getIdSStages<-function(mdp, stages) {
# 	v<-.Call("MDP_GetIdSStage", mdp$ptr, as.character(stages), PACKAGE="MDP")
# 	return(v)
# }
#
#
# #' Return the index strings for states having id idS.
# #'
# #' @param mdp The MDP loaded using \link{loadMDP}.
# #' @param idS A vector of state ids.
# #' @return A vector of index for the states.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @example tests/machine.Rex
# #' @export
# getStrIdxS<-function(mdp, idS) {
# 	n<- mdp$states + ifelse(mdp$timeHorizon>=Inf,mdp$founderStatesLast,0)
# 	idS <- idS[idS<n & idS>=0]
# 	v<-.Call("MDP_GetIdxS", mdp$ptr, as.integer(idS), PACKAGE="MDP")
# 	return(v)
# }
#
#
# #' Return the label of states having id idS.
# #'
# #' @param mdp The MDP loaded using \link{loadMDP}.
# #' @param idS A vector of state ids.
# #' @return A vector of labels for the states.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @example tests/machine.Rex
# #' @export
# getLabel<-function(mdp, idS) {
# 	n<- mdp$states + ifelse(mdp$timeHorizon>=Inf,mdp$founderStatesLast,0)
# 	idS <- idS[idS<n & idS>=0]
# 	v<-.Call("MDP_GetLabel", mdp$ptr, as.integer(idS), PACKAGE="MDP")
# 	return(v)
# }
#
#
# #' Get the weights of an action.
# #'
# #' @param mdp The MDP loaded using \link{loadMDP}.
# #' @param idS The state id.
# #' @param idxA The action index.
# #' @return A vector of weights for the action.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @example tests/machine.Rex
# #' @export
# getActionW<-function(mdp, idS, idxA) {
# 	l<-info(mdp, idS[1])
# 	l<-l[[1]]$actions[idxA+1]
# 	l<-substring(l,regexpr("w",l)+3)
# 	l<-gsub(").*","",l)
# 	zz<-textConnection(l)
# 	l<-scan(zz, sep=",", quiet = TRUE)
# 	close(zz)
# 	return(l)
# }
#
#
# #' Get the ids of the transition states of an action.
# #'
# #' @param mdp The MDP loaded using \link{loadMDP}.
# #' @param idS The state id.
# #' @param idxA The action index.
# #' @return A vector of weights for the action.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @example tests/machine.Rex
# #' @export
# getActionTransIdS<-function(mdp, idS, idxA) {
# 	l<-info(mdp, idS[1])
# 	l<-l[[1]]$actions[idxA+1]
# 	l<-substring(l,regexpr("trans",l)+7)
# 	l<-gsub(").*","",l)
# 	zz<-textConnection(l)
# 	l<-scan(zz, sep=",", quiet = TRUE)
# 	close(zz)
# 	return(l)
# }
#
#
# #' Get the transition probabilities of the transition states of an action.
# #'
# #' @param mdp The MDP loaded using \link{loadMDP}.
# #' @param idS The state id.
# #' @param idxA The action index (c++ style starting from zero).
# #' @return A vector of weights for the action.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @example tests/machine.Rex
# #' @export
# getActionTransPr<-function(mdp, idS, idxA) {
#   return(.Call("MDP_GetActionTransPr", mdp$ptr, as.integer(idS), as.integer(idxA), PACKAGE="MDP") )
# }
# # getActionTransPr<-function(mdp, idS, idxA) {
# # 	l<-info(mdp, idS[1])
# # 	l<-l[[1]]$actions[idxA+1]
# # 	l<-substring(l,regexpr("pr",l)+4)
# # 	l<-gsub(").*","",l)
# # 	zz<-textConnection(l)
# # 	l<-scan(zz, sep=",", quiet = TRUE)
# # 	close(zz)
# # 	return(l)
# # }
#
#
# #' Calculate the steady state transition probabilities for the founder process (level 0).
# #'
# #' Assume that we consider an ergodic/irreducible time-homogeneous Markov chain specified using a policy in the MDP.
# #'
# #' @param mdp The MDP loaded using \link{loadMDP}.
# #' @return A vector stady state probabilities for all the states.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @export
# calcSteadyStatePr<-function(mdp) {
# 	pr<-.Call("MDP_CalcSteadyStatePr", mdp$ptr, PACKAGE="MDP")
# 	return(pr)
# }
#
# #' Get the transition probability matrix P for the founder process (level 0).
# #'
# #' @param mdp The MDP loaded using \link{loadMDP}.
# #' @return The state probability matrix.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @export
# getTransPr<-function(mdp) {
# 	v<-.Call("MDP_GetTransPr", mdp$ptr, PACKAGE="MDP")
# 	v<-matrix(v,nrow=mdp$states)
# 	return(v)
# }
