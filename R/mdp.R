#' Load the HMDP model defined in the binary files. The model are created in memory
#' using the external C++ library.
#'
#' @param prefix A character string with the prefix added to \code{binNames}. Used to identify a specific model.
#' @param binNames A character vector of length 7 giving the names of the binary
#'     files storing the model.
#' @param eps The sum of the transition probabilities must at most differ eps from one.
#' @param check Check if the MDP seems correct.
#' @param verbose More output when running algorithms.
#' @param getLog Output the log messages.
#' 
#' @return A list containing relevant information about the model and a pointer \code{ptr} to the model rc object in memory.
#' @author Lars Relund \email{lars@@relund.dk}
#' @example tests/machine.Rex
#' @export
loadMDP<-function(prefix="", binNames=c("stateIdx.bin","stateIdxLbl.bin","actionIdx.bin",
	"actionIdxLbl.bin","actionWeight.bin","actionWeightLbl.bin","transProb.bin","externalProcesses.bin"),
	eps = 0.00001, check = TRUE, verbose=FALSE, getLog = TRUE)
{
	binNames<-paste(prefix,binNames,sep="")
	if (!is.logical(verbose)) verbose = FALSE
	mdp<-new(HMDP, binNames, verbose)

	if (!mdp$okay) {
		message(mdp$getLog())
		rm(mdp)
		return(invisible(NULL))
	}
	else if (getLog) message(mdp$getLog())

	if (check) {
	   msg<-mdp$checkHMDP(as.numeric(eps))
      if (msg==2) {
         stop(mdp$getLog(), call. = FALSE)
         return(invisible(NULL))
      }
	   else if (getLog) message(mdp$getLog())
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
	     founderStatesLast=founderStatesLast, actions=actions, levels=levels, 
	     weightNames=weightNames, ptr=mdp)
	if (mdp$externalProc) {
	   v$external <- as.data.frame(matrix(mdp$getExternalInfo(),ncol = 2, byrow = TRUE), stringsAsFactors=FALSE)
	   colnames(v$external) <- c("stageStr","prefix")
	}
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
#' @param getLog Output the log messages.
#' 
#' @return The optimal gain (g) calculated.
#' @author Lars Relund \email{lars@@relund.dk}
#' @seealso \code{\link{getPolicy}}, \code{\link{getPolicyW}}.
#' @export
policyIteAve<-function(mdp, w, dur, maxIte=100, getLog = TRUE) {
	iW<-getWIdx(mdp,w)
	iDur<-getWIdx(mdp,dur)
	.checkWDurIdx(iW,iDur,length(mdp$weightNames))
	g<-mdp$ptr$policyIte(1, as.integer(maxIte), as.integer(iW), as.integer(iDur), discountFactor=1)
	#message(mdp$ptr$getLog())
	if (getLog) cat(mdp$ptr$getLog())
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
#' @param discountFactor The discountRate for one time unit. If specified \code{rate} and \code{rateBase} are not used to calculate the discount rate.
#' @param maxIte Max number of iterations. If the model does not satisfy the unichain assumption the algorithm may loop.
#' @param discountMethod Either 'continuous' or 'discrete', corresponding to discount factor exp(-rate/rateBase) or 1/(1+rate/rateBase), respectively. Only used if \code{discountFactor} is \code{NULL}.
#' @param getLog Output the log messages.
#' 
#' @return Nothing.
#' @author Lars Relund \email{lars@@relund.dk}
#' @seealso \code{\link{getPolicy}}, \code{\link{getPolicyW}}.
#' @export
policyIteDiscount<-function(mdp, w, dur, rate = 0, rateBase = 1, discountFactor = NULL, maxIte = 100, 
                            discountMethod="continuous", getLog = TRUE) {
	iW<-getWIdx(mdp,w)
	iDur<-getWIdx(mdp,dur)
	.checkWDurIdx(iW,iDur,length(mdp$weightNames))
	if (is.null(discountFactor)) {
	   if (discountMethod=="continuous") discountFactor<-exp(-rate/rateBase)
	   if (discountMethod=="discrete") discountFactor<-1/(1 + rate/rateBase)
	}
	g<-mdp$ptr$policyIte(0, as.integer(maxIte), as.integer(iW), as.integer(iDur), discountFactor)
	if (getLog) cat(mdp$ptr$getLog())
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
#' @param discountFactor The discountRate for one time unit. If specified \code{rate} and \code{rateBase} are not used to calculate the discount rate.
#' @param maxIte The max number of iterations value iteration is performed.
#' @param eps Stopping criterion. If max(w(t)-w(t+1))<epsilon then stop the algorithm, i.e the policy becomes epsilon optimal (see [1] p161).
#' @param termValues The terminal values used (values of the last stage in the MDP).
#' @param g Average reward. If specified then do a single iteration using the opdate equations under average reward criterion with the specified g value.
#' @param getLog Output the log messages.
#' @param discountMethod Either 'continuous' or 'discrete', corresponding to discount factor exp(-rate/rateBase) or 1/(1+rate/rateBase), respectively. Only used if \code{discountFactor} is \code{NULL}.
#' 
#' @return NULL (invisible)
#' @author Lars Relund \email{lars@@relund.dk}
#' @references [1] Puterman, M.; Markov Decision Processes, Wiley-Interscience, 1994.
#' @example tests/machine.Rex
#' @export
valueIte<-function(mdp, w, dur = NULL, rate = 0, rateBase = 1, discountFactor = NULL, maxIte = 100, 
                   eps = 1e-05, termValues = NULL, g=NULL, getLog = TRUE, discountMethod="continuous") {
	iW<-getWIdx(mdp,w)
	iDur<-NULL
	if (!is.null(dur)) iDur<-getWIdx(mdp,dur)
	.checkWDurIdx(iW,iDur,length(mdp$weightNames))
	if (is.null(discountFactor)) {
	   if (discountMethod=="continuous") discountFactor<-exp(-rate/rateBase)
	   if (discountMethod=="discrete") discountFactor<-1/(1 + rate/rateBase)
	}
	if (is.null(termValues)) termValues<-rep(0,mdp$founderStatesLast)
	if (is.null(g)) {
   	if (mdp$timeHorizon>=Inf) {
   		if (is.null(iDur)) stop("A duration index must be specified under infinite time-horizon!")
   	   mdp$ptr$valueIte(0, as.integer(maxIte),
   			as.numeric(eps), as.integer(iW), as.integer(iDur), as.numeric(termValues),
   			as.numeric(0),	as.numeric(discountFactor) )
   	} else {
   		if (!is.null(iDur)) mdp$ptr$valueIte(0, as.integer(1),
                as.numeric(0), as.integer(iW), as.integer(iDur), as.numeric(termValues),
                as.numeric(0), as.numeric(discountFactor) )
   		if (is.null(iDur)) mdp$ptr$valueIte(2, as.integer(1),
               as.numeric(0), as.integer(iW), as.integer(0), as.numeric(termValues),
               as.numeric(0), as.numeric(1) )
   	}
	} else {  # value ite under ave reward criterion
	   if (is.null(iDur)) stop("A duration index must be specified under average reward criterion!")
	   mdp$ptr$valueIte(1, as.integer(1),
           as.numeric(eps), as.integer(iW), as.integer(iDur), as.numeric(termValues),
           as.numeric(g), as.numeric(1) )
	}
	if (getLog) cat(mdp$ptr$getLog())
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
#' @param stateStr Add the state string for each state.
#' @param external A vector of stage strings corresponding to external processes we want the optimal policy of.
#' @param ... Parameters passed on when find the optimal policy of the external processes.
#' 
#' Note if external is specified then it must contain stage strings from mdp$external. Moreover you 
#' must specify further arguments passed on to valueIte used for recreating the optimal policy e.g. 
#' the g value and the label for reward and duration. See the vignette about external processes. 
#' 
#' @return The policy (data frame).
#' @author Lars Relund \email{lars@@relund.dk}
#' @example tests/machine.Rex
#' @export
getPolicy<-function(mdp, sId = ifelse(mdp$timeHorizon>=Inf, mdp$founderStatesLast+1,1):
                       ifelse(mdp$timeHorizon>=Inf, mdp$states + mdp$founderStatesLast,mdp$states)-1, 
                    stageStr = NULL, stateLabels = TRUE, actionLabels = TRUE, actionIdx = TRUE, 
                    rewards = TRUE, stateStr = FALSE, external = NULL, ...) {
	if (!is.null(stageStr)) sId = mdp$ptr$getStateIdsStages(stageStr)
   maxS<-ifelse(mdp$timeHorizon>=Inf, mdp$states + mdp$founderStatesLast,mdp$states)
	if (max(sId)>=maxS | min(sId)<0)
		stop("Out of range (sId). Need to be a subset of 0,...,",maxS-1,"!")
   cols <- 1 + stateLabels + actionIdx + actionLabels + rewards + stateStr
   policy<-data.frame(matrix(NA, nrow = length(sId), ncol = cols))
   cols<-1
   policy[,cols] <- sId
   colNames = "sId"; cols = cols + 1
   if (stateStr) {
      policy[,cols]<-mdp$ptr$getStateStr(sId)
      colNames = c(colNames, "stateStr"); cols = cols + 1
   }
   if (stateLabels) {
      policy[,cols] = mdp$ptr$getStateLabel(sId); 
      colNames = c(colNames, "stateLabel"); cols = cols + 1
   }
   if (actionIdx) {
      policy[,cols] = mdp$ptr$getPolicy(sId) 
      colNames = c(colNames, "aIdx"); cols = cols + 1       
   }
   if (actionLabels) {
      policy[,cols] = mdp$ptr$getPolicyLabel(sId)
      colNames = c(colNames, "actionLabel"); cols = cols + 1
   }
   if (rewards) {
      policy[,cols] = mdp$ptr$getPolicyW(sId) 
      colNames = c(colNames, "weight"); cols = cols + 1
   }
   colnames(policy) <- colNames
   
   if (!is.null(external)) {
      policy <- list(main=policy)
      for (s in external) {
         prefix <- subset(mdpExt$external, stageStr == s, select = "prefix", drop = TRUE)
         lastStage<-mdp$ptr$getNextStageStr(s)
         termValues <- getPolicy(mdp, stageStr = lastStage)$weight
         extMDP<-loadMDP(prefix, getLog = FALSE)
         valueIte(extMDP, termValues=termValues, getLog = FALSE, ...)
         extPolicy<-getPolicy(extMDP)
         policy[[s]]<-extPolicy
         rm(extMDP)
      }
   }
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
      stateStr<-mdp$ptr$getStateStr(sId)
   }else {
      if (!is.null(stateStr)) sId<-mdp$ptr$getStateIdsStates(stateStr)
      else stateStr<-mdp$ptr$getStateStr(sId)
   }
   maxS<-ifelse(mdp$timeHorizon>=Inf, mdp$states + mdp$founderStatesLast,mdp$states)
   if (max(sId)>=maxS | min(sId)<0)
      stop("Out of range (sId). Need to be a subset of 0,...,",maxS-1,"!")
   l<-vector("list", length(sId))
   lapply(l,function(x) x<-list(sId=NULL,stateStr=NULL,label=NULL,actions=NULL))

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
               data.frame(sId=x$sId, stateStr = x$stateStr, label = x$label, stringsAsFactors = FALSE)
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
                                pr = paste(y$pr, collapse = ","), stringsAsFactors = FALSE)
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
               data.frame(sId=x$sId, label=x$label, s, stringsAsFactors = FALSE)
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
                     data.frame(sId = x$sId, aIdx=y$aIdx, label = y$label, w = w, trans = t, pr = pr, stringsAsFactors = FALSE)
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


#' Modify the current policy by setting policy action of states. 
#' 
#' If the policy does not contain all states then the actions from the previous optimal 
#' policy are used.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param policy A data frame with two columns state id and action index.
#' @return Nothing.
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
setPolicy<-function(mdp, policy) {
   if (dim(policy)[2]!=2) stop("You must specify two columns in policy.")
   mdp$ptr$setPolicy(as.integer(policy[,1]),as.integer(policy[,2]))
	invisible(NULL)
}


#' Calculate weights based on current policy. Normally run after an optimal policy has been found.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param wLbl The label of the weight we consider.
#' @param criterion The criterion used. If \code{expected} used expected reward, if \code{discount} used discounted rewards, if \code{average} use average rewards.
#' @param durLbl The label of the duration/time such that discount rates can be calculated.
#' @param rate The interest rate.
#' @param rateBase The time-horizon the rate is valid over.
#' @param discountFactor The discountRate for one time unit. If specified \code{rate} and \code{rateBase} are not used to calculate the discount rate.
#' @param termValues The terminal values used (values of the last stage in the MDP).
#' @param discountMethod Either 'continuous' or 'discrete', corresponding to discount factor exp(-rate/rateBase) or 1/(1+rate/rateBase), respectively. Only used if \code{discountFactor} is \code{NULL}.
#' 
#' @return Nothing.
#' @author Lars Relund \email{lars@@relund.dk}
#' @example tests/machine.Rex
#' @export
calcWeights<-function(mdp, wLbl, criterion="expected", durLbl = NULL, rate = 0, rateBase = 1, 
                      discountFactor = NULL, termValues = NULL, discountMethod = "continuous") {
	iW<-getWIdx(mdp,wLbl)
	if (!is.null(durLbl)) iDur<-getWIdx(mdp,durLbl)
	.checkWIdx(iW,length(mdp$weightNames))
	if (is.null(discountFactor)) {
	   if (discountMethod=="continuous") discountFactor<-exp(-rate/rateBase)
	   if (discountMethod=="discrete") discountFactor<-1/(1 + rate/rateBase)
	}
	if (mdp$timeHorizon<Inf) {
		if (is.null(termValues)) stop("Terminal values must be specified under finite time-horizon!")
		if (criterion=="expected") mdp$ptr$calcPolicy(2,iW,0,1,discountFactor)
		if (criterion=="discount") mdp$ptr$calcPolicy(1,iW,0,iDur,discountFactor)
	} else {
		if (criterion=="discount") mdp$ptr$policyIteFixedPolicy(1,iW,iDur,discountFactor)
		if (criterion=="average") return( mdp$ptr$policyIteFixedPolicy(0,iW,iDur,discountFactor) )
		#if (criterion=="expected") .Call("MDP_CalcWeightsFinite", mdp$ptr, as.integer(iW), as.numeric(termValues), PACKAGE="MDP")
	}
	invisible(NULL)
}


#' Calculate the rentention payoff (RPO) or opportunity cost for some states.
#'
#' The RPO is defined as the difference between
#' the weight of the state when using action \code{iA} and the maximum
#' weight of the node when using another predecessor different from \code{iA}.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param w The label of the weight/reward we calculate RPO for.
#' @param iA  The action index we calculate the RPO with respect to (same size as sId).
#' @param sId Vector of id's of the states we want to retrive.
#' @param criterion The criterion used. If \code{expected} used expected reward, if \code{discount} used discounted rewards, if \code{average} use average rewards.
#' @param dur The label of the duration/time such that discount rates can be calculated.
#' @param rate The interest rate.
#' @param rateBase The time-horizon the rate is valid over.
#' @param discountFactor The discountRate for one time unit. If specified \code{rate} and \code{rateBase} are not used to calculate the discount rate.
#' @param g The optimal gain (g) calculated (used if \code{criterion = "average"}).
#' @param discountMethod Either 'continuous' or 'discrete', corresponding to discount factor exp(-rate/rateBase) or 1/(1+rate/rateBase), respectively. Only used if \code{discountFactor} is \code{NULL}.
#' 
#' @return The rpo (matrix/data frame).
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
calcRPO<-function(mdp, w, iA, sId = ifelse(mdp$timeHorizon>=Inf, mdp$founderStatesLast+1,1):
                    ifelse(mdp$timeHorizon>=Inf, mdp$states + mdp$founderStatesLast,mdp$states)-1, 
                  criterion="expected", dur = "", rate = 0, rateBase = 1, discountFactor = NULL, 
                  g = 0, discountMethod="continuous") {
   iW<-getWIdx(mdp,w)
   iDur<-getWIdx(mdp,dur)
   .checkWIdx(iW,length(mdp$weightNames))
   if (is.null(discountFactor)) {
      if (discountMethod=="continuous") discountFactor<-exp(-rate/rateBase)
      if (discountMethod=="discrete") discountFactor<-1/(1 + rate/rateBase)
   }
   maxS<-ifelse(mdp$timeHorizon>=Inf, mdp$states + mdp$founderStatesLast,mdp$states)
   if (max(sId)>=maxS | min(sId)<0)
      stop("Out of range (sId). Need to be a subset of 0,...,",maxS-1,"!")
   if (length(sId)!=length(iA))
      stop("Vectors sId and iA must have same length!")
   rpo<-NA
   if (criterion=="expected") rpo<-mdp$ptr$calcRPO(2, as.integer(sId), iW, as.integer(iA), g, iDur, discountFactor)
   if (criterion=="discount") rpo<-mdp$ptr$calcRPO(1, as.integer(sId), iW, as.integer(iA), g, iDur, discountFactor)
   if (criterion=="average")  rpo<-mdp$ptr$calcRPO(0, as.integer(sId), iW, as.integer(iA), g, iDur, discountFactor)
   rpo[rpo <= -1.8e+16]<-NA # less than 2 actions
   rpo<-cbind(sId=sId, rpo=rpo)
   return(rpo)
}

#' Save the MDP to binary files
#' 
#' Currently do not save external files.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param prefix A character string with the prefix added to \code{binNames}. Used to identify a specific model.
#' @param getLog Output the log as a message.
#' 
#' @return The rpo (matrix/data frame).
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
saveMDP<-function(mdp,prefix="", getLog=TRUE) {
   mdp$ptr$save2Binary(prefix)
   if (getLog) message(mdp$ptr$getLog())
}


#' Calculate the steady state transition probabilities for the founder process (level 0).
#'
#' Assume that we consider an ergodic/irreducible time-homogeneous Markov chain specified using a policy in the MDP.
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @return A vector with stady state probabilities for all the states at the founder level.
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
calcSteadyStatePr<-function(mdp, getLog=FALSE) {
   pr<-mdp$ptr$steadyStatePr()
   if (getLog) message(mdp$ptr$getLog())
	return(pr)
}



# #' Set the weight of an action.
# #'
# #' @param mdp The MDP loaded using \link{loadMDP}.
# #' @param w The weight.
# #' @param sId The state id of the state.
# #' @param idxA The action index.
# #' @param wLbl The label of the weight we consider.
# #' @return Nothing.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @example tests/machine.Rex
# #' @export
# setActionWeight<-function(mdp, w, sId, iA, wLbl) {
# 	iW<-getWIdx(mdp,wLbl)
# 	
# 	.Call("MDP_SetActionW", mdp$ptr, as.numeric(w), as.integer(sId), as.integer(iA), as.integer(iW), PACKAGE="MDP")
# 	invisible(NULL)
# }
# 




#
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
