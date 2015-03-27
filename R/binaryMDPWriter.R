#' Function for writing an HMDP model to binary files. The function defineds
#' subfunctions which can be used to define an HMDP model saved in a set of binary
#' files.
#'
#' Binary files are efficent for storing large models. Compared to the HMP (XML)
#' format the binary files use less storage space and loading the model is faster.
#'
#' The functions which can be used are: \itemize{
#'   \item{\code{setWeights(labels, ...)}: }{Set the labels of the weights used in the actions.
#'      \code{labels} is a vector of label names, \code{...} are not used.
#'      The function must be called before starting building the model.}
#'   \item{\code{process()}: }{Starts a (sub)process.}
#'   \item{\code{endProcess()}: }{Ends a (sub)process.}
#'   \item{\code{stage(label=NULL)}: }{Starts a stage. Currently \code{label} are not used in the binary format.}
#'   \item{\code{endStage()}: }{Ends a (sub)process.}
#'   \item{\code{state(label=NULL)}: }{Starts a state. Returns (invisible) the states index number sIdx.}
#'   \item{\code{endState()}: }{Ends a stage.}
#'   \item{\code{action(label=NULL, weights, prob, ...)}: }{Starts an action. Parameter \code{weights} must be a vector of action weights,
#'      \code{prob} must contain triples of (scope,idx,pr) (see the description of actionIdx.bin below), \code{...} is currently not used.}
#'   \item{\code{endAction()}: }{Ends an action.}
#'   \item{\code{closeWriter()}: }{Close the writer. Must be called when the model description has finished.}}
#'
#' Seven binary files are created using the following format:\itemize{
#' \item{stateIdx.bin: }{File of integers containing the indexes defining all states in the format
#' "n0 s0 -1 n0 s0 a0 n1 s1 -1 n0 s0 a0 n1 s1 a1 n2 s2 -1 n0 s0 ...". Here -1 is
#' used to indicate that a new state is considered (new line).}
#' \item{stateIdxLbl.bin: }{File of characters in the format "sIdx label sIdx label ..." Here
#' sIdx corresponds to the index/line number in stateIdxLbl.bin (index starts from 0).
#' Note no delimiter is used.}
#' \item{actionIdx.bin: }{File of integers containing the indexes defining all actions in the format
#' "sIdx scope idx scope idx scope idx -1 sIdx scope idx scope idx -1 sIdx scope -1 ...".
#' sIdx corresponds to the index/line number in stateIdx.bin (index starts from 0).
#' Next pairs
#' (scope idx) will follow indicating the possible transitions. Scope can be 4 values:
#' 2 - A transition to a child process (stage zero in the child process), 1 - A transition
#' to next stage in the current process, 0 - A transition to the next stage in the father
#' process. Here idx in the pair denote the index of the state at the stage considered,
#' e.g. if scope=1 and idx=2 we consider state number 3 at next stage in the current
#' process. Finally, if scope = 3 then a transition to a state specified by it's state sIdx
#' is given. That is, if scope=3 and idx=5 then
#' we have a transition to the state specified at line 6 in stateIdxLbl.bin.
#' This is usefull when considering shared child processes.}
#' \item{actionIdxLbl.bin: }{File of characters in the format "aIdx label aIdx label ..." Here
#' aIdx corresponds to the index/line number in actionIdx.bin (index starts from 0).
#' Note no delimiter is used.}
#' \item{actionWeight.bin: }{File of doubles containing the weights of the actions in the format
#' "c1 c2 c3 c1 c2 c3 ..." assuming three weights for each action.}
#' \item{actionWeightLbl.bin: }{File of characters containing the labels of the
#' weights in the format "lable1 label2 label3" assuming three weights for each action. }
#' \item{transProb.bin: }{File of doubles containing the probabilities of the transitions defined in
#' actions in actionIdx.bin. The format is "p1 p2 p3 -1 p1 -1 p1 p2 -1 ...". Here -1 is used to
#' indicate that a new action is considered (new line).} \item{externalProcesses.bin: }{File of
#' characters containing links to the external processes. The format is "n0 s0 prefix -1 n0 s0 a0 n1
#' s1 prefix -1 ...". Here -1 is used to indicate that a new external process is considered for the
#' stage defined by the indexes.}}
#'
#' @param prefix A character string with the prefix added to \code{binNames}.
#' @param binNames A character vector giving the names of the binary files storing the model.
#' @return A list of functions.
#' @author Lars Relund \email{lars@@relund.dk}
#' @note Note all indexes are starting from zero (C/C++ style).
#' @example tests/binaryMDPWriter.Rex
#' @export
binaryMDPWriter<-function(prefix="", binNames=c("stateIdx.bin","stateIdxLbl.bin","actionIdx.bin",
	"actionIdxLbl.bin","actionWeight.bin","actionWeightLbl.bin","transProb.bin","externalProcesses.bin"))
{
	setWeights<-function(labels,...){
		if (wFixed) stop("Weights already added!")
		wCtr<<-length(labels)
		writeBin(as.character(labels), fACostLbl)
		wFixed<<-TRUE
		invisible(NULL)
	}

	process<-function(){
		if (!wFixed) stop("Weights must be added using 'setWeights' before starting building the HMDP!")
		dCtr<<- -1  # reset stage ctr
		sIdx<<-c(sIdx,NA)
		invisible(NULL)
	}

	endProcess<-function(){
		if (length(sIdx)>1) sIdx<<-sIdx[1:(length(sIdx)-1)] else sIdx<<-NULL
		# set ctr's for current level
		dCtr<<-idx[length(idx)-2]
		sCtr<<-idx[length(idx)-1]
		aCtr<<-idx[length(idx)]
		invisible(NULL)
	}

	stage<-function(label=NULL){
		dCtr<<-dCtr+1
		sCtr<<- -1  # reset state ctr
		idx<<-c(idx,dCtr)   # add stage idx
		#cat(paste("d:(",paste(c(idx),collapse=","),"),",dCtr,"|",sep=""))
		invisible(NULL)
	}

	endStage<-function(){
		if (length(idx)>1) idx<<-idx[1:(length(idx)-1)] else idx<<-NULL     # remove stage index
		#cat(paste("-d:(",paste(c(idx),collapse=","),"),",dCtr,"|",sep=""))
		invisible(NULL)
	}

	state<-function(label=NULL, end=FALSE){
		#cat("(",label,") ",sep="")
		sCtr<<-sCtr+1
		aCtr<<- -1  # reset action ctr
		idx<<-c(idx,sCtr)   # add state idx
		writeBin(as.integer(c(idx,-1)), fS)
		sRowId<<- sRowId+1
		sIdx[length(sIdx)]<<-sRowId
		#cat(paste("s:(",paste(c(idx),collapse=","),")",sRowId,"|",sep=""))
		if (!is.null(label)) writeBin(c(as.character(sRowId),label), fSLbl)   # sRowId added before label
		if (end) endState()
		invisible(sRowId)
	}

	endState<-function(){
		idx<<-idx[1:(length(idx)-1)]    # remove state index
		#cat(paste("-s:(",paste(c(idx),collapse=","),")|",sep=""))
		invisible(NULL)
	}

	action<-function(label=NULL, weights, prob, end=FALSE, ...){     # prop contain tripeles (scope,idx,prob)
		#cat("action:\n")
		#print(weights)
		#print(prob)
		#if (is.null(label) | label=="") stop("label = null");
		#if (length(weights)!=wCtr) stop("Weight length must be ",wCtr,"!")
		aCtr<<-aCtr+1
		idx<<-c(idx,aCtr)   # add action idx
		#cat(paste("a:(",paste(c(idx),collapse=","),")|",sep=""))
		#cat(paste("a: sId=",sIdx[length(sIdx)],"|",sep=""))
		scpIdx<-NULL
		aRowId<<- aRowId+1
		for (i in 0:(length(prob)/3-1)) scpIdx<-c(scpIdx,prob[1:2+3*i])
		probs<-prob[1:(length(prob)/3)*3]
#        if (any(scpIdx<0) | any(probs<0)) {
#            print(label)
#            print(prob)
#            print(scpIdx)
#            print(probs)
#            stop()
#        }
		writeBin(as.integer(c(sIdx[length(sIdx)],scpIdx,-1)), fA)
		if (!is.null(label)) writeBin(c(as.character(aRowId),label), fALbl)   # aRowId added before label
		writeBin(as.numeric(c(probs,-1)), fTransP)
		writeBin(as.numeric(weights), fACost)
		#cat("end action\n")
		if (end) endAction()
		invisible(NULL)
	}

	endAction<-function(){
		idx<<-idx[1:(length(idx)-1)]    # remove action index
		#cat(paste("-a:(",paste(c(idx),collapse=","),")|",sep=""))
		invisible(NULL)
	}

   includeProcess<-function(prefix, label, weights, prob, termStates){     # prop contain tripeles (scope,idx,prob) - Here all scope must be 2!!
      #cat("action:\n")
      #print(weights)
      #print(prob)
      #if (is.null(label) | label=="") stop("label = null");
      #if (length(weights)!=wCtr) stop("Weight length must be ",wCtr,"!")
      aCtr<<-aCtr+1
      idx<<-c(idx,aCtr)   # add action idx
      #cat(paste("a:(",paste(c(idx),collapse=","),")|",sep=""))
      #cat(paste("a: sId=",sIdx[length(sIdx)],"|",sep=""))
      scpIdx<-NULL
      aRowId<<- aRowId+1
      for (i in 0:(length(prob)/3-1)) scpIdx<-c(scpIdx,prob[1:2+3*i])
      probs<-prob[1:(length(prob)/3)*3]
      #        if (any(scpIdx<0) | any(probs<0)) {
      #            print(label)
      #            print(prob)
      #            print(scpIdx)
      #            print(probs)
      #            stop()
      #        }
      writeBin(as.integer(c(sIdx[length(sIdx)],scpIdx,-1)), fA)
      if (!is.null(label)) writeBin(c(as.character(aRowId),label), fALbl)   # aRowId added before label
      writeBin(as.numeric(c(probs,-1)), fTransP)
      writeBin(as.numeric(weights), fACost)
      #cat("end action\n")
      maxId<-max(scpIdx[2*(1:(length(scpIdx)/2))])  # number of states to create at the first stage of the child
      process()  # start external subprocess
        stage()  # first stage of the external process
        writeBin(c(as.character(idx), prefix, -1), fExt)  # store the external process' name
        pr<-as.numeric( t(matrix(c(rep(1,termStates), 1:termStates-1, rep(1/termStates,termStates)), ncol=3)) )
        for (i in 0:maxId) {  # create the states in the first stage (with no actions)
           state()
             w$action(weights=rep(0,length(weights)), prob=pr, end=TRUE) # dummy action of external process with transition to all terminal states
           endState()
        }
        endStage()
        # now the user has to include the last stage using the normal syntax
      invisible(NULL)
   }

   endIncludeProcess<-function() {
      endProcess()   # end external subprocess
      idx<<-idx[1:(length(idx)-1)]    # remove action index
      #cat(paste("-a:(",paste(c(idx),collapse=","),")|",sep=""))
      invisible(NULL)
   }

	closeWriter<-function(){
		cat("\n  Statistics:\n")
		cat("    states :",sRowId+1,"\n")
		cat("    actions:",aRowId+1,"\n")
		cat("    weights:",wCtr,"\n\n")
		cat("  Closing binary MDP writer.\n\n")
		close(fS)
		close(fSLbl)
		close(fA)
		close(fALbl)
		close(fACost)
		close(fACostLbl)
		close(fTransP)
		close(fExt)
		invisible(NULL)
	}

	binNames<-paste(prefix,binNames,sep="")
	fS <- file(binNames[1], "wb")
	fSLbl <- file(binNames[2], "wb")
	fA <- file(binNames[3], "wb")
	fALbl <- file(binNames[4], "wb")
	fACost <- file(binNames[5], "wb")
	fACostLbl <- file(binNames[6], "wb")
	fTransP <- file(binNames[7], "wb")
   fExt <- file(binNames[8], "wb")
	idx<-NULL  # containing the stage, state or action idx's
	sIdx<-NULL # containing the state row id's (used to find the state id the action is defined under)
	dCtr<- -1   # current stage at current level
	sCtr<- -1   # current state at current stage
	aCtr<- -1   # current action at current state
	wCtr<- 0    # number of weights in the model
	sRowId<- -1    # current row/line of state in stateIdx file
	aRowId<- -1    # current row/line of action in actionIdx file
	wFixed<-FALSE  # TRUE if size of weights are fixed
	v <- list(setWeights = setWeights, stage = stage, endStage = endStage, state = state, endState = endState,
		action = action, endAction = endAction, includeProcess = includeProcess, endIncludeProcess = endIncludeProcess, process = process, endProcess = endProcess,
		closeWriter = closeWriter)
	class(v) <- c("binaryMDPWriter")
	return(v)
}
