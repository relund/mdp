#' Function for writing actions of a HMDP model to binary files. The function defineds
#' subfunctions which can be used to define actions saved in a set of binary
#' files. It is assumed that the states have been defined using \code{binaryMDPWriter} 
#' and that the id of the states is known (can be retrived using e.g. \code{stateIdxDf}). 
#'
#' Binary files are efficent for storing large models. Compared to the HMP (XML)
#' format the binary files use less storage space and loading the model is faster.
#'
#' The functions which can be used are: \itemize{
#'   \item{\code{setWeights(labels, ...)}: }{Set the labels of the weights used in the actions.
#'      \code{labels} is a vector of label names, \code{...} are not used.
#'      The function must be called before starting building the model.}
#'   \item{\code{addAction(label=NULL, sIdx, weights, prob, ...)}: }{Add an action. Parameter \code{sIdx} is the id of the state defining the action, \code{weights} must be a vector of action weights,
#'      \code{prob} is a matrix (sIdx,pr)  where the first column contain the id of the transition state (see the description of actionIdx.bin below - scope is assumed to the 3), \code{...} is currently not used.}
#'   \item{\code{endAction()}: }{Ends an action.}
#'   \item{\code{closeWriter()}: }{Close the writer. Must be called when the model description has finished.}}
#'
#' Five binary files are created using the following format:\itemize{
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
#' \item{transProb.bin: }{File of doubles containing the probabilities of the transitions
#' defined in actions in actionIdx.bin. The format is
#' "p1 p2 p3 -1 p1 -1 p1 p2 -1 ...". Here -1 is
#' used to indicate that a new action is considered (new line).}}
#'
#' @param prefix A character string with the prefix added to \code{binNames}.
#' @param binNames A character vector of length 5 giving the names of the binary
#'     files storing the model.
#' @return A list of functions.
#' @author Lars Relund \email{lars@@relund.dk}
#' @note Note all indexes are starting from zero (C/C++ style).
#' @example tests/binaryMDPWriter.Rex
#' @export
binaryActionWriter<-function(prefix="", binNames=c("actionIdx.bin",
	"actionIdxLbl.bin","actionWeight.bin","actionWeightLbl.bin","transProb.bin"))
{
	setWeights<-function(labels,...){
		if (wFixed) stop("Weights already added!")
		wCtr<<-length(labels)
		writeBin(as.character(labels), fACostLbl)
		wFixed<<-TRUE
		invisible(NULL)
	}

	addAction<-function(label=NULL, sIdx, weights, prob, ...){     # prop is a matrix with columns (idS,prob)
# 		cat("action:\n")
# 		print(weights)
# 		print(prob)
#		if (length(weights)!=wCtr) stop("Weight length must be ",wCtr,"!")
		#cat(paste("a:(",paste(c(idx),collapse=","),")|",sep=""))
		#cat(paste("a: sId=",sIdx[length(sIdx)],"|",sep=""))
		aRowId<<- aRowId+1
		writeBin(as.integer(c(sIdx,t(cbind(3,prob[,1])),-1)), fA)
		if (!is.null(label)) writeBin(c(as.character(aRowId),label), fALbl)   # aRowId added before label
		writeBin(as.numeric(c(prob[,2],-1)), fTransP)
		writeBin(as.numeric(weights), fACost)
		#cat("end action\n")
		invisible(NULL)
	}

	closeWriter<-function(){
    if (!wFixed) stop("Weights must be added using 'setWeights'!")
		cat("\n  Statistics:\n")
		cat("    actions:",aRowId+1,"\n")
		cat("    weights:",wCtr,"\n\n")
		cat("  Closing binary Action writer.\n\n")
		close(fA)
		close(fALbl)
		close(fACost)
		close(fACostLbl)
		close(fTransP)
		invisible(NULL)
	}

	binNames<-paste(prefix,binNames,sep="")
	fA <- file(binNames[1], "wb")
	fALbl <- file(binNames[2], "wb")
	fACost <- file(binNames[3], "wb")
	fACostLbl <- file(binNames[4], "wb")
	fTransP <- file(binNames[5], "wb")
	wCtr<- 0    # number of weights in the model
	aRowId<- -1    # current row/line of action in actionIdx file
	wFixed<-FALSE  # TRUE if size of weights are fixed
	v <- list(setWeights = setWeights, addAction = addAction, closeWriter = closeWriter)
	class(v) <- c("binaryActionWriter")
	return(v)
}
