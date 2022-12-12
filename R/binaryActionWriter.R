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
#' @param append Logical indicating whether should keep the currents actions (default - TRUE) 
#' defined or delete them and start over (FALSE).
#' @return A list of functions.
#' @author Lars Relund \email{lars@@relund.dk}
#' @note Note all indexes are starting from zero (C/C++ style).
#' @example inst/examples/binaryMDPWriter.R
#' @export
binaryActionWriter<-function(prefix="", binNames=c("actionIdx.bin",
	"actionIdxLbl.bin","actionWeight.bin","actionWeightLbl.bin","transProb.bin"), append=TRUE)
{
	setWeights<-function(labels,...){
		if (wFixed) stop("Weights already added!")
		wCtr<<-length(labels)
		writeBin(as.character(labels), fACostLbl)
		wFixed<<-TRUE
		invisible(NULL)
	}

	addAction<-function(label=NULL, sIdx, weights, prob, ...){     # do not hold now: prop is a matrix with columns (idS,prob)
# 		cat("action:\n")
# 		print(weights)
# 		print(prob)
#		if (length(weights)!=wCtr) stop("Weight length must be ",wCtr,"!")
		#cat(paste("a:(",paste(c(idx),collapse=","),")|",sep=""))
		#cat(paste("a: sId=",sIdx[length(sIdx)],"|",sep=""))
		aRowId<<- aRowId+1
		scpIdx<-NULL
		for (i in 0:(length(prob)/3-1)) scpIdx<-c(scpIdx,prob[1:2+3*i])
		probs<-prob[1:(length(prob)/3)*3]    
		writeBin(as.integer(c(sIdx,scpIdx,-1)), fA)
		if (!is.null(label)) writeBin(c(as.character(aRowId),label), fALbl)   # aRowId added before label
		writeBin(as.numeric(c(probs,-1)), fTransP)
		writeBin(as.numeric(weights), fACost)
		#cat("end action\n")
		invisible(NULL)
	}

	closeWriter<-function(){
    if (!wFixed) stop("Weights must be added using 'setWeights'!")
		cat("\n  Statistics:\n")
		cat("    actions:",aRowId+1,"\n")
		cat("  Closing binary Action writer.\n\n")
		close(fA)
		close(fALbl)
		close(fACost)
		close(fACostLbl)
		close(fTransP)
		invisible(NULL)
	}
   
	binNames<-paste(prefix,binNames,sep="")
	if (append) {
	   # find number of actions already written
	   tmp<-readBin(binNames[1], integer(),n=file.info(binNames[1])$size/4)
	   aRowId<-length(tmp[tmp==-1])-1 # current number of actions defined
	   wFixed<-TRUE  # TRUE if size of weights are fixed
	} else {
	   aRowId<- -1    # current row/line of action in actionIdx file
	   wCtr<- 0    # number of weights in the model
	   wFixed<-FALSE  # TRUE if size of weights are fixed
	}
  mode <- ifelse(append,"ab","wb")
	fA <- file(binNames[1], mode)
	fALbl <- file(binNames[2], mode)
	fACost <- file(binNames[3], mode)
	fACostLbl <- file(binNames[4], mode)
	fTransP <- file(binNames[5], mode)
	v <- list(setWeights = setWeights, addAction = addAction, closeWriter = closeWriter)
	class(v) <- c("binaryActionWriter")
	return(v)
}
