#' Function for writing an HMDP model to a hmp file (XML). The function define
#' subfunctions which can be used to define an HMDP model stored in a hmp file.
#'
#' HMP files are in XML format and human readable using e.g. a text editor.
#' HMP files are not suitable for storing large HMDP models since text files are very
#' verbose. Moreover, approximation of the weights and probabilities may occur since
#' the parser writing the hmp file may no output all digits. If you consider large
#' models then use the binary file format instead.
#'
#' The functions which can be used are:\itemize{
#'   \item{\code{setWeights(labels, duration)}: }{Set the labels of the weights used in the actions.
#'      \code{labels} is a vector of label names, \code{duration} A number defining which label
#'      that corresponds to duration/time, e.g. if the first entry in labels is time then \code{duration = 1}.
#'      The function must be called before starting building the model.}
#'   \item{\code{process()}: }{Starts a (sub)process.}
#'   \item{\code{endProcess()}: }{Ends a (sub)process.}
#'   \item{\code{stage(label=NULL)}: }{Starts a stage.}
#'   \item{\code{endStage()}: }{Ends a (sub)process.}
#'   \item{\code{state(label=NULL)}: }{Starts a state. Returns the states index number sIdx.}
#'   \item{\code{endState()}: }{Ends a stage.}
#'   \item{\code{action(label=NULL, weights, prob, statesNext=NULL)}: }{Starts an action.
#'     Parameter \code{weights} must be a vector of action weights,
#'     \code{prob} must contain triples of (scope, idx,pr).
#'     The \code{scope} can be 3 values: 0 - A transition to the next stage in the
#'     father process, 1 - A transition to next stage in the current process, 2 - A
#'     transition to a child process (stage zero in the child process). \code{idx} in
#'     the pair denote the index of the state at the stage considered, e.g. if scope=1
#'     and idx=2 we consider state number 3 at next stage in the current process
#'     (number from zero). Note scope = 3 is not supported in the hmp file format!
#'     \code{statesNext} is the number of states in the next stage of the process
#'     (only needed if have a transition to the father).}
#'   \item{\code{endAction()}: }{Ends an action.}
#'   \item{\code{closeWriter()}: }{Close the writer. Must be called when the model description has finished.}}
#'
#' @param file The name of the file storing the model (e.g. mdp.hmp).
#' @param rate The interest rate (used if consider discounting).
#' @param rateBase The time where the \code{rate} is taken over, e.g. if the \code{rate} is 0.1 and \code{rateBase} is 365 days
#'   then we have an interest rate of 10 percent over the year.
#' @param precision The precision used when checking if probabilities sum to one.
#' @param desc Description of the model.
#' @return A list of functions.
#' @author Lars Relund \email{lars@@relund.dk}
#' @note Note all indexes are starting from zero (C/C++ style).
#' @example tests/hmpMDPWriter.R
#' @export
hmpMDPWriter<-function(file="r.hmp", rate=0.1, rateBase=1, precision=0.00001, desc="HMP file created using hmpMDPWriter in R")
{
	addLevelRates<-function(rates){
		tr$addTag("i",paste(rates,collapse=" "))
		invisible(NULL)
	}

	setSources<-function(s){
		tr$addTag("sources",paste(s-1,collapse=" "))
		invisible(NULL)
	}

	setWeights<-function(labels, duration) {
		if (is.null(duration)) durIdx<<- -1  # no duration specified by negative number
		else durIdx<<-duration
		tr$addTag("i",rate)
		if (wFixed) stop("Weights already added!")
		for (i in 1:length(labels)) {
			if (i!=durIdx) tr$addTag("quantities",attrs=c(l=labels[i]))
		}
		wFixed<<-TRUE
		tr$addTag("sources","0 1")
		invisible(NULL)
	}

	process<-function(){
		if (!wFixed) stop("Weights must be added using 'setWeights' before starting building the HMDP!")
		tr$addTag("proc",close=FALSE)
		invisible(NULL)
	}

	endProcess<-function(){
		tr$closeTag()
		invisible(NULL)
	}

	stage<-function(label=NULL){
		if (is.null(label)) {
			tr$addTag("g",close=FALSE)
		} else {
			tr$addTag("g",attrs=c(l=label),close=FALSE)
		}
		invisible(NULL)
	}

	endStage<-function(){
		tr$closeTag()
		invisible(NULL)
	}

	state<-function(label=NULL){
		if (is.null(label)) {
			tr$addTag("s",close=FALSE)
		} else {
			tr$addTag("s",attrs=c(l=label),close=FALSE)
		}
		invisible(NULL)
	}

	endState<-function(){
		tr$closeTag()
		invisible(NULL)
	}

	action<-function(label=NULL, weights, prob, statesNext=NULL){  # prop contain tripeles (scope,idx,prob), statesNext: Number of states in the next stage of the process, only needed if have a transition to the father
		scope<-prob[3*0:(length(prob)/3-1)+1]   # scopes we consider
		if (any(scope==3)) {
			stop("Scope = 3 is not supported in hmp files!")
		}
		term <- FALSE
		if (any(scope==0)) {     # we have an prob that return to the father
			if (is.null(statesNext)) stop("Number of states at the next stage must be specified!")
			if (statesNext!=0) term<-TRUE
			idx<-3*(which(scope==0)-1)+1            # index of scope==0
			prob[idx+1]<-prob[idx+1]+statesNext     # add number of states at next stage to father idx
		}
		tags<-NULL
		if (!is.null(label)) tags<-c(tags,l=label)
		if (term) tags<-c(tags,term='t')
		if (is.null(tags)) {
			tr$addTag("a",close=FALSE)
		} else {
			tr$addTag("a",attrs=tags,close=FALSE)
		}
		if (any(scope==2)) {    # we have an prob to a new child process
			if (!all(prob==c(2,0,1))) stop("Only a deterministic transition to the dummy stage in the child process allowed (prop=(2,0,1))!")
			return(invisible(NULL))   # only a deterministic transition with zero weights allowed in the hmp format
		}
		tr$addTag("q",paste(weights[which(1:length(weights)!=durIdx)],collapse=" "))  # quantities
		probs<-prob[which((1:length(prob)-1)%%3!=0)]   # probs contain pairs (idx,prob)
		if (length(probs)==2) { # deterministic transition
			tr$addTag("p",probs[1],attrs=c(t='d'))
		} else {
			tr$addTag("p",paste(probs,collapse=" "),attrs=c(t='s'))
		}
		if (durIdx<0) tr$addTag("d", 1)
		else tr$addTag("d", weights[durIdx])
		invisible(NULL)
	}

	endAction<-function(){
		tr$closeTag()
		invisible(NULL)
	}

	closeWriter<-function(){
		saveXML(tr$value(),file=file,compression=0,prefix = NULL)
		cat("\nModel saved to file:",file,"\n")
	}

	file <- file
	wFixed<-FALSE
	durIdx <- NULL   # index of weight storing the duration (number from 1)
	tr<-xmlTree("mlhmp",dtd=NULL,attrs=c(l=desc,b=rate,dsl=rateBase,precision=precision,version="1.1"))

	v <- list(setWeights = setWeights,
		stage = stage, endStage = endStage, state = state, endState = endState,
		action = action, endAction = endAction, process = process, endProcess = endProcess,
		closeWriter = closeWriter)
	class(v) <- c("hmpMDPWriter")
	return(v)
}
