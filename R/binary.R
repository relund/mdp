## functions related to the binary files

#' Function for writing an HMDP model to binary files. The function defineds
#' subfunctions which can be used to define an HMDP model saved in a set of binary
#' files.
#'
#' Binary files are efficent for storing large models. Compared to the HMP (XML)
#' format the binary files use less storage space and loads the model faster.
#'
#' The functions which can be used are: 
#' \itemize{
#'   \item{\code{setWeights(labels, ...)}: }{Set the labels of the weights used in the actions. 
#'   \code{labels} is a vector of label names, \code{...} are not used. The function must be called
#'   before starting building the model.}
#'   
#'   \item{\code{process()}: }{Starts a (sub)process. May also be used to specify a traditional MDP 
#'   using matrices (MDPtoolbox style). The style is as follows: P is a list of matrices (one for 
#'   each action) each of size SxS (S = number of states). Each row must sum to one if used or all 
#'   entries in a row must be zero if not used. R is a matrix of size SxA (A = number of 
#'   actions) and D is a matrix of size SxA with durations (optional if not specified assume all 
#'   durations are 1).}
#'   
#'   \item{\code{endProcess()}: }{Ends a (sub)process.}
#'   
#'   \item{\code{stage(label=NULL)}: }{Starts a stage. Currently \code{label} are not used in the
#'   binary format.}
#'   
#'   \item{\code{endStage()}: }{Ends a (sub)process.}
#'   
#'   \item{\code{state(label=NULL)}: }{Starts a state. Returns (invisible) the states id number
#'   which can be used for later reference given scope 3.}
#'   
#'   \item{\code{endState()}: }{Ends a stage.}
#'   
#'   \item{\code{action(scope=NULL, id=NULL, pr=NULL, prob=NULL, weights, label=NULL, end=FALSE, ...)}: }
#'   {Starts an action. Parameter
#'   \code{weights} must be a vector of action weights. There are two ways to enter transition prob 
#'   1) \code{prob} contains triples of (scope,id,pr), 
#'   2) Vectors \code{id} and \code{pr} are of equal size. If \code{scope} not is specified, all scopes are assumed 1.
#'    (see the description of actionIdx.bin below). If \code{end=TRUE} then an \code{endAction()} is not nesseary. 
#'   \code{...} is currently not used.}
#'   
#'   \item{\code{endAction()}: }{Ends an action. Do not use if you set \code{end=TRUE} when you specify an action.}
#'   
#'   \item{\code{includeProcess(prefix, label=NULL, weights, prob, termStates)}: }{Include an 
#'   external process. External processes will only be loaded in memory when needed. That is,
#'   external processes is usefull when considering large models and have problems with memory.
#'   Parameter \code{prefix} is the prefix of the external process. The next parameters specify the
#'   child jump action to the process, i.e. \code{weights} must be a vector of action weights,
#'   \code{prob} must contain triples of (scope,idx,pr) (see the description of actionIdx.bin
#'   below), Finally \code{termStates} must specify the number of states at the last stage in the
#'   external process. Note that inside an \code{includeProcess ... endIncludeProcess} you must
#'   specify the father jump actions of the last stage in the external process. An external process
#'   is represented using its first and last stage, together with its jump actions. Returns 
#'   (invisible) the state id's of the first stage in the external process which can be used for 
#'   later reference given scope 3.}
#'   
#'   \item{\code{endIncludeProcess()}: }{Ends an includeProcess.}
#'   
#'   \item{\code{closeWriter()}: }{Close the writer. Must be called when the model description has
#'   finished.}}
#'
#' Eight binary files are created using the following format:\itemize{
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
#' stage defined by the indexes.}
#' \item{externalProcesses.bin: }{File of characters in the format "stageStr prefix stageStr prefix
#' ..." Here stageStr corresponds to the index (e.g. n0 s0 a0 n1) of the stage corresponding to the
#' first stage in the external process and prefix to the prefix of the external process. Note no
#' delimiter is used.}}
#'
#' @param prefix A character string with the prefix added to \code{binNames}.
#' @param binNames A character vector giving the names of the binary files storing the model.
#' @param getLog Output log text.
#' 
#' @return A list of functions.
#' @author Lars Relund \email{lars@@relund.dk}
#' @note Note all indexes are starting from zero (C/C++ style).
#' @example inst/examples/binaryMDPWriter.R
#' @export
binaryMDPWriter <-
   function(prefix = "",
            binNames = c(
               "stateIdx.bin",
               "stateIdxLbl.bin",
               "actionIdx.bin",
               "actionIdxLbl.bin",
               "actionWeight.bin",
               "actionWeightLbl.bin",
               "transProb.bin",
               "externalProcesses.bin"
            ),
            getLog = TRUE
   )
{
   setWeights<-function(labels,...){
      if (wFixed) stop("Weights already added!")
      wCtr<<-length(labels)
      writeBin(as.character(labels), fACostLbl)
      wFixed<<-TRUE
      invisible(NULL)
   }
   
   process<-function(P=NULL, R=NULL, D=NULL){
      if (!wFixed)
         stop("Weights must be added using 'setWeights' before starting building the HMDP!")
      dCtr<<- -1  # reset stage ctr
      sIdx<<-c(sIdx,NA)
      if (!is.null(P) & !is.null(R)) { # MDP specified using MDPtoolbox style
         if (is.null(D)) D<-matrix(1,nrow = nrow(R), ncol = ncol(R))
         stage()
         for (i in 1:nrow(R)) {
            state(label=i)
            for (j in 1:ncol(R)) {
               jIdx<-which(P[[j]][i,]>0)
               if (length(jIdx)==0) next
               action(label=j, pr=P[[j]][i,jIdx], id = jIdx-1, weights = c(D[i,j],R[i,j]), end = TRUE)
            }
            endState()
         }
         endStage()
         endProcess()
      }
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
   
   action <-
      function(scope = NULL,
               id = NULL,
               pr = NULL,
               prob = NULL,
               weights,
               label = NULL,
               end = FALSE,
               ...) {
         # prop contain tripeles (scope,idx,prob)
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
      if (!is.null(prob)) {
         for (i in 0:(length(prob)/3-1)) scpIdx<-c(scpIdx,prob[1:2+3*i])
         probs<-prob[1:(length(prob)/3)*3]
         writeBin(as.integer(c(sIdx[length(sIdx)],scpIdx,-1)), fA)
         writeBin(as.numeric(c(probs,-1)), fTransP)
         #cat("end action\n")
      } else if (!is.null(pr)) {
         #cat("pr:",paste0(pr,collapse = ",")," id:",paste0(id,collapse = ",")," w:",paste0(weights,collapse = ","),"\n"); cat
         if (is.null(scope)) scope<-rep(1,length(pr))
         i<-1:length(pr)-1
         scpIdx[1+i*2]<-scope
         scpIdx[2+i*2]<-id
         writeBin(as.integer(c(sIdx[length(sIdx)],scpIdx,-1)), fA)
         writeBin(as.numeric(c(pr,-1)), fTransP)
      }
      writeBin(as.numeric(weights), fACost)
      if (!is.null(label)) writeBin(c(as.character(aRowId),label), fALbl)   # aRowId added before label
      if (end) endAction()
      invisible(NULL)
   }
   
   endAction<-function(){
      idx<<-idx[1:(length(idx)-1)]    # remove action index
      #cat(paste("-a:(",paste(c(idx),collapse=","),")|",sep=""))
      invisible(NULL)
   }
   
   includeProcess<-function(prefix, label=NULL, weights, prob, termStates){     # prop contain tripeles (scope,idx,prob) - Here all scope must be 2!!
      stateId<-NULL # to store state id's
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
      writeBin(c(paste(idx,collapse=","), prefix), fExt)  # store the external process' name
      pr<-as.numeric( t(matrix(c(rep(1,termStates), 1:termStates-1, rep(1/termStates,termStates)), ncol=3)) )
      for (i in 0:maxId) {
         # create the states in the first stage (with no actions)
         stateId <- c(stateId, state())
         action(weights = rep(0, length(weights)),
                prob = pr,
                end = TRUE) # dummy action of external process with transition to all terminal states
         endState()
      }
      endStage()
      # now the user has to include the last stage using the normal syntax
      invisible(stateId)
   }
   
   endIncludeProcess<-function() {
      endProcess()   # end external subprocess
      idx<<-idx[1:(length(idx)-1)]    # remove action index
      #cat(paste("-a:(",paste(c(idx),collapse=","),")|",sep=""))
      invisible(NULL)
   }
   
   closeWriter<-function(){
      if (getLog) {
         cat("\n  Statistics:\n")
         cat("    states :",sRowId+1,"\n")
         cat("    actions:",aRowId+1,"\n")
         cat("    weights:",wCtr,"\n\n")
         cat("  Closing binary MDP writer.\n\n") 
      }
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
   v <-
      list(
         setWeights = setWeights,
         stage = stage,
         endStage = endStage,
         state = state,
         endState = endState,
         action = action,
         endAction = endAction,
         includeProcess = includeProcess,
         endIncludeProcess = endIncludeProcess,
         process = process,
         endProcess = endProcess,
         closeWriter = closeWriter
      )
   class(v) <- c("binaryMDPWriter")
   return(v)
}


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
#'   \item{\code{addAction(label=NULL, sIdx, weights, prob, ...)}: }{Add an action. Parameter 
#'      \code{sIdx} is the id of the state defining the action, \code{weights} must be a vector of 
#'      action weights,
#'      \code{prob} is a matrix (sIdx,pr)  where the first column contain the id of the transition 
#'      state (see the description of actionIdx.bin below - scope is assumed to the 3), \code{...} 
#'      is currently not used.}
#'   \item{\code{endAction()}: }{Ends an action.}
#'   \item{\code{closeWriter()}: }{Close the writer. Must be called when the model description has 
#'     finished.}}
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
binaryActionWriter <- function(prefix = "",
                               binNames = c(
                                  "actionIdx.bin",
                                  "actionIdxLbl.bin",
                                  "actionWeight.bin",
                                  "actionWeightLbl.bin",
                                  "transProb.bin"
                               ),
                               append = TRUE
)
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



#' Info about the states in the binary files of the HMDP model under consideration.
#'
#' @param prefix A character string with the prefix added to til binary files.
#' @param labels Should labels be extracted.
#' @param stateStr Should state strings be extracted. If false then add columns (n0, s0, a0, ...)
#'   where n0 the index of the stage at level 0, s0 the index of the state and a0 the index of the
#'   action. If the HMDP has more than one level columns index (d1, s1, a1, ...) are added.
#' @param fileS The binary file containing the description of states.
#' @param labelS The binary file containing the state labels.
#'
#' @note The model don't have to be loaded, i.e only read the binary files. The state id (sId) will
#' not be the same as in the loaded model!
#'
#' @return A data frame with the information.
#'
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
getBinInfoStates <-
   function(prefix = "",
            labels = TRUE,
            stateStr = TRUE,
            fileS = "stateIdx.bin",
            labelS = "stateIdxLbl.bin"
   ) {
   fileS <- paste(prefix, fileS, sep = "")
   tmp <- readBin(fileS, integer(), n = file.info(fileS)$size / 4)
   rows <- length(tmp[tmp == -1])
   if (!stateStr) {
      cols <- max(rle(tmp != -1)$length)
      mat <- as.data.frame(matrix(NA, nrow = rows, ncol = cols + 1))
      idx <- c(0, which(tmp == -1))
      for (i in 1:(length(idx) - 1))
         mat[i, 1:(idx[i + 1] - idx[i] - 1) + 1] <-
         tmp[(idx[i] + 1):(idx[i + 1] - 1)]
      levels <- cols %/% 3 + 1
      if (levels == 1)
         colnames(mat) <- c("sId", paste(c("n", "s"), levels - 1, sep = ""))
      if (levels > 1)
         colnames(mat) <-
         c("sId", paste(c("n", "s", "a"), rep(0:(levels - 2), each = 3), sep = ""), paste(c("n", "s"), levels -
                                                                                             1, sep = ""))
   } else {
      mat <- as.data.frame(matrix(NA, nrow = rows, ncol = 2))
      idx <- c(0, which(tmp == -1))
      for (i in 1:(length(idx) - 1))
         mat[i, 2] <- paste(tmp[(idx[i] + 1):(idx[i + 1] - 1)], collapse = ",")
      colnames(mat) <- c("sId", "stageStr")
   }
   mat[, 1] <- 1:nrow(mat) - 1
   if (labels) {
      labelS <- paste(prefix, labelS, sep = "")
      tmp <- readBin(labelS, character(), n = file.info(labelS)$size)
      tmp <-
         as.data.frame(matrix(tmp, ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
      colnames(tmp) <- c("sId", "label")
      mat <- merge(mat, tmp, all.x = TRUE)
   }
   return(dplyr::as_tibble(mat))
}



#' Info about the actions in the HMDP model under consideration.
#'
#' @param prefix A character string with the prefix added to til binary files.
#' @param labels Should labels be extracted.
#' @param fileA The binary file containing the description of actions.
#' @param filePr The binary file containing the description of transition probabilities.
#' @param fileW The binary file containing the description of weights.
#' @param fileLabelA The binary file containing the action labels.
#' @param fileLabelW The binary file containing the weight labels.
#'
#' @return A data frame with the information. Scope string contain the scope of the transitions and
#'   can be 4 values: 2 - A transition to a child process (stage zero in the child process), 1 - A
#'   transition to next stage in the current process, 0 - A transition to the next stage in the
#'   father process. Finally, if scope = 3 then a transition to the state with sId = idx is
#'   considered. The index string denote the index (id is scope = 3) of the state at the next stage.
#'
#' @note The model don't have to be loaded, i.e only read the binary files. The state id (sId) will
#' not be the same as in the loaded model!
#'
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
getBinInfoActions<-function(prefix="", labels = TRUE, fileA="actionIdx.bin",
                         filePr="transProb.bin", fileW="actionWeight.bin",
                         fileLabelW="actionWeightLbl.bin", fileLabelA="actionIdxLbl.bin")
{
   fileA<-paste(prefix,fileA,sep="")
   filePr<-paste(prefix,filePr,sep="")
   fileW<-paste(prefix,fileW,sep="")
   fileLabelW<-paste(prefix,fileLabelW,sep="")

   tmpA<-readBin(fileA, integer(),n=file.info(fileA)$size/4)
   tmpPr<-readBin(filePr, numeric(),n=file.info(filePr)$size/8)
   tmpW<-readBin(fileW, numeric(),n=file.info(fileW)$size/8)
   colNames<-readBin(fileLabelW, character(),n=file.info(fileLabelW)$size)
   rows<-length(tmpA[tmpA==-1])
   cols<-5+length(colNames)

   mat<-as.data.frame(matrix(NA,nrow=rows,ncol=cols))
   mat[,1]<-1:nrow(mat)-1
   idxA<-c(0,which(tmpA== -1))
   idxPr<-c(0,which(tmpPr== -1))
   for (i in 1:(length(idxA)-1)) {
      v<-tmpA[(idxA[i]+1):(idxA[i+1]-1)]
      mat[i,2]<-v[1]
      mat[i,3]<-paste(v[ seq(2,length(v),2) ],collapse = ",")
      mat[i,4]<-paste(v[ seq(3,length(v),2) ],collapse = ",")
      v<-tmpPr[(idxPr[i]+1):(idxPr[i+1]-1)]
      mat[i,5]<-paste(v,collapse = ",")
   }

   for (i in 1:rows) {
      mat[i,6:cols]<-tmpW[(length(colNames)*(i-1)+1):(length(colNames)*i)]
   }
   colnames(mat)<-c("aId","sId","scope","index","pr", colNames)

   if (labels) {
      fileLabelA<-paste(prefix,fileLabelA,sep="")
      tmp<-readBin(fileLabelA, character(),n=file.info(fileLabelA)$size)
      tmp<-as.data.frame(matrix(tmp,ncol=2,byrow=TRUE),stringsAsFactors = FALSE)
      colnames(tmp)<-c("aId","label")
      tmp$aId<-as.numeric(tmp$aId)
      mat<-merge(mat,tmp,all.x=TRUE)
   }
   return(dplyr::as_tibble(mat))
}

