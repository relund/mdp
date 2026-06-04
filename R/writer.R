## functions related to the binary files

#' Function for writing an HMDP model to binary files. The function defines
#' sub-functions which can be used to define an HMDP model saved in a set of binary
#' files.
#'
#' Binary files are efficient for storing large models. Compared to the HMP (XML)
#' format the binary files use less storage space and loads the model faster.
#'
#' The returned writer exposes these functions:
#'
#' * `setWeights(labels, ...)`: sets the labels of the weights used in the actions.
#'   `labels` is a vector of label names. `...` is currently ignored. Call this
#'   before building the model.
#' * `process()`: starts a (sub)process. It may also be used to specify a
#'   traditional MDP using matrices in `MDPtoolbox` style. In that style, `P` is
#'   a list of matrices, one per action, each of size `$S x S$` where `$S$` is
#'   the number of states. Each used row must sum to one, or all entries in a
#'   row must be zero if unused. `R` is a matrix of size `$S x A$`, where `$A$`
#'   is the number of actions, and `D` is a matrix of size `$S x A$` with
#'   durations. If `D` is omitted, all durations are assumed to be 1.
#' * `endProcess()`: ends a (sub)process.
#' * `stage(label = NULL)`: starts a stage. `label` is currently unused in the
#'   binary format.
#' * `endStage()`: ends a stage.
#' * `state(label = NULL)`: starts a state and returns, invisibly, the state id.
#'   That id can later be referenced with scope 3.
#' * `endState()`: ends a state.
#' * `action(scope = NULL, id = NULL, pr = NULL, prob = NULL, weights,
#'   transWeights = NULL, label = NULL, end = FALSE, ...)`: starts an action.
#'   `weights` must be a vector of action weights. `transWeights` must contain
#'   transition weights ordered by transition, with all transition weight labels
#'   for the first transition followed by all labels for the second transition,
#'   and so on. Transition probabilities can be entered in two ways:
#'
#'   1. `prob` contains triples `(scope, id, pr)`.
#'   2. `id` and `pr` are vectors of equal length. If `scope` is omitted, all
#'      scopes default to 1.
#'
#'   See the description of `actionIdx.bin` below. If `end = TRUE`, calling
#'   `endAction()` is not necessary. `...` is currently ignored.
#' * `endAction()`: ends an action. Do not use this if `end = TRUE` was used
#'   when the action was specified.
#' * `includeProcess(prefix, label = NULL, weights, prob, termStates,
#'   transWeights = NULL)`: includes an external process. External processes are
#'   loaded into memory only when needed, which helps with large models. `prefix`
#'   is the external process prefix. `weights` must be a vector of action
#'   weights, and `prob` must contain triples `(scope, idx, pr)`; see the
#'   description of `actionIdx.bin` below. `termStates` must specify the number
#'   of states at the last stage in the external process. Inside an
#'   `includeProcess ... endIncludeProcess`
#'   block, you must specify the father jump actions of the last stage in the
#'   external process. The external process is represented by its first and last
#'   stage together with its jump actions. The function returns, invisibly, the
#'   state ids of the first stage in the external process, which can later be
#'   referenced with scope 3.
#' * `endIncludeProcess()`: ends an `includeProcess` block.
#' * `closeWriter()`: closes the writer. Call this when the model description is
#'   finished.
#'
#' Ten binary files are created:
#'
#' * `stateIdx.bin`: integers defining all states in the format
#'   `"n0 s0 -1 n0 s0 a0 n1 s1 -1 n0 s0 a0 n1 s1 a1 n2 s2 -1 n0 s0 ..."`.
#'   Here `-1` indicates that a new state is considered.
#' * `stateIdxLbl.bin`: character data in the format `sIdx label sIdx label ...`.
#'   Here `sIdx` corresponds to the index or line number in `stateIdxLbl.bin`,
#'   starting from 0. No delimiter is used.
#' * `actionIdx.bin`: integers defining all actions in the format
#'   `sIdx scope idx scope idx scope idx -1 sIdx scope idx scope idx -1 sIdx scope -1 ...`.
#'   `sIdx` corresponds to the index or line number in `stateIdx.bin`, starting
#'   from 0. The following `(scope, idx)` pairs indicate possible transitions.
#'   Scope can take four values:
#'
#'   * `2`: a transition to a child process, at stage zero in the child process.
#'   * `1`: a transition to the next stage in the current process.
#'   * `0`: a transition to the next stage in the father process.
#'   * `3`: a transition to a state specified by its state `sIdx`.
#'
#'   For example, if `scope = 1` and `idx = 2`, the transition is to state
#'   number 3 at the next stage in the current process. If `scope = 3` and
#'   `idx = 5`, the transition is to the state specified at line 6 in
#'   `stateIdxLbl.bin`. This is useful when considering shared child processes.
#' * `actionIdxLbl.bin`: character data in the format `aIdx label aIdx label ...`.
#'   Here `aIdx` corresponds to the index or line number in `actionIdx.bin`,
#'   starting from 0. No delimiter is used.
#' * `actionWeight.bin`: doubles containing action weights in the format
#'   `"c1 c2 c3 c1 c2 c3 ..."`, assuming three weights for each action.
#' * `actionWeightLbl.bin`: character data containing the weight labels in the
#'   format `label1 label2 label3`, assuming three weights for each action.
#' * `transProb.bin`: doubles containing transition probabilities defined in
#'   `actionIdx.bin`. The format is `"p1 p2 p3 -1 p1 -1 p1 p2 -1 ..."`. Here
#'   `-1` indicates that a new action is considered.
#' * `externalProcesses.bin`: character data containing links to external
#'   processes in the format `stageStr prefix stageStr prefix ...`. Here
#'   `stageStr` corresponds to the stage index, for example `n0 s0 a0 n1`, of
#'   the stage corresponding to the first stage in the external process, and
#'   `prefix` is the external process prefix. No delimiter is used.
#' * `transWeight.bin`: doubles containing transition weights in the format
#'   `"t11 t12 t21 t22 -1 ..."`, assuming two transition weights for each
#'   transition and two transitions in the first action.
#' * `transWeightLbl.bin`: character data containing the transition weight
#'   labels.
#'
#' @param prefix A character string with the prefix added to `binNames`.
#' @param binNames A character vector giving the names of the binary files storing the model.
#' @param getLog Output log text.
#' 
#' @return A list of functions.
#' @note Note all indexes are starting from zero (C/C++ style).
#' @example inst/examples/binaryMDPWriter-ex.R
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
               "externalProcesses.bin",
               "transWeight.bin",
               "transWeightLbl.bin"
            ),
            getLog = TRUE
   )
{
   pushContext <- function(value) {
      writerContext <<- c(writerContext, value)
      invisible(NULL)
   }

   popContext <- function() {
      writerContext <<- writerContext[-length(writerContext)]
      invisible(NULL)
   }

   currentContext <- function() {
      if (length(writerContext) == 0) return(NULL)
      writerContext[length(writerContext)]
   }

   requireContext <- function(expected, message) {
      if (!identical(currentContext(), expected)) stop(message, call. = FALSE)
      invisible(NULL)
   }

   setWeights<-function(labels,...){
      if (wFixed) stop("Weights already added!")
      wCtr<<-length(labels)
      writeBin(as.character(labels), fACostLbl)
      wFixed<<-TRUE
      invisible(NULL)
   }

   setTransWeights<-function(labels,...){
      if (tWFixed) stop("Transition weights already added!")
      tWCtr<<-length(labels)
      writeBin(as.character(labels), fTransWLbl)
      tWFixed<<-TRUE
      invisible(NULL)
   }
   
   process<-function(P=NULL, R=NULL, D=NULL, .fromInclude = FALSE){
      if (!wFixed)
         stop("Weights must be added using 'setWeights' before starting building the HMDP!")
      if (length(writerContext)>0 && !identical(currentContext(), "action")) {
         stop("Cannot start a process before closing the current writer block.", call. = FALSE)
      }
      if (.fromInclude) {
         requireContext("action", "Cannot start an included process unless an include-process action is open.")
      }
      pushContext("process")
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
      requireContext("process", "Cannot end a process unless a process is open.")
      if (length(sIdx)>1) sIdx<<-sIdx[1:(length(sIdx)-1)] else sIdx<<-NULL
      # set ctr's for current level
      dCtr<<-idx[length(idx)-2]
      sCtr<<-idx[length(idx)-1]
      aCtr<<-idx[length(idx)]
      popContext()
      invisible(NULL)
   }
   
   stage<-function(label=NULL){
      requireContext("process", "Cannot start a stage outside an open process.")
      pushContext("stage")
      dCtr<<-dCtr+1
      sCtr<<- -1  # reset state ctr
      idx<<-c(idx,dCtr)   # add stage idx
      #cat(paste("d:(",paste(c(idx),collapse=","),"),",dCtr,"|",sep=""))
      invisible(NULL)
   }
   
   endStage<-function(){
      requireContext("stage", "Cannot end a stage unless a stage is open.")
      if (length(idx)>1) idx<<-idx[1:(length(idx)-1)] else idx<<-NULL     # remove stage index
      #cat(paste("-d:(",paste(c(idx),collapse=","),"),",dCtr,"|",sep=""))
      popContext()
      invisible(NULL)
   }
   
   state<-function(label=NULL, end=FALSE){
      requireContext("stage", "Cannot start a state outside an open stage.")
      lastAutoClosedAction <<- FALSE
      pushContext("state")
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
      requireContext("state", "Cannot end a state while another writer block is open. Call endAction() or use action(..., end = TRUE) before endState().")
      idx<<-idx[1:(length(idx)-1)]    # remove state index
      #cat(paste("-s:(",paste(c(idx),collapse=","),")|",sep=""))
      popContext()
      invisible(NULL)
   }
   
   action <-
      function(scope = NULL,
               id = NULL,
               pr = NULL,
               prob = NULL,
               weights,
               transWeights = NULL,
               label = NULL,
               end = FALSE,
               ...) {
         requireContext("state", "Cannot start an action outside an open state.")
         lastAutoClosedAction <<- FALSE
         pushContext("action")
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
      nTrans <- length(scpIdx)/2
      if (tWCtr>0) {
         if (is.null(transWeights)) transWeights <- rep(0, nTrans * tWCtr)
         if (length(transWeights) != nTrans * tWCtr)
            stop("transWeights must have length number of transitions times number of transition weights.")
         writeBin(as.numeric(c(transWeights,-1)), fTransW)
      }
      writeBin(as.numeric(weights), fACost)
      if (!is.null(label)) writeBin(c(as.character(aRowId),label), fALbl)   # aRowId added before label
      if (end) {
         endAction()
         lastAutoClosedAction <<- TRUE
      }
      invisible(NULL)
   }
   
   endAction<-function(){
      if (!identical(currentContext(), "action") && identical(currentContext(), "state") && lastAutoClosedAction) {
         lastAutoClosedAction <<- FALSE
         return(invisible(NULL))
      }
      requireContext("action", "Cannot end an action unless an action is open.")
      idx<<-idx[1:(length(idx)-1)]    # remove action index
      #cat(paste("-a:(",paste(c(idx),collapse=","),")|",sep=""))
      popContext()
      lastAutoClosedAction <<- FALSE
      invisible(NULL)
   }
   
   includeProcess<-function(prefix, label=NULL, weights, prob, termStates, transWeights = NULL){     # prop contain tripeles (scope,idx,prob) - Here all scope must be 2!!
      requireContext("state", "Cannot include a process outside an open state.")
      pushContext("action")
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
      if (tWCtr>0) {
         nTrans <- length(scpIdx)/2
         if (is.null(transWeights)) transWeights <- rep(0, nTrans * tWCtr)
         if (length(transWeights) != nTrans * tWCtr)
            stop("transWeights must have length number of transitions times number of transition weights.")
         writeBin(as.numeric(c(transWeights,-1)), fTransW)
      }
      writeBin(as.numeric(weights), fACost)
      #cat("end action\n")
      maxId<-max(scpIdx[2*(1:(length(scpIdx)/2))])  # number of states to create at the first stage of the child
      process(.fromInclude = TRUE)  # start external subprocess
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
      requireContext("action", "Cannot end an included process unless an include-process action is open.")
      idx<<-idx[1:(length(idx)-1)]    # remove action index
      #cat(paste("-a:(",paste(c(idx),collapse=","),")|",sep=""))
      popContext()
      lastAutoClosedAction <<- FALSE
      invisible(NULL)
   }
   
   closeWriter<-function(){
      if (length(writerContext)>0) {
         stop(
            paste0("Cannot close writer while a ", currentContext(), " is still open."),
            call. = FALSE
         )
      }
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
      close(fTransW)
      close(fTransWLbl)
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
   fTransW <- file(binNames[9], "wb")
   fTransWLbl <- file(binNames[10], "wb")
   idx<-NULL  # containing the stage, state or action idx's
   sIdx<-NULL # containing the state row id's (used to find the state id the action is defined under)
   dCtr<- -1   # current stage at current level
   sCtr<- -1   # current state at current stage
   aCtr<- -1   # current action at current state
   wCtr<- 0    # number of weights in the model
   tWCtr<- 0    # number of transition weights in the model
   sRowId<- -1    # current row/line of state in stateIdx file
   aRowId<- -1    # current row/line of action in actionIdx file
   wFixed<-FALSE  # TRUE if size of weights are fixed
   tWFixed<-FALSE  # TRUE if size of transition weights are fixed
   writerContext<-character()
   lastAutoClosedAction<-FALSE
   v <-
      list(
         setWeights = setWeights,
         setTransWeights = setTransWeights,
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

#' Function for building an HMDP model directly in memory.
#'
#' `memoryMDPWriter()` defines the same main sub-functions as
#' [binaryMDPWriter()], but stores states and actions directly in C++ memory
#' instead of writing intermediate binary files. `closeWriter()` compiles the
#' model and returns the loaded `"HMDP"` object.
#'
#' External or included processes are not supported by `memoryMDPWriter()`.
#'
#' @param prefix A character string kept for compatibility and stored in the
#'   returned object metadata.
#' @param eps The sum of transition probabilities must at most differ `eps`
#'   from one when `check = TRUE`.
#' @param check Check if the MDP seems correct before returning it.
#' @param verbose More output when compiling and running algorithms.
#' @param getLog Output the log messages.
#' @return A list of functions. Calling `closeWriter()` returns an `"HMDP"`
#'   object.
#' @note Note all indexes are starting from zero (C/C++ style).
#' @example inst/examples/memoryMDPWriter-ex.R
#' @export
memoryMDPWriter <- function(prefix = "",
                            eps = 0.00001,
                            check = TRUE,
                            verbose = FALSE,
                            getLog = TRUE) {
   if (!is.logical(verbose)) verbose <- FALSE
   builder <- methods::new(HMDPBuilder, verbose)
   closed <- FALSE

   assertOpen <- function() {
      if (closed) stop("memoryMDPWriter is closed.", call. = FALSE)
      invisible(NULL)
   }

   pushContext <- function(value) {
      writerContext <<- c(writerContext, value)
      invisible(NULL)
   }

   popContext <- function() {
      writerContext <<- writerContext[-length(writerContext)]
      invisible(NULL)
   }

   currentContext <- function() {
      if (length(writerContext) == 0) return(NULL)
      writerContext[length(writerContext)]
   }

   requireContext <- function(expected, message) {
      if (!identical(currentContext(), expected)) stop(message, call. = FALSE)
      invisible(NULL)
   }

   setWeights <- function(labels, ...) {
      assertOpen()
      if (wFixed) stop("Weights already added!")
      wCtr <<- length(labels)
      builder$setWeights(as.character(labels))
      wFixed <<- TRUE
      invisible(NULL)
   }

   setTransWeights <- function(labels, ...) {
      assertOpen()
      if (tWFixed) stop("Transition weights already added!")
      tWCtr <<- length(labels)
      builder$setTransWeights(as.character(labels))
      tWFixed <<- TRUE
      invisible(NULL)
   }

   process <- function(P = NULL, R = NULL, D = NULL, .fromInclude = FALSE) {
      assertOpen()
      if (!wFixed)
         stop("Weights must be added using 'setWeights' before starting building the HMDP!")
      if (.fromInclude) {
         stop("memoryMDPWriter() does not support external processes.", call. = FALSE)
      }
      if (length(writerContext)>0 && !identical(currentContext(), "action")) {
         stop("Cannot start a process before closing the current writer block.", call. = FALSE)
      }
      pushContext("process")
      dCtr <<- -1
      sIdx <<- c(sIdx, NA)
      if (!is.null(P) & !is.null(R)) {
         if (is.null(D)) D <- matrix(1, nrow = nrow(R), ncol = ncol(R))
         stage()
         for (i in 1:nrow(R)) {
            state(label = i)
            for (j in 1:ncol(R)) {
               jIdx <- which(P[[j]][i,]>0)
               if (length(jIdx)==0) next
               action(label = j, pr = P[[j]][i,jIdx], id = jIdx-1,
                      weights = c(D[i,j], R[i,j]), end = TRUE)
            }
            endState()
         }
         endStage()
         endProcess()
      }
      invisible(NULL)
   }

   endProcess <- function() {
      assertOpen()
      requireContext("process", "Cannot end a process unless a process is open.")
      if (length(sIdx)>1) sIdx <<- sIdx[1:(length(sIdx)-1)] else sIdx <<- NULL
      dCtr <<- idx[length(idx)-2]
      sCtr <<- idx[length(idx)-1]
      aCtr <<- idx[length(idx)]
      popContext()
      invisible(NULL)
   }

   stage <- function(label = NULL) {
      assertOpen()
      requireContext("process", "Cannot start a stage outside an open process.")
      pushContext("stage")
      dCtr <<- dCtr+1
      sCtr <<- -1
      idx <<- c(idx, dCtr)
      invisible(NULL)
   }

   endStage <- function() {
      assertOpen()
      requireContext("stage", "Cannot end a stage unless a stage is open.")
      if (length(idx)>1) idx <<- idx[1:(length(idx)-1)] else idx <<- NULL
      popContext()
      invisible(NULL)
   }

   state <- function(label = NULL, end = FALSE) {
      assertOpen()
      requireContext("stage", "Cannot start a state outside an open stage.")
      lastAutoClosedAction <<- FALSE
      pushContext("state")
      sCtr <<- sCtr+1
      aCtr <<- -1
      idx <<- c(idx, sCtr)
      sRowId <<- builder$addState(as.integer(idx), if (is.null(label)) "" else as.character(label))
      sIdx[length(sIdx)] <<- sRowId
      if (end) endState()
      invisible(sRowId)
   }

   endState <- function() {
      assertOpen()
      requireContext("state", "Cannot end a state while another writer block is open. Call endAction() or use action(..., end = TRUE) before endState().")
      idx <<- idx[1:(length(idx)-1)]
      popContext()
      invisible(NULL)
   }

   action <- function(scope = NULL,
                      id = NULL,
                      pr = NULL,
                      prob = NULL,
                      weights,
                      transWeights = NULL,
                      label = NULL,
                      end = FALSE,
                      ...) {
      assertOpen()
      requireContext("state", "Cannot start an action outside an open state.")
      lastAutoClosedAction <<- FALSE
      pushContext("action")
      aCtr <<- aCtr+1
      idx <<- c(idx, aCtr)
      aRowId <<- aRowId+1
      scpIdx <- NULL
      probs <- NULL
      if (!is.null(prob)) {
         for (i in 0:(length(prob)/3-1)) scpIdx <- c(scpIdx, prob[1:2+3*i])
         probs <- prob[1:(length(prob)/3)*3]
      } else if (!is.null(pr)) {
         if (is.null(scope)) scope <- rep(1, length(pr))
         i <- 1:length(pr)-1
         scpIdx[1+i*2] <- scope
         scpIdx[2+i*2] <- id
         probs <- pr
      } else {
         stop("Either 'pr' or 'prob' must be provided.", call. = FALSE)
      }
      nTrans <- length(scpIdx)/2
      if (nTrans == 0) stop("An action must define at least one transition.", call. = FALSE)
      if (tWCtr>0) {
         if (is.null(transWeights)) transWeights <- rep(0, nTrans * tWCtr)
         if (length(transWeights) != nTrans * tWCtr)
            stop("transWeights must have length number of transitions times number of transition weights.")
      } else {
         transWeights <- numeric()
      }
      builder$addAction(
         as.integer(sIdx[length(sIdx)]),
         as.integer(scpIdx[2*(1:nTrans)-1]),
         as.integer(scpIdx[2*(1:nTrans)]),
         as.numeric(probs),
         as.numeric(weights),
         as.numeric(transWeights),
         if (is.null(label)) "" else as.character(label)
      )
      if (end) {
         endAction()
         lastAutoClosedAction <<- TRUE
      }
      invisible(NULL)
   }

   endAction <- function() {
      assertOpen()
      if (!identical(currentContext(), "action") && identical(currentContext(), "state") && lastAutoClosedAction) {
         lastAutoClosedAction <<- FALSE
         return(invisible(NULL))
      }
      requireContext("action", "Cannot end an action unless an action is open.")
      idx <<- idx[1:(length(idx)-1)]
      popContext()
      lastAutoClosedAction <<- FALSE
      invisible(NULL)
   }

   includeProcess <- function(...) {
      assertOpen()
      stop("memoryMDPWriter() does not support external processes.", call. = FALSE)
   }

   endIncludeProcess <- function(...) {
      assertOpen()
      stop("memoryMDPWriter() does not support external processes.", call. = FALSE)
   }

   closeWriter <- function() {
      assertOpen()
      if (length(writerContext)>0) {
         stop(
            paste0("Cannot close writer while a ", currentContext(), " is still open."),
            call. = FALSE
         )
      }
      mdpPtr <- builder$close()
      closed <<- TRUE
      builder <<- NULL
      if (getLog) {
         cat("\n  Statistics:\n")
         cat("    states :", sRowId+1, "\n")
         cat("    actions:", aRowId+1, "\n")
         cat("    weights:", wCtr, "\n\n")
         cat("  Closing memory MDP writer.\n\n")
      }
      .makeMDPList(
         mdpPtr,
         binNames = paste0(prefix, "<memory>"),
         eps = eps,
         check = check,
         getLog = getLog
      )
   }

   idx <- NULL
   sIdx <- NULL
   dCtr <- -1
   sCtr <- -1
   aCtr <- -1
   wCtr <- 0
   tWCtr <- 0
   sRowId <- -1
   aRowId <- -1
   wFixed <- FALSE
   tWFixed <- FALSE
   writerContext <- character()
   lastAutoClosedAction <- FALSE
   v <- list(
      setWeights = setWeights,
      setTransWeights = setTransWeights,
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
   class(v) <- c("memoryMDPWriter")
   return(v)
}


#' Function for writing actions of a HMDP model to binary files. The function defines
#' sub-functions which can be used to define actions saved in a set of binary
#' files. It is assumed that the states have been defined using `binaryMDPWriter` 
#' and that the id of the states is known (can be retrieved using e.g. `stateIdxDf`). 
#'
#' Binary files are efficient for storing large models. Compared to the HMP (XML)
#' format the binary files use less storage space and loading the model is faster.
#'
#' The returned writer exposes these functions:
#'
#' * `setWeights(labels, ...)`: sets the labels of the weights used in the
#'   actions. `labels` is a vector of label names. `...` is currently ignored.
#'   Call this before building the model.
#' * `addAction(label = NULL, sIdx, weights, prob, ...)`: adds an action. `sIdx`
#'   is the id of the state defining the action. `weights` must be a vector of
#'   action weights. `prob` is a matrix `(sIdx, pr)` where the first column
#'   contains the id of the transition state; see the description of
#'   `actionIdx.bin` below, where scope is assumed to be 3. `...` is currently
#'   ignored.
#' * `endAction()`: ends an action.
#' * `closeWriter()`: closes the writer. Call this when the model description is
#'   finished.
#'
#' Five binary files are created:
#'
#' * `actionIdx.bin`: integers defining all actions in the format
#'   `sIdx scope idx scope idx scope idx -1 sIdx scope idx scope idx -1 sIdx scope -1 ...`.
#'   `sIdx` corresponds to the index or line number in `stateIdx.bin`, starting
#'   from 0. The following `(scope, idx)` pairs indicate possible transitions.
#'   Scope can take four values:
#'
#'   * `2`: a transition to a child process, at stage zero in the child process.
#'   * `1`: a transition to the next stage in the current process.
#'   * `0`: a transition to the next stage in the father process.
#'   * `3`: a transition to a state specified by its state `sIdx`.
#'
#'   For example, if `scope = 1` and `idx = 2`, the transition is to state
#'   number 3 at the next stage in the current process. If `scope = 3` and
#'   `idx = 5`, the transition is to the state specified at line 6 in
#'   `stateIdxLbl.bin`. This is useful when considering shared child processes.
#' * `actionIdxLbl.bin`: character data in the format `aIdx label aIdx label ...`.
#'   Here `aIdx` corresponds to the index or line number in `actionIdx.bin`,
#'   starting from 0. No delimiter is used.
#' * `actionWeight.bin`: doubles containing action weights in the format
#'   `"c1 c2 c3 c1 c2 c3 ..."`, assuming three weights for each action.
#' * `actionWeightLbl.bin`: character data containing the weight labels in the
#'   format `label1 label2 label3`, assuming three weights for each action.
#' * `transProb.bin`: doubles containing the transition probabilities defined in
#'   `actionIdx.bin`. The format is `"p1 p2 p3 -1 p1 -1 p1 p2 -1 ..."`. Here
#'   `-1` indicates that a new action is considered.
#'
#' @param prefix A character string with the prefix added to `binNames`.
#' @param binNames A character vector of length 5 giving the names of the binary
#'     files storing the model.
#' @param append Logical indicating whether should keep the currents actions (default - TRUE) 
#' defined or delete them and start over (FALSE).
#' @return A list of functions.
#' @note Note all indexes are starting from zero (C/C++ style).
#' @example inst/examples/binaryMDPWriter-ex.R
#' @export
binaryActionWriter <- function(prefix = "",
                               binNames = c(
                                  "actionIdx.bin",
                                  "actionIdxLbl.bin",
                                  "actionWeight.bin",
                                  "actionWeightLbl.bin",
                                  "transProb.bin",
                                  "transWeight.bin",
                                  "transWeightLbl.bin"
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

   setTransWeights<-function(labels,...){
      if (tWFixed) stop("Transition weights already added!")
      tWCtr<<-length(labels)
      writeBin(as.character(labels), fTransWLbl)
      tWFixed<<-TRUE
      invisible(NULL)
   }
   
   addAction<-function(label=NULL, sIdx, weights, prob, transWeights = NULL, ...){     # do not hold now: prop is a matrix with columns (idS,prob)
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
      if (tWCtr>0) {
         nTrans <- length(scpIdx)/2
         if (is.null(transWeights)) transWeights <- rep(0, nTrans * tWCtr)
         if (length(transWeights) != nTrans * tWCtr)
            stop("transWeights must have length number of transitions times number of transition weights.")
         writeBin(as.numeric(c(transWeights,-1)), fTransW)
      }
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
      close(fTransW)
      close(fTransWLbl)
      invisible(NULL)
   }
   
   binNames<-paste(prefix,binNames,sep="")
   if (append) {
      # find number of actions already written
      tmp<-readBin(binNames[1], integer(),n=file.info(binNames[1])$size/4)
      aRowId<-length(tmp[tmp==-1])-1 # current number of actions defined
      wFixed<-TRUE  # TRUE if size of weights are fixed
      tWFixed<-TRUE
      tWCtr<-0
   } else {
      aRowId<- -1    # current row/line of action in actionIdx file
      wCtr<- 0    # number of weights in the model
      tWCtr<- 0
      wFixed<-FALSE  # TRUE if size of weights are fixed
      tWFixed<-FALSE
   }
   mode <- ifelse(append,"ab","wb")
   fA <- file(binNames[1], mode)
   fALbl <- file(binNames[2], mode)
   fACost <- file(binNames[3], mode)
   fACostLbl <- file(binNames[4], mode)
   fTransP <- file(binNames[5], mode)
   fTransW <- file(binNames[6], mode)
   fTransWLbl <- file(binNames[7], mode)
   v <- list(setWeights = setWeights, setTransWeights = setTransWeights, addAction = addAction, closeWriter = closeWriter)
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
#' @note The model don't have to be loaded, i.e only read the binary files. The state id (`sId`) will
#' not be the same as in the loaded model!
#'
#' @return A data frame with the information.
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
#'   can be 4 values: 
#'   * 0: A transition to the next stage in the father process,
#'   * 1: A transition to next stage in the current process, 
#'   * 2: A transition to a child process (stage zero in the child process), 
#'   * 3: A transition to the state with `sId = idx` is considered. 
#'   
#'   The index string denote the index (id is scope = 3) of the state at the next stage.
#'
#' @note The model don't have to be loaded, i.e only read the binary files. The state id (`sId`) will
#'   not be the same as in the loaded model!
#' @example inst/examples/binaryMDPWriter-ex.R
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



#' Function for writing an HMDP model to a hmp file (XML). The function define
#' sub-functions which can be used to define an HMDP model stored in a hmp file.
#'
#' HMP files are in XML format and human readable using e.g. a text editor.
#' HMP files are not suitable for storing large HMDP models since text files are very
#' verbose. Moreover, approximation of the weights and probabilities may occur since
#' the parser writing the hmp file may no output all digits. If you consider large
#' models then use the binary file format instead.
#'
#' The returned writer exposes these functions:
#'
#' * `setWeights(labels, duration)`: sets the labels of the weights used in the
#'   actions. `labels` is a vector of label names. `duration` identifies which
#'   label corresponds to duration or time. For example, if the first entry in
#'   `labels` is time, then `duration = 1`. Call this before building the model.
#' * `setTransWeights(labels)`: sets the labels of transition-level weights.
#' * `process()`: starts a (sub)process.
#' * `endProcess()`: ends a (sub)process.
#' * `stage(label = NULL)`: starts a stage.
#' * `endStage()`: ends a stage.
#' * `state(label = NULL)`: starts a state and returns the state index `sIdx`.
#' * `endState()`: ends a state.
#' * `action(label = NULL, weights, prob, statesNext = NULL, transWeights = NULL)`: starts an
#'   action. `weights` must be a vector of action weights, and `prob` must
#'   contain triples `(scope, idx, pr)`. `scope` can take three values:
#'
#'   * `0`: a transition to the next stage in the father process.
#'   * `1`: a transition to the next stage in the current process.
#'   * `2`: a transition to a child process, at stage zero in the child process.
#'
#'   The `idx` value denotes the index of the state at the stage considered. For
#'   example, if `scope = 1` and `idx = 2`, the transition is to state number 3
#'   at the next stage in the current process, counting from zero. `scope = 3`
#'   is not supported in the `hmp` file format. `statesNext` is the number of
#'   states in the next stage of the process and is only needed when there is a
#'   transition to the father.
#' * `endAction()`: ends an action.
#' * `closeWriter()`: closes the writer. Call this when the model description is
#'   finished.
#'
#' @param file The name of the file storing the model (e.g. `r.hmp`).
#' @param rate The interest rate (used if consider discounting).
#' @param rateBase The time where the `rate` is taken over, e.g. if the `rate` is 0.1 and `rateBase` is 365 days
#'   then we have an interest rate of 10 percent over the year.
#' @param precision The precision used when checking if probabilities sum to one.
#' @param desc Description of the model.
#' @param getLog Output log text.
#' @return A list of functions.
#' @note Note all indexes are starting from zero (C/C++ style).
#' @example inst/examples/hmpMDPWriter-ex.R
#' @export
hmpMDPWriter<-function(file="r.hmp", rate=0.1, rateBase=1, precision=0.00001, desc="HMP file created using hmpMDPWriter in R", getLog = TRUE) {
   # addLevelRates<-function(rates){
   # 	tr$addTag("i",paste(rates,collapse=" "))
   #    xml2::xml_add_child(doc, "i", paste(rates,collapse=" "))
   # 	invisible(NULL)
   # }
   
   # setSources<-function(s){
   # 	tr$addTag("sources",paste(s-1,collapse=" "))
   #    xml2::xml_add_child(doc, "sources", paste(s-1,collapse=" "))
   # 	invisible(NULL)
   # }
   
   setWeights<-function(labels, duration) {
      if (is.null(duration)) durIdx <<- -1  # no duration specified by negative number
      else durIdx<<-duration
      # tr$addTag("i",rate)
      xml2::xml_add_child(doc, "i", rate)
      
      if (wFixed) stop("Weights already added!")
      for (i in 1:length(labels)) {
         if (i!=durIdx) {
            # tr$addTag("quantities",attrs=c(l=labels[i]))
            xml2::xml_add_child(doc, "quantities", l = labels[i])
         }
      }
      wFixed<<-TRUE
      # tr$addTag("sources","0 1")
      xml2::xml_add_child(doc, "sources", "0 1")
      invisible(NULL)
   }
   
   setTransWeights<-function(labels) {
      for (i in seq_along(labels)) {
         xml2::xml_add_child(doc, "transQuantities", l = labels[i])
      }
      invisible(NULL)
   }
   
   process<-function(){
      if (!wFixed) stop("Weights must be added using 'setWeights' before starting building the HMDP!")
      # tr$addTag("proc",close=FALSE)
      n <<- xml2::xml_add_child(n, "proc")
      invisible(NULL)
   }
   
   endProcess<-function(){
      # tr$closeTag()
      n <<- xml2::xml_parent(n)
      invisible(NULL)
   }
   
   stage<-function(label=NULL){
      if (is.null(label)) {
         # tr$addTag("g",close=FALSE)
         n <<- xml2::xml_add_child(n, "g")
      } else {
         # tr$addTag("g",attrs=c(l=label),close=FALSE)
         n <<- xml2::xml_add_child(n, "g", l = label)
      }
      invisible(NULL)
   }
   
   endStage<-function(){
      # tr$closeTag()
      n <<- xml2::xml_parent(n)
      invisible(NULL)
   }
   
   state<-function(label=NULL){
      if (is.null(label)) {
         # tr$addTag("s",close=FALSE)
         n <<- xml2::xml_add_child(n, "s")
      } else {
         # tr$addTag("s",attrs=c(l=label),close=FALSE)
         n <<- xml2::xml_add_child(n, "s", l = label)
      }
      invisible(NULL)
   }
   
   endState<-function(){
      # tr$closeTag()
      n <<- xml2::xml_parent(n)
      invisible(NULL)
   }
   
   action<-function(label=NULL, weights, prob, statesNext=NULL, transWeights=NULL){  # prop contain tripeles (scope,idx,prob), statesNext: Number of states in the next stage of the process, only needed if have a transition to the father
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
      n <<- xml2::xml_add_child(n, "a")
      tags<-NULL
      if (!is.null(label)) tags<-c(tags,l=label)
      if (term) tags<-c(tags,term='t')
      if (is.null(tags)) {
         # tr$addTag("a",close=FALSE)
      } else {
         # tr$addTag("a",attrs=tags,close=FALSE)
         xml2::xml_attrs(n) <- tags
      }
      if (any(scope==2)) {    # we have an prob to a new child process
         if (!all(prob==c(2,0,1))) stop("Only a deterministic transition to the dummy stage in the child process allowed (prop=(2,0,1))!")
         return(invisible(NULL))   # only a deterministic transition with zero weights allowed in the hmp format
      }
      # tr$addTag("q",paste(weights[which(1:length(weights)!=durIdx)],collapse=" "))  # quantities
      xml2::xml_add_child(n, "q", paste(weights[which(1:length(weights)!=durIdx)],collapse=" "))
      if (!is.null(transWeights)) {
         xml2::xml_add_child(n, "qt", paste(transWeights, collapse=" "))
      }
      probs<-prob[which((1:length(prob)-1)%%3!=0)]   # probs contain pairs (idx,prob)
      if (length(probs)==2) { # deterministic transition
         # tr$addTag("p",probs[1],attrs=c(t='d'))
         xml2::xml_add_child(n, "p", probs[1], t='d')
      } else {
         # tr$addTag("p",paste(probs,collapse=" "),attrs=c(t='s'))
         xml2::xml_add_child(n, "p", paste(probs,collapse=" "), t='s')
      }
      if (durIdx<0) {
         # tr$addTag("d", 1)
         xml2::xml_add_child(n, "d", 1)
      }
      else {
         names(weights) <- NULL
         # tr$addTag("d", weights[durIdx])
         xml2::xml_add_child(n, "d", weights[durIdx])
      }
      invisible(NULL)
   }
   
   endAction<-function(){
      # tr$closeTag()
      n <<- xml2::xml_parent(n)
      invisible(NULL)
   }
   
   closeWriter<-function(){
      # saveXML(tr$value(),file="old.hmp",compression=0,prefix = NULL)
      xml2::write_xml(doc, file)
      if (getLog) cat("\nModel saved to file:",file,"\n")
   }
   
   wFixed<-FALSE  # have weights been added
   durIdx <- NULL   # index of weight storing the duration (number from 1)
   # tr<-xmlTree("mlhmp",dtd=NULL,attrs=c(l=desc,b=rate,dsl=rateBase,precision=precision,version="1.1"))
   doc <- xml2::xml_new_root("mlhmp", l=desc, b=rate, dsl=rateBase, precision=precision, version="1.1")
   n <- doc  # current node
   
   v <- list(setWeights = setWeights, setTransWeights = setTransWeights,
             stage = stage, endStage = endStage, state = state, endState = endState,
             action = action, endAction = endAction, process = process, endProcess = endProcess,
             closeWriter = closeWriter)
   class(v) <- c("hmpMDPWriter")
   return(v)
}
