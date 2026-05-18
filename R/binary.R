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
#'   label = NULL, end = FALSE, ...)`: starts an action. `weights` must be a
#'   vector of action weights. Transition probabilities can be entered in two
#'   ways:
#'
#'   1. `prob` contains triples `(scope, id, pr)`.
#'   2. `id` and `pr` are vectors of equal length. If `scope` is omitted, all
#'      scopes default to 1.
#'
#'   See the description of `actionIdx.bin` below. If `end = TRUE`, calling
#'   `endAction()` is not necessary. `...` is currently ignored.
#' * `endAction()`: ends an action. Do not use this if `end = TRUE` was used
#'   when the action was specified.
#' * `includeProcess(prefix, label = NULL, weights, prob, termStates)`: includes
#'   an external process. External processes are loaded into memory only when
#'   needed, which helps with large models. `prefix` is the external process
#'   prefix. `weights` must be a vector of action weights, and `prob` must
#'   contain triples `(scope, idx, pr)`; see the description of `actionIdx.bin`
#'   below. `termStates` must specify the number of states at the last stage in
#'   the external process. Inside an `includeProcess ... endIncludeProcess`
#'   block, you must specify the father jump actions of the last stage in the
#'   external process. The external process is represented by its first and last
#'   stage together with its jump actions. The function returns, invisibly, the
#'   state ids of the first stage in the external process, which can later be
#'   referenced with scope 3.
#' * `endIncludeProcess()`: ends an `includeProcess` block.
#' * `closeWriter()`: closes the writer. Call this when the model description is
#'   finished.
#'
#' Eight binary files are created:
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
