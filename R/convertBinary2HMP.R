#' Convert a HMDP model stored in binary format to a hmp (xml) file.
#' The function simply parse the binary files and create hmp files using
#' the \link{hmpMDPWriter}.
#'
#' @usage
#' convertBinary2HMP(binNames=c("stateIdx.bin","stateIdxLbl.bin","actionIdx.bin",
#' "actionIdxLbl.bin","actionWeight.bin","actionWeightLbl.bin","transProb.bin")
#' ,out='r_convert.hmp', duration=1)
#'
#' @param binNames A character vector of length 7 giving the names of the binary files storing the model.
#' @param out The name of the hmp file (e.g. mdp.hmp).
#' @param duration Weight number storing the duration.
#' @return NULL (invisible).
#' @author Lars Relund \email{lars@@relund.dk}
#' @note Note all indexes are starting from zero (C/C++ style).
#' @seealso convertHMP2Binary.
#' @examples
#' f = system.file("inst/models", "mdp.xml", package = "MDP")
#' convertHMP2Binary("mdp.hmp")
#' stateIdxDf()
#' actionInfo()
convertBinary2HMP<-function(prefix="", binNames=c("stateIdx.bin","stateIdxLbl.bin","actionIdx.bin",
	"actionIdxLbl.bin","actionWeight.bin","actionWeightLbl.bin","transProb.bin"),
	out=paste(prefix,'converted.hmp',sep=""), duration=1) {

	process<-function(mat) {
		#cat("process\n"); print(mat)
		stages<-length(unique(mat[,2]))
		w$process()
			for (i in 1:stages-1) {
				#print(i); print(nrow(mat[mat[,2]==i+1,]))
				stage(mat[mat[,2]==i,,drop=F], statesNext=nrow(mat[mat[,2]==i+1,]))
			}
		w$endProcess()
	}

	stage<-function(mat, statesNext) {
		#cat("stage\n"); print(mat)
		states<-length(unique(mat[,3]))
		w$stage()
			for (i in 1:states-1) {
				#print(i); print(nrow(mat[mat[,3]==i+1,]))
				state(mat[mat[,3]==i,,drop=F], statesNext=statesNext)
			}
		w$endStage()
	}

	state<-function(mat, statesNext) {
		#cat("state\n"); print(mat)
		#level1<-sum(!is.na(mat[1,2:ncol(mat)])) %/% 3    # level of the first state in mat
		matA<-aIdx[aIdx[,2]==mat[1,1],,drop=F]  # actions to the first state in mat
		if (nrow(mat)!=1) {
			aIdx<-unique(mat[!is.na(mat[,4]),4])   # actions that define child processes
			aCtr<-1
			#print(aIdx)
		}
		w$state(label=sLabels[sLabels[,1]==mat[1,1],2]) # create state in hmp
			for (i in 1:nrow(matA)) {   # scan actions
				#matSA<-mat[2:nrow(mat),]
				#cat("action\n"); print(matA[i,1:6])
				scp<-matA[i,3:ncol(matA)]
				scp<-idx<-scp[!is.na(scp)]
				scp<-scp[1:length(scp)%%2==1]
				idx<-idx[1:length(idx)%%2==0]
				weights<-aW[aW[,1]==matA[i,1],1:wLth+1]
				#print(weights)
				if (any(scp==2)) { # new process
					if (length(scp)>1)
						stop("Only a deterministic transition to sub process allowed for action (aId) ",matA[i,1],"!")
					if (idx[1]!=0)
						stop("Only a deterministic transition to state 0 in sub process allowed for action (aId)",matA[i,1],"!")
					if (any(weights!=0))
						stop("Only zero weights allowed for transition to sub process, action (aId)",matA[i,1],"!")
					w$action(label=aLabels[aLabels[,1]==matA[i,1],2], weights=c(0,0,0), prob=c(2,0,1))
						process(mat[mat[,4]==aIdx[aCtr] &  !is.na(mat[,4]), c(1,5:ncol(mat))])
						aCtr<-aCtr+1
					w$endAction()
				} else {
					pr<-prMat[prMat[,1]==matA[i,1],2:ncol(prMat)]
					pr<-pr[!is.na(pr)]
					pr<-as.numeric(rbind(scp,idx,pr))
					#print(pr); print(statesNext)
					w$action(label=aLabels[aLabels[,1]==matA[i,1],2], weights=weights, prob=pr, statesNext=statesNext)
					w$endAction()
				}
			}
		w$endState()
	}

	ptm <- proc.time()
	sIdx<-stateIdxMat(prefix,binNames[1])
	sLabels<-stateIdxDf(prefix,binNames[1])
	sLabels<-sLabels[,c(1,ncol(sLabels))]
	aIdx<-actionIdxMat(prefix,binNames[3])
	aLabels<-actionIdxDf(prefix,binNames[3])
	aLabels<-aLabels[,c(1,ncol(aLabels))]
	aW<-actionWeightMat(prefix,binNames[5],binNames[6])
	prMat<-transProbMat(prefix,binNames[7])
	wNames<-weightNames(prefix,binNames[6])
	wLth<-length(wNames)
	# level<-sum(!is.na(sIdx[i,2:cols])) %/% 3
	w<-hmpMDPWriter(file=out,desc="HMP file created by converting binary files")
		w$setWeights(wNames, duration)
		process(sIdx)
	w$closeWriter()
	cat("Converted binary files to hmp format.\n")
	print(proc.time() - ptm)
	invisible(NULL)
}
