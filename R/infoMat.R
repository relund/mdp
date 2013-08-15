#' Info about the states in the HMDP model under consideration.
#'
#' @param prefix A character string with the prefix added to til file(s).
#' @param file The HMDP binary file containing the description under consideration.
#'
#' @return A matrix with columns (sId, n0, s0, a0, ...) where
#' sId is the state row id, n0 the index of the stage at level 0, s0 the index
#' of the state and a0 the index of the action. If the HMDP has more
#' than one level columns index (d1, s1, a1, ...) are added.
#'
#' @author Lars Relund \email{lars@@relund.dk}
#'
#' @example tests/infoMat.Rex
#' @export
stateIdxMat<-function(prefix="", file="stateIdx.bin") {
	file<-paste(prefix,file,sep="")
	tmp<-readBin(file, integer(),n=file.info(file)$size/4)
	rows<-length(tmp[tmp==-1])
	cols<-max(rle(tmp!=-1)$length)
	mat<-matrix(NA,nrow=rows,ncol=cols+1)
	idx<-c(0,which(tmp== -1))
	for (i in 1:(length(idx)-1)) mat[i,1:(idx[i+1]-idx[i]-1)+1]<-tmp[(idx[i]+1):(idx[i+1]-1)]
	levels<-cols %/% 3 + 1
	if (levels==1) colnames(mat)<-c("sId",paste(c("n","s"),levels-1,sep=""))
	if (levels>1) colnames(mat)<-c("sId",paste(c("n","s","a"),rep(0:(levels-2),each=3),sep=""),paste(c("n","s"),levels-1,sep=""))
	mat[,1]<-1:nrow(mat)-1
	return(mat)
}


#' Info about the states in the HMDP model under consideration.
#'
#' @param prefix A character string with the prefix added to til file(s).
#' @param file The HMDP binary file containing the description under consideration.
#' @param labels The HMDP binary file containing the labels under consideration.
#'
#' @return A data frame with the same columns as in
#' \code{stateIdxMat} plus another column containing the labels.
#'
#' @author Lars Relund \email{lars@@relund.dk}
#'
#' @example tests/infoMat.Rex
#' @export
stateIdxDf<-function(prefix="", file="stateIdx.bin", labels="stateIdxLbl.bin") {
	labels<-paste(prefix,labels,sep="")
	mat<-stateIdxMat(prefix, file)
	tmp<-readBin(labels, character(),n=file.info(labels)$size)
	tmp<-as.data.frame(matrix(tmp,ncol=2,byrow=TRUE),stringsAsFactors = FALSE)
	colnames(tmp)<-c("sId","label")
	mat<-merge(mat,tmp,all.x=TRUE)
	return(mat)
}


#' Info about the transition probabilities in the HMDP model under consideration.
#'
#' @param prefix A character string with the prefix added to til file(s).
#' @param file The HMDP binary file containing the description under consideration.
#'
#' @return A matrix with columns (aId, ...) where
#' aId is the action row id and ... are the probabilities of the action.
#'
#' @author Lars Relund \email{lars@@relund.dk}
#'
#' @example tests/infoMat.Rex
#' @export
transProbMat<-function(prefix="", file="transProb.bin") {
	file<-paste(prefix,file,sep="")
	tmp<-readBin(file, numeric(),n=file.info(file)$size/8)
	rows<-length(tmp[tmp==-1])
	cols<-max(rle(tmp!=-1)$length)
	mat<-matrix(NA,nrow=rows,ncol=cols+1)
	idx<-c(0,which(tmp== -1))
	for (i in 1:(length(idx)-1)) mat[i,1:(idx[i+1]-idx[i]-1)+1]<-tmp[(idx[i]+1):(idx[i+1]-1)]
	colnames(mat)<-c("aId",paste("pr",1:(ncol(mat)-1)-1,sep=""))
	mat[,1]<-1:nrow(mat)-1
	return(mat)
}


#' Info about the actions in the HMDP model under consideration.
#'
#' @param prefix A character string with the prefix added to til file(s).
#' @param file The HMDP binary file containing the description under consideration.
#'
#' @return A matrix with columns (aId, ...) where
#' aId is the action row id and ... are alternating pairs (scp, idx), one for each
#' possible transition where scp is the scope that can be 4 values:
#' 2 - A transition to a child process (stage zero in the child process), 1 - A transition
#' to next stage in the current process, 0 - A transition to the next stage in the father
#' process. the idx in the pair denote the index of the state at the stage considered.
#' Finally, if scope = 3 then a transition to the state with sId = idx is considered.
#'
#' @author Lars Relund \email{lars@@relund.dk}
#'
#' @example tests/infoMat.Rex
#' @export
actionIdxMat<-function(prefix="", file="actionIdx.bin") {
	file<-paste(prefix,file,sep="")
	tmp<-readBin(file, integer(),n=file.info(file)$size/4)
	rows<-length(tmp[tmp==-1])
	cols<-max(rle(tmp!=-1)$length)
	mat<-matrix(NA,nrow=rows,ncol=cols+1)
	idx<-c(0,which(tmp== -1))
	for (i in 1:(length(idx)-1)) mat[i,1:(idx[i+1]-idx[i]-1)+1]<-tmp[(idx[i]+1):(idx[i+1]-1)]
	colnames(mat)<-c("aId","sId",paste(c("scp","idx"),rep(1:((ncol(mat)-2)/2)-1,each=2),sep=""))
	mat[,1]<-1:nrow(mat)-1
	return(mat)
}


#' Info about the actions in the HMDP model under consideration.
#'
#' @param prefix A character string with the prefix added to til file(s).
#' @param file The HMDP binary file containing the description under consideration.
#' @param labels The HMDP binary file containing the labels under consideration.
#'
#' @return A data frame with the same columns as in
#' \code{actionIdxMat} plus another column containing the labels.
#'
#' @author Lars Relund \email{lars@@relund.dk}
#'
#' @example tests/infoMat.Rex
#' @export
actionIdxDf<-function(prefix="", file="actionIdx.bin", labels="actionIdxLbl.bin") {
	labels<-paste(prefix,labels,sep="")
	mat<-actionIdxMat(prefix, file)
	tmp<-readBin(labels, character(),n=file.info(labels)$size)
	tmp<-as.data.frame(matrix(tmp,ncol=2,byrow=TRUE),stringsAsFactors = FALSE)
	colnames(tmp)<-c("aId","label")
	tmp$aId<-as.numeric(tmp$aId)
	mat<-merge(mat,tmp,all.x=TRUE)
	colnames(mat)<-c("aId","sId",paste(c("scp","idx"),rep(1:((ncol(mat)-2)/2)-1,each=2),sep=""),"label")
	return(mat)
}


#' Info about the actions in the HMDP model under consideration.
#'
#' @param prefix A character string with the prefix added to til file(s).
#' @param file The HMDP binary file containing the description under consideration.
#' @param labels The HMDP binary file containing the labels under consideration.
#' @param weightFile The HMDP binary file containing the action costs.
#' @param transPrFile The HMDP binary file containing the transition probabilities.
#'
#' @return A matrix with columns from \code{actionIdxMat},
#' \code{actionCostMat} and \code{transProbMat} if labels is NULL. If labels
#' not are NULL then a data frame are returned with a label column too.
#'
#' @author Lars Relund \email{lars@@relund.dk}
#'
#' @example tests/infoMat.Rex
#' @export
actionInfo<-function(prefix="", file="actionIdx.bin" , weightFile="actionWeight.bin", transPrFile="transProb.bin", labels="actionIdxLbl.bin") {
	labels<-paste(prefix,labels,sep="")
	mat<-actionIdxMat(prefix, file)
	mat1<-actionWeightMat(prefix, weightFile)
	mat<-merge(mat,mat1,all.x=TRUE)
	mat2<-transProbMat(prefix, transPrFile)
	mat<-merge(mat,mat2,all.x=TRUE)
	i<-(ncol(mat)-2-ncol(mat1)+1)/3     # number of idx used for (scp, idx, pr) triple
	mat<-mat[,c("aId","sId",colnames(mat1[,2:ncol(mat1),drop=FALSE]),paste(c("scp","idx","pr"),rep(1:i-1,each=3),sep=""))]
	if (!is.null(labels)) {
		tmp<-readBin(labels, character(),n=file.info(labels)$size)
		tmp<-as.data.frame(matrix(tmp,ncol=2,byrow=TRUE))
		colnames(tmp)<-c("aId","label")
		mat<-merge(mat,tmp,all.x=TRUE)
		return(mat)
	}
	mat<-as.matrix(mat)
	return(mat)
}


#' Info about the weights of the actions in the HMDP model under consideration.
#'
#' @param prefix A character string with the prefix added to til file(s).
#' @param file The HMDP binary file containing the description under consideration.
#' @param labels The HMDP binary file containing the labels under consideration.
#'
#' @return A matrix with columns (aId, ...) where
#' aId is the action row id and ... are the weights of the action.
#'
#' @author Lars Relund \email{lars@@relund.dk}
#'
#' @example tests/infoMat.Rex
#' @export
actionWeightMat<-function(prefix="", file="actionWeight.bin",labels="actionWeightLbl.bin") {
	file<-paste(prefix,file,sep="")
	labels<-paste(prefix,labels,sep="")
	tmp<-readBin(file, numeric(),n=file.info(file)$size/8)
	colNames<-readBin(labels, character(),n=file.info(labels)$size)
	cols<-length(colNames)
	rows<-length(tmp)/cols
	mat<-matrix(NA,nrow=rows,ncol=cols+1)
	for (i in 1:rows) mat[i,1:cols+1]<-tmp[(cols*(i-1)+1):(cols*i)]
	#colnames(mat)<-c("aId",paste("w",1:(ncol(mat)-1)-1,sep=""))
	colnames(mat)<-c("aId",colNames)
	mat[,1]<-1:nrow(mat)-1
	return(mat)
}


#' Names of weights used in actions.
#'
#' @param prefix A character string with the prefix added to the binary file names.
#' @param labels The HMDP binary file containing the weight labels.
#' @author Lars Relund \email{lars@@relund.dk}
#' @return Vector of weight names.
#' @export
weightNames<-function(prefix="", labels="actionWeightLbl.bin") {
	labels<-paste(prefix,labels,sep="")
	colNames<-readBin(labels, character(),n=file.info(labels)$size)
	return(colNames)
}


#' Information about a state and its corresponding actions
#'
#' @param mdp The MDP loaded using \link{loadMDP}.
#' @param idS The id of the state(s) considered.
#' @param idxS A string containing the index of the state(s) (e.g. "n0,s0,a0,n1,s1"). Parameter \code{idS} are ignored if not NULL.
#' @param idxN A string containing the index of the stage(s) (e.g. "n0,s0,a0,n1"). Parameter \code{idS} and \code{idxS} are ignored if not NULL.
#' @return A list of states containing actions.
#' @author Lars Relund \email{lars@@relund.dk}
#' @example tests/machine.Rex
#' @export
info<-function(mdp, idS=NULL, idxS=NULL, idxN=NULL) {
	if (!is.null(idxS)) idS<-getIdS(mdp, idxS)
	if (!is.null(idxN)) idS<-getIdSStages(mdp, idxN)
	l<-vector("list", length(idS))
	lapply(l,function(x) x<-list(id=NULL,idx=NULL,label=NULL,actions=NULL))
	idxStr<-getStrIdxS(mdp,idS)
	label<-getLabel(mdp,idS)
	for (i in 1:length(l)) {
		l[[i]]$id <- idS[i]
		l[[i]]$idx <- idxStr[i]
		l[[i]]$label <- label[i]
		l[[i]]$actions <- .Call("MDP_GetActionInfo", mdp$ptr, as.integer(idS[i]), PACKAGE="MDP")
	}
	return(l)
}
