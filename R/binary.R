## functions related to the binary files


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
binInfoStates <-
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
binInfoActions<-function(prefix="", labels = TRUE, fileA="actionIdx.bin",
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

