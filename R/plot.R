#' Plot parts of the state expanded hypergraph (experimental).
#' 
#' A plot is created based on a grid. Each grid point is numbered from left to right and topdown,
#' i.e. given grid coordinates (row,col) (where (1,1) is the top left corner) the grid id is
#' (row-1)*cols+col. You must yourself assign a state to a grid point using the states data frame
#' (see below).
#' 
#' @param gridDim A 2-dim vector (rows,cols) representing the size of the grid.
#' @param states A data frame containing 3 columns: sId = state id, gId = grid id and label = text 
#'   to be plotted.
#' @param actions A data frame with a columns in the following order for each action of type (head, tail1, 
#'   tail2,...,label,lwd,lty,col,highlight), since number of tails may differ NAs may appear. Each 
#'   number (head and tails) must correspond to a state id. Column label contains labels = text to 
#'   be plotted (important must appear after the last tail column). Next columns is the line width, 
#'   line type and line color. Column highlight contains boolean which is true if highlight the 
#'   hyperarc (useful if want to show the policy).
#' @param showGrid If true show the grid points (good for debugging).
#' @param fileN If specified the plot will be saved as a pdf file.
#' @param devOff If false do not make a dev.off(), i.e. you can add more graphic to the file.
#' @param ... Graphical parameters e.g. \code{cex=0.5} to control text size. 
#'   
#' @return NULL
#'   
#' @author Lars Relund \email{lars@@relund.dk}
#' @export
plotHypergraph<-function(gridDim,states=NULL,actions=NULL,showGrid=FALSE,fileN=NULL,devOff=TRUE, 
                         radx = 0.02, rady=0.03, cex=1, marX=0.03, marY=0.05, ...){
   library(diagram)
   # internal functions
   gMap<-function(sId) return(states$gId[states$sId %in% sId])		# return gId given sId
   sMap<-function(gId) return(states$sId[states$gId %in% gId])		# return sId given gId
   
   fontf<-"Times"
   width=8 # A4 is 8.25 x 11.75 inch
   height=5	
   if (!is.null(fileN)) pdf(file=fileN, width=width, height=height, family=fontf)
   
   pos <- coordinates(rep(gridDim[2], gridDim[1]))  # coordinates of each point in the grid
   openplotmat(xlim=c(min(pos[,1])-marX,max(pos[,1])+marX), 
               ylim=c(min(pos[,2])-marY,max(pos[,2])+marY) )  #main = "State expanded hypergraph"
   
   # plot actions
   if (!is.null(actions)) {
      tailCols<-3:grep('label',colnames(actions))-1
      for (i in 1:(dim(actionDF)[1])) {
         tails<-actions[i,tailCols]
         if (actions$highlight[i]) splitarrow(from = pos[gMap(tails), ], to = pos[gMap(actions[i,1]),], arr.side = 2, arr.pos = 0.1, lwd=2, lty=1, 
                                              arr.type="curved", arr.lwd = 0.5, arr.length = 0.1, arr.width = 0.08, lcol="gray")
         pt<-splitarrow(from = pos[gMap(tails), ], to = pos[gMap(actions[i,1]),], arr.side = 2, arr.pos = 0.1, lwd=actions$lwd[i], lty=actions$lty[i], 
                        arr.type="curved", arr.lwd = 0.5, arr.length = 0.1, arr.width = 0.08, lcol=actions$col[i])
         textempty(pt,lab=actions$label[i],adj=c(-0.1,0.1), cex=cex, ...)
      }
   }	
   
   # visual view of the point numbers (for figuring out how to map stateId to gridId)
   if (showGrid) {
      for (i in 1:dim(pos)[1]) textrect(pos[i, ], lab = i, radx = 0.0, cex=cex)
   }
   
   # plot states
   if (!is.null(states)) {
      for (i in 1:length(states$gId)) { 
         textellipse(pos[states$gId[i], ], lab = states$label[i], radx = radx, rady=rady, shadow.size = 0, lwd=0.5, cex=cex) 
      }
   }
   if (devOff && !is.null(fileN)) dev.off()
}
