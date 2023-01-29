#' Plot parts of the state expanded hypergraph (experimental).
#' 
#' The plot is created based on a grid `(rows, cols)`. Each grid point is numbered from bottom to
#' top and left to right (starting from 1), i.e. given grid point with coordinates `(r, c)` (where
#' `(1,1)` is the top left corner and `(rows, cols)` is the bottom right corner) the grid id is `(c
#' - 1) * rows + r`. You must assign a node to the hypergraph to a grid point (see below).
#' 
#' @param hgf A list with the hypergraph containing two data frames, normally found using
#'   [getHypergraph()]. The data frame `nodes` must have columns: `sId` (state id), `gId` (grid id)
#'   and `label` (node label). The data frame `hyperarcs` must have columns `sId` (head node),
#'   `trans<n>` (tail nodes), `aIdx` (action index), `label` (action label), `lwd` (hyperarc line
#'   width), `lty` (hyperarc line type) and `col` (hyperarc color).
#' @param gridDim A 2-dim vector (rows, cols) representing the size of the grid.
#' @param showGrid If true show the grid points (good for debugging).
#' @param radx Horizontal radius of the box.
#' @param rady Vertical radius of the box.
#' @param cex Relative size of text.
#' @param marX Horizontal margin.
#' @param marY Vertical margin.
#' @param ... Graphical parameters passed to `textempty`. 
#'   
#' @return No return value (NULL invisible), called for side effects (plotting).
#' @seealso [getHypergraph()] and [plot.HMDP()].
#' @example inst/examples/plot-ex.R
#' @import diagram
#' @export
plotHypergraph <-
   function(hgf,
            gridDim,
            showGrid = FALSE,
            radx = 0.03,
            rady = 0.05,
            cex = 1,
            marX = 0.035,
            marY = 0.15,
            ...) {
   # internal functions
   gMap<-function(sId) return(hgf$nodes$gId[hgf$nodes$sId %in% sId])		# return gId given sId
   sMap<-function(gId) return(hgf$nodes$sId[hgf$nodes$gId %in% gId])		# return sId given gId
   pos <- coordinates(rep(gridDim[2], gridDim[1]), hor = TRUE)  # coordinates of each point in the grid
   
   # reposition
   posN <- pos
   for (i in 1:nrow(pos)) {
      c <- (i-1) %% gridDim[2] + 1
      r <- (i-1) %/% gridDim[2] + 1
      id <- (c - 1) * gridDim[1] + r
      # cat(i, r, c, id, "\n", sep= " ")
      posN[id, ] <- pos[i, ]
   }
   pos <- posN

   openplotmat(xlim=c(min(pos[,1])-marX,max(pos[,1])+marX), 
               ylim=c(0-marY,max(pos[,2])+marY) )  #main = "State expanded hypergraph"
   
   # plot time index
   # if (addTime) {
   #    posT <- matrix(c(unique(pos[,1]), rep(0, gridDim[2])), ncol = 2)  # coordinates for time index
   #    colnames(posT) <- colnames(pos)
   #    for (i in 1:gridDim[2] - 1) textempty(posT[i+1, ], lab = parse(text = str_c("italic(t == ", i, ")")), cex=cex)
   # }
   

   # plot actions
   if (!is.null(hgf$hyperarcs)) {
      tailCols <- hgf$hyperarcs %>% dplyr::select(dplyr::starts_with("trans"))
      for (i in 1:nrow(hgf$hyperarcs)) {
         tails <- tailCols[i, ] %>% as.numeric()
         #cat("i:",i,"highlight:",hgf$hyperarcs$highlight[i],"\n")
         # if (hgf$hyperarcs$highlight[i]) splitarrow(from = pos[gMap(tails), ], to = pos[gMap(hgf$hyperarcs[i,1]),], arr.side = 2, arr.pos = 0.1, lwd=2, lty=1,
         #                                      arr.type="curved", arr.lwd = 0.5, arr.length = 0.1, arr.width = 0.08, lcol="gray")
         pt <-
            splitarrow(
               from = pos[gMap(tails),],
               to = pos[gMap(hgf$hyperarcs[i, 1]), ],
               arr.side = 2,
               arr.pos = 0.1,
               lwd = hgf$hyperarcs$lwd[i],
               lty = hgf$hyperarcs$lty[i],
               arr.type = "curved",
               arr.lwd = 0.5,
               arr.length = 0.1,
               arr.width = 0.08,
               lcol = hgf$hyperarcs$col[i]
            )
         textempty(
            pt,
            lab = hgf$hyperarcs$label[i],
            adj = c(-0.1, 0.1),
            cex = cex,
            ...
         )
      }
   }	   
   
   # plot states
   if (!is.null(hgf$nodes)) {
      for (i in 1:nrow(hgf$nodes)) { 
         textellipse(pos[hgf$nodes$gId[i], ], lab = hgf$nodes$label[i], radx = radx, rady=rady, shadow.size = 0, lwd=0.5, cex=cex) 
      }
   }
   
   # visual view of the point numbers (for figuring out how to map stateId to gridId)
   if (showGrid) {
      for (i in 1:dim(pos)[1]) textrect(pos[i, ], lab = i, radx = 0.0, cex=cex)
   }
   return(invisible(NULL))
}



#' Plot the state-expanded hypergraph of the MDP. 
#'
#' @param x The MDP model. 
#' @param ... Arguments passed to [plotHypergraph()]. Moreover, you may use 
#'    * `hyperarcColor`: A string. If empty string no colors are used (default). If `label` then 
#'       use different colors based on the hyperarc/action labels. If `policy` then use highlight 
#'       the current policy.
#'    * `nodeLabel`: A string. If empty string, then display node labels (default). If `sId` then 
#'       display the state ids. If `sId:label` then display the state ids together with the label. 
#'       If `sIdx:label` then display the state index and the label. If `weight` then display the 
#'       node weight. 
#'    * `hyperarcShow` A string. If `all` then show all hyperarcs (default). If `policy` then only 
#'       show the current policy.
#'
#' @return No return value (NULL invisible), called for side effects (plotting).
#' @seealso [getHypergraph()] and [plotHypergraph()].
#' @example inst/examples/plot-ex.R
#' @importFrom rlang .data
#' @export
plot.HMDP <- function(x, ...) {
   args <- list(...)
   # set defaults
   hyperarcColor <- ifelse(is.null(args$hyperarcColor), "", args$hyperarcColor)
   nodeLabel <- ifelse(is.null(args$nodeLabel), "", args$nodeLabel)
   hyperarcShow <- ifelse(is.null(args$hyperarcShow), "all", args$hyperarcShow) 
   mdp <- x
   # prepare args for plotHypergraph
   args$hyperarcColor <- NULL
   args$nodeLabel <- NULL
   args$hyperarcShow <- NULL
   
   if (mdp$levels != 1) {
      message("Cannot plot a hierarchical MDP without specifying the placement of\n", 
              "states and actions (use `plotHypergraph` instead).")
      return(invisible(NULL))
   }
   if (mdp$timeHorizon < Inf) {
      statesCt <- purrr::map_dbl(paste0(1:mdp$timeHorizon - 1), mdp$ptr$getStateSizeStage)
      gridDim <- c(max(statesCt), mdp$timeHorizon)
   } else {
      gridDim <- c(mdp$founderStatesLast, 2)
   }
   hgf <- getHypergraph(mdp)
   hgf$nodes <- hgf$nodes %>% 
      tidyr::separate(.data$stateStr, into = c("c", "r"), remove = FALSE, convert = TRUE) %>% 
      dplyr::mutate(c = .data$c + 1, 
                    r = .data$r + 1, 
                    gId = (.data$c - 1) * max(.data$r) + .data$r) 
   if (hyperarcShow == "policy") {
      hgf$hyperarcs <- hgf$hyperarcs %>% 
         dplyr::left_join(getPolicy(mdp), by = c("sId", "aIdx")) %>% 
         dplyr::filter(!is.na(.data$actionLabel))
   }
   if (hyperarcColor == "label") {
      colDF <- tibble::tibble(label = unique(hgf$hyperarcs$label), 
                              col = grDevices::rainbow(length(unique(hgf$hyperarcs$label))))
      hgf$hyperarcs <- hgf$hyperarcs %>% 
         dplyr::select(-col) %>% 
         dplyr::left_join(colDF, by = "label") 
   }
   if (hyperarcColor == "policy") {
      hgf$hyperarcs <- hgf$hyperarcs %>% 
         dplyr::left_join(getPolicy(mdp)) %>% 
         dplyr::mutate(col = ifelse(is.na(.data$actionLabel), "black", "blue"))
   }
   if (nodeLabel == "sId") {
      hgf$nodes <- hgf$nodes %>% 
         dplyr::mutate(label = .data$sId)
   }
   if (nodeLabel == "sId:label") {
      hgf$nodes <- hgf$nodes %>% 
         dplyr::mutate(label = paste0(.data$sId, ": ", .data$label))
   }
   if (nodeLabel == "sIdx:label") {
      hgf$nodes <- hgf$nodes %>% 
         dplyr::mutate(label = paste0(.data$r - 1, ": ", .data$label))
   }
   if (nodeLabel == "weight") {
      hgf$nodes <- hgf$nodes %>%
         dplyr::left_join(getPolicy(mdp), by = "sId") %>% 
         dplyr::mutate(label = .data$weight)
   }
   do.call(plotHypergraph, args = c(list(hgf, gridDim), args))
   return(invisible(NULL))
}


#' Return the (parts of) state-expanded hypergraph
#' 
#' The function is useful together with [plotHypergraph()].
#'
#' @param mdp The MDP loaded using [loadMDP()].
#' @param ... Arguments passed to [getInfo()].
#'
#' @return A list representing the hypergraph with two elements: A tibble `nodes` and a 
#'    tibble `hyperarcs`. 
#' @seealso [plotHypergraph()] and [plot.HMDP()].
#' @importFrom rlang .data
#' @export
#' @example inst/examples/plot-ex.R
getHypergraph <- function(mdp, ...) {
   dat <- getInfo(mdp, withList = FALSE, ...)
   actions <- dat$df %>% 
      tidyr::unnest_longer("actions") %>% # convert actions (one row for each action)
      tidyr::unnest_wider("actions", names_repair = tidyr::tidyr_legacy) %>% 
      dplyr::filter(!is.na(.data$aIdx)) %>% 
      tidyr::unnest_wider(.data$trans, names_sep = "") %>% 
      tidyr::unnest_wider(.data$pr, names_sep = "") %>% 
      dplyr::rename(labelAction = .data$label1) %>% 
      dplyr::mutate(lwd = 1, lty = 1, col = "black") %>% 
      dplyr::select(-.data$stateStr, -.data$label) %>% 
      dplyr::rename(label = .data$labelAction)
   states <- dat$df %>% 
      dplyr::mutate(gId = NA_integer_) %>% 
      dplyr::select(.data$sId, .data$stateStr, .data$label, .data$gId)
   if (mdp$timeHorizon == Inf) {
      states$label[1:mdp$founderStatesLast] <- states$label[(nrow(states) - mdp$founderStatesLast + 1):nrow(states)]
   }
   return(list(nodes = states, hyperarcs = actions))
}








# plotHypergraphOld <-
#    function(gridDim,
#             states = NULL,
#             actions = NULL,
#             showGrid = FALSE,
#             radx = 0.02,
#             rady = 0.03,
#             cex = 1,
#             marX = 0.03,
#             marY = 0.05,
#             ...) {
#    # internal functions
#    gMap<-function(sId) return(states$gId[states$sId %in% sId])		# return gId given sId
#    sMap<-function(gId) return(states$sId[states$gId %in% gId])		# return sId given gId
#    
#    # fontf<-"Times"
#    # width=8 # A4 is 8.25 x 11.75 inch
#    # height=5	
#    # if (!is.null(fileN)) pdf(file=fileN, width=width, height=height, family=fontf)
#    
#    pos <- coordinates(rep(gridDim[2], gridDim[1]))  # coordinates of each point in the grid
#    openplotmat(xlim=c(min(pos[,1])-marX,max(pos[,1])+marX), 
#                ylim=c(min(pos[,2])-marY,max(pos[,2])+marY) )  #main = "State expanded hypergraph"
#    
#    # plot actions
#    if (!is.null(actions)) {
#       tailCols<-3:grep('label',colnames(actions))-1
#       for (i in 1:(dim(actions)[1])) {
#          tails<-actions[i,tailCols]
#          #cat("i:",i,"highlight:",actions$highlight[i],"\n")
#          if (actions$highlight[i]) splitarrow(from = pos[gMap(tails), ], to = pos[gMap(actions[i,1]),], arr.side = 2, arr.pos = 0.1, lwd=2, lty=1, 
#                                               arr.type="curved", arr.lwd = 0.5, arr.length = 0.1, arr.width = 0.08, lcol="gray")
#          pt<-splitarrow(from = pos[gMap(tails), ], to = pos[gMap(actions[i,1]),], arr.side = 2, arr.pos = 0.1, lwd=actions$lwd[i], lty=actions$lty[i], 
#                         arr.type="curved", arr.lwd = 0.5, arr.length = 0.1, arr.width = 0.08, lcol=actions$col[i])
#          textempty(pt,lab=actions$label[i],adj=c(-0.1,0.1), cex=cex, ...)
#       }
#    }	
#    
#    # visual view of the point numbers (for figuring out how to map stateId to gridId)
#    if (showGrid) {
#       for (i in 1:dim(pos)[1]) textrect(pos[i, ], lab = i, radx = 0.0, cex=cex)
#    }
#    
#    # plot states
#    if (!is.null(states)) {
#       for (i in 1:length(states$gId)) { 
#          textellipse(pos[states$gId[i], ], lab = states$label[i], radx = radx, rady=rady, shadow.size = 0, lwd=0.5, cex=cex) 
#       }
#    }
#    return(invisible(NULL))
#    }
