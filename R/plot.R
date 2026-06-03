#' Plot parts of the state expanded hypergraph.
#' 
#' The plot is created based on a grid `(rows, cols)`. Each grid point is numbered from bottom to
#' top and left to right (starting from 1), i.e. given grid point with coordinates `(r, c)` (where
#' `(1,1)` is the top left corner and `(rows, cols)` is the bottom right corner) the grid id is `(c
#' - 1) * rows + r`. You must assign a node to the hypergraph to a grid point (see below).
#' 
#' @param hgf A list with the hypergraph containing two data frames, normally found using
#'   [getHypergraph()]. The data frame `nodes` must have columns: `sId` (state id), `gId` (grid id)
#'   and `label` (node label). The data frame `hyperarcs` must have columns `sId` (head node),
#'   `trans` (a list-column of tail node ids), `pr` (a list-column of transition
#'   probabilities), `actionWeights` (a list-column of action weights),
#'   `transWeights` (a list-column of transition-by-weight matrices), `aIdx`
#'   (action index), `label` (action label), `lwd` (hyperarc line width), `lty`
#'   (hyperarc line type) and `col` (hyperarc color).
#' @param gridDim A 2-dim vector (rows, cols) representing the size of the grid.
#' @param showGrid If true show the grid points (good for debugging).
#' @param radx Horizontal radius of the box.
#' @param rady Vertical radius of the box.
#' @param cex Relative size of text.
#' @param marX Horizontal margin.
#' @param marY Vertical margin.
#' @param drawBorder If `TRUE`, draw a border around the plot region and report the
#'   outside and inside padding (good for debugging).
#' @param actionOffset Distance used to separate actions with the same
#'   start and trans states. Set to `0` to draw overlapping actions.
#' @param transLabels Transition-label mode. `"none"` draws no transition labels
#'   (the default); `"custom"` draws values from an optional `transLabels`
#'   list-column in `hgf$hyperarcs`; otherwise use a `|`-separated
#'   combination of `"label"`, `"sId"`, `"prob"`, and `"weights"`, for example
#'   `"prob|weights"`. The older `"state"` spelling is treated as `"label"`.
#' @param transLabelCex Relative size of transition-label text.
#' @param transLabelAdj Position adjustment passed to `textempty()` for transition
#'   labels, drawn at the middle of each split-to-transition branch.
#' @param stateLabel What to plot in states. `"custom"` uses a `stateLabel`
#'   column in `hgf$nodes`; otherwise use a `|`-separated combination of
#'   `"label"` (state label, default), `"sId"` (state id), `"sIdx"`
#'   (stage-based state index), and `"weight"` (optimal weight of the state).
#' @param actionLabel What to plot near the split. One of `"none"`, `"custom"`
#'   (uses an `actionLabel` column in `hgf$hyperarcs`), or a `|`-separated
#'   combination of `"label"` (action label, default) and `"aIdx"`.
#' @param actionWLabel What to plot from the start state to the split. One of
#'   `"none"` (default), `"weight"`, or `"custom"` (uses an `actionWLabel`
#'   column in `hgf$hyperarcs`).
#' @param actionColor Action coloring scheme. Default `""` uses black lines. `"label"` uses different colors based on the action labels. `"policy"` highlights the current policy.
#' @param actionsVisible Action visibility mode. `"all"` (default) shows all actions. `"policy"` only shows actions in the current policy.
#' @param connectedTo Optional vector of state ids. If supplied, plot only states
#'   reachable from these states by following visible hyperarcs forward,
#'   and trim hyperarcs and transition-level data to the remaining states.
#' @param recalcGrid If `TRUE` and `connectedTo` is supplied, recalculate the
#'   grid for the visible nodes. Nodes keep their original columns, but visible
#'   nodes within each column are placed consecutively from the top and the
#'   number of grid rows is reduced to the maximum number of visible nodes in
#'   any column.
#' @param mdp The MDP model. Required if `stateLabel` contains `"weight"`,
#'   `actionColor = "policy"`, or `actionsVisible = "policy"`.
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
            drawBorder = FALSE,
            actionOffset = 0.025,
            transLabels = "none",
            transLabelCex = 0.8 * cex,
            transLabelAdj = c(0.5, -0.6),
            stateLabel = "label",
            actionLabel = "label",
            actionWLabel = "none",
            actionColor = c("", "label", "policy"),
            actionsVisible = c("all", "policy"),
            connectedTo = NULL,
            recalcGrid = FALSE,
            mdp = NULL,
            ...) {
   normalizeLabelArg <- function(x, default, arg) {
      if (missing(x) || is.null(x) || identical(x, "")) return(default)
      if (!is.character(x) || length(x) != 1) {
         stop(paste0(arg, " must be a single character string."), call. = FALSE)
      }
      x
   }
   parseLabelSpec <- function(x, allowed, arg, special = character(0), aliases = character(0)) {
      if (x %in% special) return(x)
      tokens <- trimws(strsplit(x, "|", fixed = TRUE)[[1]])
      tokens <- tokens[tokens != ""]
      tokens <- dplyr::recode(tokens, !!!as.list(aliases), .default = tokens)
      invalid <- setdiff(tokens, allowed)
      if (length(tokens) == 0 || length(invalid) > 0) {
         stop(
            paste0(arg, " must use ", paste(c(special, allowed), collapse = ", "), "."),
            call. = FALSE
         )
      }
      tokens
   }
   collapseLabelParts <- function(parts) {
      parts <- as.character(parts)
      parts <- parts[!is.na(parts) & parts != ""]
      if (length(parts) == 0) return(NA_character_)
      paste(parts, collapse = " | ")
   }
   formatNumericLabel <- function(x) {
      as.character(round(as.numeric(x), 2))
   }
   formatWeightVector <- function(w) {
      w <- w[!is.na(w)]
      if (length(w) == 0) {
         NA_character_
      } else if (length(w) == 1) {
         formatNumericLabel(w)
      } else {
         paste0("(", paste(formatNumericLabel(w), collapse = ", "), ")")
      }
   }

   transLabels <- normalizeLabelArg(transLabels, "none", "transLabels")
   stateLabel <- normalizeLabelArg(stateLabel, "label", "stateLabel")
   actionLabel <- normalizeLabelArg(actionLabel, "label", "actionLabel")
   actionWLabel <- normalizeLabelArg(actionWLabel, "none", "actionWLabel")
   transLabelSpec <- parseLabelSpec(
      transLabels,
      c("label", "sId", "prob", "weights"),
      "transLabels",
      special = c("none", "custom"),
      aliases = c(state = "label")
   )
   stateLabelSpec <- parseLabelSpec(
      stateLabel,
      c("label", "sId", "sIdx", "weight"),
      "stateLabel",
      special = "custom"
   )
   actionLabelSpec <- parseLabelSpec(
      actionLabel,
      c("label", "aIdx"),
      "actionLabel",
      special = c("none", "custom")
   )
   actionWLabelSpec <- parseLabelSpec(
      actionWLabel,
      "weight",
      "actionWLabel",
      special = c("none", "custom")
   )
   actionColor <- match.arg(actionColor)
   actionsVisible <- match.arg(actionsVisible)

   # Apply actionsVisible and actionColor logic to hgf$hyperarcs
   if (!is.null(hgf$hyperarcs)) {
      if (actionsVisible == "policy") {
         if (is.null(mdp)) {
            stop("mdp model must be provided to plotHypergraph when actionsVisible = \"policy\".", call. = FALSE)
         }
         policy_arcs <- getPolicy(mdp) %>% dplyr::select("sId", "aIdx") %>% dplyr::mutate(is_policy = TRUE)
         hgf$hyperarcs <- hgf$hyperarcs %>% 
            dplyr::inner_join(policy_arcs, by = c("sId", "aIdx"))
      }
      if (actionColor == "label") {
         colDF <- tibble::tibble(label = unique(hgf$hyperarcs$label), 
                                 col = grDevices::rainbow(length(unique(hgf$hyperarcs$label))))
         hgf$hyperarcs <- hgf$hyperarcs %>% 
            dplyr::select(-col) %>% 
            dplyr::left_join(colDF, by = "label") 
      }
      if (actionColor == "policy") {
         if (is.null(mdp)) {
            stop("mdp model must be provided to plotHypergraph when actionColor = \"policy\".", call. = FALSE)
         }
         policy_arcs <- getPolicy(mdp) %>% dplyr::select("sId", "aIdx") %>% dplyr::mutate(is_policy = TRUE)
         hgf$hyperarcs <- hgf$hyperarcs %>% 
            dplyr::left_join(policy_arcs, by = c("sId", "aIdx")) %>% 
            dplyr::mutate(col = ifelse(is.na(.data$is_policy), "black", "blue")) %>%
            dplyr::select(-"is_policy")
      }
   }

   filterConnectedHypergraph <- function(hgf, connectedTo) {
      if (is.null(connectedTo)) return(hgf)
      if (!is.numeric(connectedTo)) {
         stop("connectedTo must be a numeric vector of state ids.", call. = FALSE)
      }
      connectedTo <- unique(stats::na.omit(as.numeric(connectedTo)))
      if (length(connectedTo) == 0) return(hgf)
      if (is.null(hgf$nodes) || !"sId" %in% names(hgf$nodes)) {
         stop("connectedTo requires hgf$nodes to contain an sId column.", call. = FALSE)
      }
      missingSId <- setdiff(connectedTo, hgf$nodes$sId)
      if (length(missingSId) > 0) {
         stop(
            paste0("connectedTo contains sId values not present in hgf$nodes: ", paste(missingSId, collapse = ", ")),
            call. = FALSE
         )
      }
      if (is.null(hgf$hyperarcs) || nrow(hgf$hyperarcs) == 0) {
         hgf$nodes <- hgf$nodes[hgf$nodes$sId %in% connectedTo, , drop = FALSE]
         return(hgf)
      }
      if (!"trans" %in% names(hgf$hyperarcs)) {
         stop("connectedTo requires hgf$hyperarcs to contain a trans list-column.", call. = FALSE)
      }

      forwardEdges <- lapply(hgf$nodes$sId, function(x) numeric(0))
      names(forwardEdges) <- as.character(hgf$nodes$sId)
      for (i in seq_len(nrow(hgf$hyperarcs))) {
         trans <- as.numeric(hgf$hyperarcs$trans[[i]])
         trans <- trans[!is.na(trans)]
         key <- as.character(hgf$hyperarcs$sId[i])
         if (!is.null(forwardEdges[[key]])) {
            forwardEdges[[key]] <- unique(c(forwardEdges[[key]], trans))
         }
      }

      reachable <- connectedTo
      queue <- connectedTo
      while (length(queue) > 0) {
         current <- queue[1]
         queue <- queue[-1]
         nextStates <- forwardEdges[[as.character(current)]]
         nextStates <- nextStates[nextStates %in% hgf$nodes$sId]
         nextStates <- nextStates[!nextStates %in% reachable]
         if (length(nextStates) > 0) {
            reachable <- c(reachable, nextStates)
            queue <- c(queue, nextStates)
         }
      }
      reachable <- unique(reachable)

      keepArc <- logical(nrow(hgf$hyperarcs))
      transKeep <- vector("list", nrow(hgf$hyperarcs))
      for (i in seq_len(nrow(hgf$hyperarcs))) {
         trans <- as.numeric(hgf$hyperarcs$trans[[i]])
         transKeep[[i]] <- !is.na(trans) & trans %in% reachable
         keepArc[i] <- hgf$hyperarcs$sId[i] %in% reachable && any(transKeep[[i]])
      }
      hgf$hyperarcs <- hgf$hyperarcs[keepArc, , drop = FALSE]
      transKeep <- transKeep[keepArc]

      for (i in seq_len(nrow(hgf$hyperarcs))) {
         keep <- transKeep[[i]]
         hgf$hyperarcs$trans[[i]] <- hgf$hyperarcs$trans[[i]][keep]
         if ("pr" %in% names(hgf$hyperarcs) && length(hgf$hyperarcs$pr[[i]]) == length(keep)) {
            hgf$hyperarcs$pr[[i]] <- hgf$hyperarcs$pr[[i]][keep]
         }
         if ("transLabels" %in% names(hgf$hyperarcs) && length(hgf$hyperarcs$transLabels[[i]]) == length(keep)) {
            hgf$hyperarcs$transLabels[[i]] <- hgf$hyperarcs$transLabels[[i]][keep]
         }
         if ("transWeights" %in% names(hgf$hyperarcs) && is.matrix(hgf$hyperarcs$transWeights[[i]]) &&
             nrow(hgf$hyperarcs$transWeights[[i]]) == length(keep)) {
            hgf$hyperarcs$transWeights[[i]] <- hgf$hyperarcs$transWeights[[i]][keep, , drop = FALSE]
         }
      }
      hgf$nodes <- hgf$nodes[hgf$nodes$sId %in% reachable, , drop = FALSE]
      return(hgf)
   }

   hgf <- filterConnectedHypergraph(hgf, connectedTo)

   recalculateGrid <- function(hgf, gridDim) {
      if (is.null(hgf$nodes) || nrow(hgf$nodes) == 0) return(list(hgf = hgf, gridDim = gridDim))
      if (!"gId" %in% names(hgf$nodes)) {
         stop("recalcGrid requires hgf$nodes to contain a gId column.", call. = FALSE)
      }
      originalCols <- (hgf$nodes$gId - 1) %/% gridDim[1] + 1
      originalRows <- (hgf$nodes$gId - 1) %% gridDim[1] + 1
      if (any(is.na(originalCols) | originalCols < 1 | originalCols > gridDim[2])) {
         stop("recalcGrid requires node gId values to be inside gridDim.", call. = FALSE)
      }
      visibleCounts <- tabulate(originalCols, nbins = gridDim[2])
      rowsNew <- max(visibleCounts)
      if (rowsNew == 0) return(list(hgf = hgf, gridDim = c(1, gridDim[2])))
      newRows <- integer(length(originalRows))
      for (col in seq_len(gridDim[2])) {
         idx <- which(originalCols == col)
         if (length(idx) == 0) next
         idx <- idx[order(originalRows[idx], hgf$nodes$sId[idx])]
         newRows[idx] <- seq_along(idx)
      }
      hgf$nodes$gId <- (originalCols - 1) * rowsNew + newRows
      list(hgf = hgf, gridDim = c(rowsNew, gridDim[2]))
   }

   if (!is.null(connectedTo) && isTRUE(recalcGrid)) {
      gridRecalc <- recalculateGrid(hgf, gridDim)
      hgf <- gridRecalc$hgf
      gridDim <- gridRecalc$gridDim
   }

   # Apply stateLabel logic to hgf$nodes$label
   if (!is.null(hgf$nodes)) {
      if (identical(stateLabelSpec, "custom")) {
         if (!"stateLabel" %in% names(hgf$nodes)) {
            stop(
               "stateLabel = \"custom\" requires a stateLabel column in hgf$nodes.",
               call. = FALSE
            )
         }
         hgf$nodes$label <- as.character(hgf$nodes$stateLabel)
      } else {
         stateLabels <- as.character(hgf$nodes$label)
         stateSIdx <- NULL
         stateWeights <- NULL
         if ("sIdx" %in% stateLabelSpec) {
            if (!"stateStr" %in% names(hgf$nodes)) {
               stop("stateLabel containing \"sIdx\" requires a stateStr column in hgf$nodes.", call. = FALSE)
            }
            parts <- strsplit(hgf$nodes$stateStr, ",")
            stateSIdx <- vapply(parts, function(x) {
               if (length(x) > 0) x[length(x)] else ""
            }, character(1))
         }
         if ("weight" %in% stateLabelSpec) {
            if (is.null(mdp)) {
               stop(
                  "mdp model must be provided to plotHypergraph when stateLabel contains \"weight\".",
                  call. = FALSE
               )
            }
            policy_weights <- getPolicy(mdp) %>% dplyr::select("sId", "weight")
            hgf$nodes <- hgf$nodes %>% dplyr::left_join(policy_weights, by = "sId")
            stateWeights <- formatNumericLabel(hgf$nodes$weight)
         }
         hgf$nodes$label <- vapply(seq_len(nrow(hgf$nodes)), function(i) {
            collapseLabelParts(vapply(stateLabelSpec, function(part) {
               switch(
                  part,
                  label = stateLabels[i],
                  sId = as.character(hgf$nodes$sId[i]),
                  sIdx = stateSIdx[i],
                  weight = stateWeights[i],
                  NA_character_
               )
            }, character(1)))
         }, character(1))
         hgf$nodes <- hgf$nodes %>% dplyr::select(-dplyr::any_of("weight"))
      }
   }

   # Apply actionLabel and actionWLabel logic to hgf$hyperarcs
   if (!is.null(hgf$hyperarcs)) {
      orig_label <- hgf$hyperarcs$label
      if (identical(actionLabelSpec, "none")) {
         hgf$hyperarcs$label <- NA_character_
      } else if (identical(actionLabelSpec, "custom")) {
         if (!"actionLabel" %in% names(hgf$hyperarcs)) {
            stop(
               "actionLabel = \"custom\" requires an actionLabel column in hgf$hyperarcs.",
               call. = FALSE
            )
         }
         hgf$hyperarcs$label <- as.character(hgf$hyperarcs$actionLabel)
      } else {
         hgf$hyperarcs$label <- vapply(seq_len(nrow(hgf$hyperarcs)), function(i) {
            collapseLabelParts(vapply(actionLabelSpec, function(part) {
               switch(
                  part,
                  label = as.character(orig_label[i]),
                  aIdx = as.character(hgf$hyperarcs$aIdx[i]),
                  NA_character_
               )
            }, character(1)))
         }, character(1))
      }

      if (identical(actionWLabelSpec, "weight")) {
         hgf$hyperarcs$actionWLabel <- purrr::map_chr(hgf$hyperarcs$actionWeights, function(w) {
            formatWeightVector(w)
         })
      } else if (identical(actionWLabelSpec, "custom")) {
         if (!"actionWLabel" %in% names(hgf$hyperarcs)) {
            stop(
               "actionWLabel = \"custom\" requires an actionWLabel column in hgf$hyperarcs.",
               call. = FALSE
            )
         }
         hgf$hyperarcs$actionWLabel <- as.character(hgf$hyperarcs$actionWLabel)
      } else {
         hgf$hyperarcs$actionWLabel <- NA_character_
      }
   }

   # internal functions
   gMap<-function(sId) return(hgf$nodes$gId[hgf$nodes$sId %in% sId])		# return gId given sId
   sMap<-function(gId) return(hgf$nodes$sId[hgf$nodes$gId %in% gId])		# return sId given gId
   ellipseBoundaryPoint <- function(mid, toward) {
      direction <- toward - mid
      if (all(direction == 0)) return(mid)
      scale <- 1 / sqrt((direction[1] / radx)^2 + (direction[2] / rady)^2)
      mid + scale * direction
   }
   ellipseBoundaryPoints <- function(mid, toward) {
      t(vapply(seq_len(nrow(mid)), function(i) {
         ellipseBoundaryPoint(mid[i, ], toward)
      }, numeric(2)))
   }
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

   xlim <- c(min(pos[,1])-marX,max(pos[,1])+marX)
   ylim <- c(min(pos[,2])-marY,max(pos[,2])+marY)
   openplotmat(xlim = xlim, ylim = ylim)  #main = "State expanded hypergraph"
   if (drawBorder) {
      outsidePadding <- stats::setNames(graphics::par("mai"), c("bottom", "left", "top", "right"))
      insidePadding <- c(
         bottom = min(pos[, 2])-ylim[1],
         left = min(pos[, 1])-xlim[1],
         top = ylim[2]-max(pos[, 2]),
         right = xlim[2]-max(pos[, 1])
      )
      message(
         "plotHypergraph padding: ",
         "outside figure margin in inches ",
         paste(names(outsidePadding), round(outsidePadding, 3), sep = "=", collapse = ", "),
         "; inside plot margin in user coordinates ",
         paste(names(insidePadding), round(insidePadding, 3), sep = "=", collapse = ", "),
         ". To remove outside spacing use par(mai = c(0, 0, 0, 0)); ",
         "to remove inside spacing use marX = 0 and marY = 0."
      )
   }
   
   # plot time index
   # if (addTime) {
   #    posT <- matrix(c(unique(pos[,1]), rep(0, gridDim[2])), ncol = 2)  # coordinates for time index
   #    colnames(posT) <- colnames(pos)
   #    for (i in 1:gridDim[2] - 1) textempty(posT[i+1, ], lab = parse(text = str_c("italic(t == ", i, ")")), cex=cex)
   # }
   

   # plot actions
   if (!is.null(hgf$hyperarcs)) {
      if (!"trans" %in% names(hgf$hyperarcs)) {
         stop("hgf$hyperarcs must contain a trans list-column.", call. = FALSE)
      }
      if (!identical(transLabelSpec, "none") && !identical(transLabelSpec, "custom") &&
          "prob" %in% transLabelSpec && !"pr" %in% names(hgf$hyperarcs)) {
         stop(
            "transLabels containing \"prob\" requires a pr list-column in hgf$hyperarcs.",
            call. = FALSE
         )
      }
      if (!identical(transLabelSpec, "none") && !identical(transLabelSpec, "custom") &&
          "weights" %in% transLabelSpec && !"transWeights" %in% names(hgf$hyperarcs)) {
         stop(
            "transLabels containing \"weights\" requires a transWeights list-column in hgf$hyperarcs.",
            call. = FALSE
         )
      }
      if (identical(transLabelSpec, "custom") && !"transLabels" %in% names(hgf$hyperarcs)) {
         stop(
            "transLabels = \"custom\" requires a transLabels list-column in hgf$hyperarcs.",
            call. = FALSE
         )
      }

      # Pre-populate transLabels based on transLabels option
      if (!identical(transLabelSpec, "none") && !identical(transLabelSpec, "custom")) {
         hgf$hyperarcs$transLabels <- purrr::pmap(
            list(
               trans = hgf$hyperarcs$trans,
               pr = if ("pr" %in% names(hgf$hyperarcs)) hgf$hyperarcs$pr else rep(list(NULL), nrow(hgf$hyperarcs)),
               transWeights = if ("transWeights" %in% names(hgf$hyperarcs)) hgf$hyperarcs$transWeights else rep(list(NULL), nrow(hgf$hyperarcs))
            ),
            function(trans, pr, transWeights) {
               trans <- as.numeric(trans)
               nTrans <- length(trans)
               weightLabels <- NULL
               if ("weights" %in% transLabelSpec) {
                  if (!is.matrix(transWeights)) {
                     stop(
                        "transLabels containing \"weights\" requires a transition-by-weight matrix in each transWeights row.",
                        call. = FALSE
                     )
                  }
                  if (ncol(transWeights) == 0) {
                     weightLabels <- rep(NA_character_, nrow(transWeights))
                  } else {
                     weightLabels <- apply(transWeights, 1, formatWeightVector)
                  }
               }
               vapply(seq_len(nTrans), function(j) {
                  collapseLabelParts(vapply(transLabelSpec, function(part) {
                     switch(
                        part,
                        label = hgf$nodes$label[match(trans[j], hgf$nodes$sId)],
                        sId = as.character(trans[j]),
                        prob = formatNumericLabel(pr[j]),
                        weights = weightLabels[j],
                        NA_character_
                     )
                  }, character(1)))
               }, character(1))
            }
         )
      }

      actionKeys <- vapply(
         seq_len(nrow(hgf$hyperarcs)),
         function(i) {
            trans <- as.numeric(hgf$hyperarcs$trans[[i]])
            trans <- trans[!is.na(trans)]
            paste(hgf$hyperarcs$sId[i], paste(trans, collapse = ","), sep = ":")
         },
         character(1)
      )
      actionOffsets <- stats::ave(
         seq_along(actionKeys),
         actionKeys,
         FUN = function(x) seq_along(x) - (length(x) + 1) / 2
      )
      for (i in seq_len(nrow(hgf$hyperarcs))) {
         trans <- as.numeric(hgf$hyperarcs$trans[[i]])
         validTrans <- !is.na(trans)
         trans <- trans[validTrans]
         if (length(trans) == 0) next
         fromPos <- pos[gMap(trans), , drop = FALSE]
         toPos <- pos[gMap(hgf$hyperarcs$sId[i]), ]
         centre <- NULL
         if (actionOffsets[i] != 0 && actionOffset != 0) {
            baseCentre <- colMeans(fromPos) + 0.5 * (toPos - colMeans(fromPos))
            direction <- toPos - colMeans(fromPos)
            directionLength <- sqrt(sum(direction^2))
            if (directionLength > 0) {
               perpendicular <- c(-direction[2], direction[1]) / directionLength
               centre <- baseCentre + actionOffsets[i] * actionOffset * perpendicular
            }
         }
         splitCentre <- if (is.null(centre)) {
            colMeans(fromPos) + 0.5 * (toPos - colMeans(fromPos))
         } else {
            centre
         }
         fromBoundary <- ellipseBoundaryPoints(fromPos, splitCentre)
         toBoundary <- ellipseBoundaryPoint(toPos, splitCentre)
         #cat("i:",i,"highlight:",hgf$hyperarcs$highlight[i],"\n")
         # if (hgf$hyperarcs$highlight[i]) splitarrow(from = pos[gMap(trans), ], to = pos[gMap(hgf$hyperarcs[i,1]),], arr.side = 2, arr.pos = 0.1, lwd=2, lty=1,
         #                                      arr.type="curved", arr.lwd = 0.5, arr.length = 0.1, arr.width = 0.08, lcol="gray")
         pt <-
            splitarrow(
               from = fromBoundary,
               to = toBoundary,
               centre = centre,
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
         if ("actionWLabel" %in% names(hgf$hyperarcs) && !is.na(hgf$hyperarcs$actionWLabel[i]) && hgf$hyperarcs$actionWLabel[i] != "") {
            textempty(
               (toBoundary + pt) / 2,
               lab = hgf$hyperarcs$actionWLabel[i],
               adj = c(0.5, -0.6),
               cex = cex,
               ...
            )
         }
         if (!identical(transLabelSpec, "none")) {
            labs <- hgf$hyperarcs$transLabels[[i]][validTrans]
            if (length(labs) != length(trans)) {
               stop(
                  paste0("transLabels = \"", transLabels, "\" requires one label per plotted transition."),
                  call. = FALSE
               )
            }
            for (j in seq_along(trans)) {
               stateIndex <- match(trans[j], hgf$nodes$sId)
               if (is.na(stateIndex) || is.na(labs[j])) next
               transPos <- pos[hgf$nodes$gId[stateIndex], ]
               transBoundary <- ellipseBoundaryPoint(transPos, splitCentre)
               labelPos <- (pt + transBoundary) / 2
               textempty(
                  labelPos,
                  lab = labs[j],
                  adj = transLabelAdj,
                  cex = transLabelCex,
                  ...
               )
            }
         }
      }
   }	   
   
   # plot states
   if (!is.null(hgf$nodes)) {
      for (i in seq_len(nrow(hgf$nodes))) { 
         textellipse(pos[hgf$nodes$gId[i], ], lab = hgf$nodes$label[i], radx = radx, rady=rady, shadow.size = 0, lwd=0.5, cex=cex) 
      }
   }
   
   # visual view of the point numbers (for figuring out how to map stateId to gridId)
   if (showGrid) {
      for (i in seq_len(dim(pos)[1])) textrect(pos[i, ], lab = i, radx = 0.0, cex=cex)
   }
   if (drawBorder) {
      graphics::box(which = "plot")
   }
   return(invisible(NULL))
}



#' Plot the state-expanded hypergraph of the MDP. 
#'
#' @param x The MDP model. 
#' @param ... Arguments passed to [plotHypergraph()].
#'
#' @return No return value (NULL invisible), called for side effects (plotting).
#' @seealso [getHypergraph()] and [plotHypergraph()] for possible arguments.
#' @example inst/examples/plot-ex.R
#' @importFrom rlang .data
#' @export
plot.HMDP <- function(x, ...) {
   args <- list(...)
   mdp <- x
   
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

   do.call(plotHypergraph, args = c(list(hgf, gridDim, mdp = mdp), args))
   return(invisible(NULL))
}


#' Return the (parts of) state-expanded hypergraph
#' 
#' The function is useful together with [plotHypergraph()].
#'
#' @param mdp The MDP loaded using [loadMDP()].
#' @param ... Arguments passed to [getInfo()].
#'
#' @return A list representing the hypergraph with two elements: a tibble
#'   `nodes` and a tibble `hyperarcs`. `hyperarcs` stores `actionWeights`,
#'   `trans`, and `pr` as list-columns of vectors. `transWeights` is a
#'   list-column of matrices with one row per transition and one column per
#'   transition-weight namespace.
#' @seealso [plotHypergraph()] and [plot.HMDP()].
#' @importFrom rlang .data
#' @export
#' @example inst/examples/plot-ex.R
getHypergraph <- function(mdp, ...) {
   dat <- getInfo(mdp, withList = FALSE, ...)
   nActionWeights <- length(mdp$weightActionNames)
   nTransWeights <- length(mdp$weightTransNames)
   normalizeActionWeights <- function(x) {
      if (nActionWeights == 0) return(numeric(0))
      x <- as.numeric(x)
      if (length(x) == 0 || all(is.na(x))) return(rep(NA_real_, nActionWeights))
      if (length(x) != nActionWeights) {
         stop("Action weights do not match action weight names.", call. = FALSE)
      }
      x
   }
   normalizeTransWeights <- function(x, nTrans) {
      if (nTransWeights == 0) {
         return(matrix(numeric(0), nrow = nTrans, ncol = 0))
      }
      x <- as.numeric(x)
      if (length(x) == 0 || all(is.na(x))) x <- rep(NA_real_, nTrans * nTransWeights)
      if (length(x) != nTrans * nTransWeights) {
         stop("Transition weights do not match transitions and weight names.", call. = FALSE)
      }
      matrix(
         x,
         nrow = nTrans,
         ncol = nTransWeights,
         byrow = TRUE,
         dimnames = list(NULL, mdp$weightTransNames)
      )
   }
   actions <- dat$df %>% 
      tidyr::unnest_longer("actions") %>% # convert actions (one row for each action)
      tidyr::unnest_wider("actions", names_repair = tidyr::tidyr_legacy) %>% 
      dplyr::rename(stateLabel = "label") %>%
      dplyr::filter(!is.na(.data$aIdx)) %>% 
      dplyr::mutate(
         actionWeights = purrr::map(.data$weights, normalizeActionWeights),
         trans = purrr::map(.data$trans, as.numeric),
         pr = purrr::map(.data$pr, as.numeric),
         transWeights = purrr::map2(
            .data$transWeights,
            .data$trans,
            ~ normalizeTransWeights(.x, length(.y))
         ),
         transLabels = purrr::map(.data$trans, ~ rep(NA_character_, length(.x))),
         actionWLabel = NA_character_,
         label = as.character(.data$label1)
      ) %>%
      dplyr::mutate(lwd = 1, lty = 1, col = "black") %>% 
      dplyr::select(-"stateStr", -"stateLabel", -"weights", -"label1")

   states <- dat$df %>% 
      dplyr::mutate(gId = NA_integer_) %>% 
      dplyr::select("sId", "stateStr", "label", "gId")
   if (mdp$timeHorizon == Inf) {
      states$label[1:mdp$founderStatesLast] <- states$label[(nrow(states) - mdp$founderStatesLast + 1):nrow(states)]
   }

   return(list(nodes = states, hyperarcs = actions))
}
