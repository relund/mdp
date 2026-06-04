#' Plot parts of the state expanded hypergraph.
#'
#' The plot is created based on a grid `(rows, cols)`. Each grid point is numbered from bottom to
#' top and left to right (starting from 1), i.e. given grid point with coordinates `(r, c)` (where
#' `(1,1)` is the top left corner and `(rows, cols)` is the bottom right corner) the grid id is `(c
#' - 1) * rows + r`. You must assign a node to the hypergraph to a grid point (see below).
#'
#' @param hgf A list with the hypergraph containing two data frames, normally found using
#'   [get_hypergraph()]. The data frame `nodes` must have columns: `s_id` (state id), `g_id` (grid id)
#'   and `label` (node label). The data frame `hyperarcs` must have columns `s_id` (head node),
#'   `trans` (a list-column of tail node ids), `pr` (a list-column of transition
#'   probabilities), `action_weights` (a list-column of action weights),
#'   `trans_weights` (a list-column of transition-by-weight matrices), `a_idx`
#'   (action index), `label` (action label), `lwd` (hyperarc line width), `lty`
#'   (hyperarc line type) and `col` (hyperarc color).
#' @param grid_dim A 2-dim vector (rows, cols) representing the size of the grid.
#' @param show_grid If true show the grid points (good for debugging).
#' @param radx Horizontal radius of the box.
#' @param rady Vertical radius of the box.
#' @param cex Relative size of text.
#' @param mar_x Horizontal margin.
#' @param mar_y Vertical margin.
#' @param draw_border If `TRUE`, draw a border around the plot region and report the
#'   outside and inside padding (good for debugging).
#' @param action_offset Distance used to separate actions with the same
#'   start and trans states. Set to `0` to draw overlapping actions.
#' @param trans_labels Transition-label mode. `"none"` draws no transition labels
#'   (the default); `"custom"` draws values from an optional `trans_labels`
#'   list-column in `hgf$hyperarcs`; otherwise use a `|`-separated
#'   combination of `"label"`, `"s_id"`, `"prob"`, and `"weights"`, for example
#'   `"prob|weights"`. The older `"state"` spelling is treated as `"label"`.
#' @param trans_label_cex Relative size of transition-label text.
#' @param trans_label_adj Position adjustment passed to `textempty()` for transition
#'   labels, drawn at the middle of each split-to-transition branch.
#' @param state_label What to plot in states. `"custom"` uses a `state_label`
#'   column in `hgf$nodes`; otherwise use a `|`-separated combination of
#'   `"label"` (state label, default), `"s_id"` (state id), `"s_idx"`
#'   (stage-based state index), and `"weight"` (optimal weight of the state).
#' @param action_label What to plot near the split. One of `"none"`, `"custom"`
#'   (uses an `action_label` column in `hgf$hyperarcs`), or a `|`-separated
#'   combination of `"label"` (action label, default) and `"a_idx"`.
#' @param action_w_label What to plot from the start state to the split. One of
#'   `"none"` (default), `"weight"`, or `"custom"` (uses an `action_w_label`
#'   column in `hgf$hyperarcs`).
#' @param action_color Action coloring scheme. Default `""` uses black lines. `"label"` uses different colors based on the action labels. `"policy"` highlights the current policy.
#' @param actions_visible Action visibility mode. `"all"` (default) shows all actions. `"policy"` only shows actions in the current policy.
#' @param connected_to Optional vector of state ids. If supplied, plot only states
#'   reachable from these states by following visible hyperarcs forward,
#'   and trim hyperarcs and transition-level data to the remaining states.
#' @param recalc_grid If `TRUE` and `connected_to` is supplied, recalculate the
#'   grid for the visible nodes. Nodes keep their original columns, but visible
#'   nodes within each column are placed consecutively from the top and the
#'   number of grid rows is reduced to the maximum number of visible nodes in
#'   any column.
#' @param mdp The MDP model. Required if `state_label` contains `"weight"`,
#'   `action_color = "policy"`, or `actions_visible = "policy"`.
#' @param ... Graphical parameters passed to `textempty`.
#'
#' @return No return value (NULL invisible), called for side effects (plotting).
#' @seealso [get_hypergraph()] and [plot.HMDP()].
#' @example inst/examples/plot-ex.R
#' @import diagram
#' @export
plot_hypergraph <-
  function(hgf,
           grid_dim,
           show_grid = FALSE,
           radx = 0.03,
           rady = 0.05,
           cex = 1,
           mar_x = 0.035,
           mar_y = 0.15,
           draw_border = FALSE,
           action_offset = 0.025,
           trans_labels = "none",
           trans_label_cex = 0.8 * cex,
           trans_label_adj = c(0.5, -0.6),
           state_label = "label",
           action_label = "label",
           action_w_label = "none",
           action_color = c("", "label", "policy"),
           actions_visible = c("all", "policy"),
           connected_to = NULL,
           recalc_grid = FALSE,
           mdp = NULL,
           ...) {
    normalize_label_arg <- function(x, default, arg) {
      if (missing(x) || is.null(x) || identical(x, "")) {
        return(default)
      }
      if (!is.character(x) || length(x) != 1) {
        stop(paste0(arg, " must be a single character string."), call. = FALSE)
      }
      x
    }
    parse_label_spec <- function(x, allowed, arg, special = character(0), aliases = character(0)) {
      if (x %in% special) {
        return(x)
      }
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
    collapse_label_parts <- function(parts) {
      parts <- as.character(parts)
      parts <- parts[!is.na(parts) & parts != ""]
      if (length(parts) == 0) {
        return(NA_character_)
      }
      paste(parts, collapse = " | ")
    }
    format_numeric_label <- function(x) {
      as.character(round(as.numeric(x), 2))
    }
    format_weight_vector <- function(w) {
      w <- w[!is.na(w)]
      if (length(w) == 0) {
        NA_character_
      } else if (length(w) == 1) {
        format_numeric_label(w)
      } else {
        paste0("(", paste(format_numeric_label(w), collapse = ", "), ")")
      }
    }

    trans_labels <- normalize_label_arg(trans_labels, "none", "trans_labels")
    state_label <- normalize_label_arg(state_label, "label", "state_label")
    action_label <- normalize_label_arg(action_label, "label", "action_label")
    action_w_label <- normalize_label_arg(action_w_label, "none", "action_w_label")
    transLabelSpec <- parse_label_spec(
      trans_labels,
      c("label", "s_id", "prob", "weights"),
      "trans_labels",
      special = c("none", "custom"),
      aliases = c(state = "label")
    )
    stateLabelSpec <- parse_label_spec(
      state_label,
      c("label", "s_id", "s_idx", "weight"),
      "state_label",
      special = "custom"
    )
    actionLabelSpec <- parse_label_spec(
      action_label,
      c("label", "a_idx"),
      "action_label",
      special = c("none", "custom")
    )
    actionWLabelSpec <- parse_label_spec(
      action_w_label,
      "weight",
      "action_w_label",
      special = c("none", "custom")
    )
    action_color <- match.arg(action_color)
    actions_visible <- match.arg(actions_visible)

    # Apply actions_visible and action_color logic to hgf$hyperarcs
    if (!is.null(hgf$hyperarcs)) {
      if (actions_visible == "policy") {
        if (is.null(mdp)) {
          stop("mdp model must be provided to plot_hypergraph when actions_visible = \"policy\".", call. = FALSE)
        }
        policy_arcs <- get_policy(mdp) %>%
          dplyr::select("s_id", "a_idx") %>%
          dplyr::mutate(is_policy = TRUE)
        hgf$hyperarcs <- hgf$hyperarcs %>%
          dplyr::inner_join(policy_arcs, by = c("s_id", "a_idx"))
      }
      if (action_color == "label") {
        col_df <- tibble::tibble(
          label = unique(hgf$hyperarcs$label),
          col = grDevices::rainbow(length(unique(hgf$hyperarcs$label)))
        )
        hgf$hyperarcs <- hgf$hyperarcs %>%
          dplyr::select(-col) %>%
          dplyr::left_join(col_df, by = "label")
      }
      if (action_color == "policy") {
        if (is.null(mdp)) {
          stop("mdp model must be provided to plot_hypergraph when action_color = \"policy\".", call. = FALSE)
        }
        policy_arcs <- get_policy(mdp) %>%
          dplyr::select("s_id", "a_idx") %>%
          dplyr::mutate(is_policy = TRUE)
        hgf$hyperarcs <- hgf$hyperarcs %>%
          dplyr::left_join(policy_arcs, by = c("s_id", "a_idx")) %>%
          dplyr::mutate(col = ifelse(is.na(.data$is_policy), "black", "blue")) %>%
          dplyr::select(-"is_policy")
      }
    }

    filter_connected_hypergraph <- function(hgf, connected_to) {
      if (is.null(connected_to)) {
        return(hgf)
      }
      if (!is.numeric(connected_to)) {
        stop("connected_to must be a numeric vector of state ids.", call. = FALSE)
      }
      connected_to <- unique(stats::na.omit(as.numeric(connected_to)))
      if (length(connected_to) == 0) {
        return(hgf)
      }
      if (is.null(hgf$nodes) || !"s_id" %in% names(hgf$nodes)) {
        stop("connected_to requires hgf$nodes to contain an s_id column.", call. = FALSE)
      }
      missing_s_id <- setdiff(connected_to, hgf$nodes$s_id)
      if (length(missing_s_id) > 0) {
        stop(
          paste0("connected_to contains s_id values not present in hgf$nodes: ", paste(missing_s_id, collapse = ", ")),
          call. = FALSE
        )
      }
      if (is.null(hgf$hyperarcs) || nrow(hgf$hyperarcs) == 0) {
        hgf$nodes <- hgf$nodes[hgf$nodes$s_id %in% connected_to, , drop = FALSE]
        return(hgf)
      }
      if (!"trans" %in% names(hgf$hyperarcs)) {
        stop("connected_to requires hgf$hyperarcs to contain a trans list-column.", call. = FALSE)
      }

      forwardEdges <- lapply(hgf$nodes$s_id, function(x) numeric(0))
      names(forwardEdges) <- as.character(hgf$nodes$s_id)
      for (i in seq_len(nrow(hgf$hyperarcs))) {
        trans <- as.numeric(hgf$hyperarcs$trans[[i]])
        trans <- trans[!is.na(trans)]
        key <- as.character(hgf$hyperarcs$s_id[i])
        if (!is.null(forwardEdges[[key]])) {
          forwardEdges[[key]] <- unique(c(forwardEdges[[key]], trans))
        }
      }

      reachable <- connected_to
      queue <- connected_to
      while (length(queue) > 0) {
        current <- queue[1]
        queue <- queue[-1]
        nextStates <- forwardEdges[[as.character(current)]]
        nextStates <- nextStates[nextStates %in% hgf$nodes$s_id]
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
        keepArc[i] <- hgf$hyperarcs$s_id[i] %in% reachable && any(transKeep[[i]])
      }
      hgf$hyperarcs <- hgf$hyperarcs[keepArc, , drop = FALSE]
      transKeep <- transKeep[keepArc]

      for (i in seq_len(nrow(hgf$hyperarcs))) {
        keep <- transKeep[[i]]
        hgf$hyperarcs$trans[[i]] <- hgf$hyperarcs$trans[[i]][keep]
        if ("pr" %in% names(hgf$hyperarcs) && length(hgf$hyperarcs$pr[[i]]) == length(keep)) {
          hgf$hyperarcs$pr[[i]] <- hgf$hyperarcs$pr[[i]][keep]
        }
        if ("trans_labels" %in% names(hgf$hyperarcs) && length(hgf$hyperarcs$trans_labels[[i]]) == length(keep)) {
          hgf$hyperarcs$trans_labels[[i]] <- hgf$hyperarcs$trans_labels[[i]][keep]
        }
        if ("trans_weights" %in% names(hgf$hyperarcs) && is.matrix(hgf$hyperarcs$trans_weights[[i]]) &&
          nrow(hgf$hyperarcs$trans_weights[[i]]) == length(keep)) {
          hgf$hyperarcs$trans_weights[[i]] <- hgf$hyperarcs$trans_weights[[i]][keep, , drop = FALSE]
        }
      }
      hgf$nodes <- hgf$nodes[hgf$nodes$s_id %in% reachable, , drop = FALSE]
      return(hgf)
    }

    hgf <- filter_connected_hypergraph(hgf, connected_to)

    recalculate_grid <- function(hgf, grid_dim) {
      if (is.null(hgf$nodes) || nrow(hgf$nodes) == 0) {
        return(list(hgf = hgf, grid_dim = grid_dim))
      }
      if (!"g_id" %in% names(hgf$nodes)) {
        stop("recalc_grid requires hgf$nodes to contain a g_id column.", call. = FALSE)
      }
      originalCols <- (hgf$nodes$g_id - 1) %/% grid_dim[1] + 1
      originalRows <- (hgf$nodes$g_id - 1) %% grid_dim[1] + 1
      if (any(is.na(originalCols) | originalCols < 1 | originalCols > grid_dim[2])) {
        stop("recalc_grid requires node g_id values to be inside grid_dim.", call. = FALSE)
      }
      visibleCounts <- tabulate(originalCols, nbins = grid_dim[2])
      rowsNew <- max(visibleCounts)
      if (rowsNew == 0) {
        return(list(hgf = hgf, grid_dim = c(1, grid_dim[2])))
      }
      newRows <- integer(length(originalRows))
      for (col in seq_len(grid_dim[2])) {
        idx <- which(originalCols == col)
        if (length(idx) == 0) next
        idx <- idx[order(originalRows[idx], hgf$nodes$s_id[idx])]
        newRows[idx] <- seq_along(idx)
      }
      hgf$nodes$g_id <- (originalCols - 1) * rowsNew + newRows
      list(hgf = hgf, grid_dim = c(rowsNew, grid_dim[2]))
    }

    if (!is.null(connected_to) && isTRUE(recalc_grid)) {
      gridRecalc <- recalculate_grid(hgf, grid_dim)
      hgf <- gridRecalc$hgf
      grid_dim <- gridRecalc$grid_dim
    }

    # Apply state_label logic to hgf$nodes$label
    if (!is.null(hgf$nodes)) {
      if (identical(stateLabelSpec, "custom")) {
        if (!"state_label" %in% names(hgf$nodes)) {
          stop(
            "state_label = \"custom\" requires a state_label column in hgf$nodes.",
            call. = FALSE
          )
        }
        hgf$nodes$label <- as.character(hgf$nodes$state_label)
      } else {
        state_labels <- as.character(hgf$nodes$label)
        stateSIdx <- NULL
        stateWeights <- NULL
        if ("s_idx" %in% stateLabelSpec) {
          if (!"state_str" %in% names(hgf$nodes)) {
            stop("state_label containing \"s_idx\" requires a state_str column in hgf$nodes.", call. = FALSE)
          }
          parts <- strsplit(hgf$nodes$state_str, ",")
          stateSIdx <- vapply(parts, function(x) {
            if (length(x) > 0) x[length(x)] else ""
          }, character(1))
        }
        if ("weight" %in% stateLabelSpec) {
          if (is.null(mdp)) {
            stop(
              "mdp model must be provided to plot_hypergraph when state_label contains \"weight\".",
              call. = FALSE
            )
          }
          policy_weights <- get_policy(mdp) %>% dplyr::select("s_id", "weight")
          hgf$nodes <- hgf$nodes %>% dplyr::left_join(policy_weights, by = "s_id")
          stateWeights <- format_numeric_label(hgf$nodes$weight)
        }
        hgf$nodes$label <- vapply(seq_len(nrow(hgf$nodes)), function(i) {
          collapse_label_parts(vapply(stateLabelSpec, function(part) {
            switch(part,
              label = state_labels[i],
              s_id = as.character(hgf$nodes$s_id[i]),
              s_idx = stateSIdx[i],
              weight = stateWeights[i],
              NA_character_
            )
          }, character(1)))
        }, character(1))
        hgf$nodes <- hgf$nodes %>% dplyr::select(-dplyr::any_of("weight"))
      }
    }

    # Apply action_label and action_w_label logic to hgf$hyperarcs
    if (!is.null(hgf$hyperarcs)) {
      orig_label <- hgf$hyperarcs$label
      if (identical(actionLabelSpec, "none")) {
        hgf$hyperarcs$label <- NA_character_
      } else if (identical(actionLabelSpec, "custom")) {
        if (!"action_label" %in% names(hgf$hyperarcs)) {
          stop(
            "action_label = \"custom\" requires an action_label column in hgf$hyperarcs.",
            call. = FALSE
          )
        }
        hgf$hyperarcs$label <- as.character(hgf$hyperarcs$action_label)
      } else {
        hgf$hyperarcs$label <- vapply(seq_len(nrow(hgf$hyperarcs)), function(i) {
          collapse_label_parts(vapply(actionLabelSpec, function(part) {
            switch(part,
              label = as.character(orig_label[i]),
              a_idx = as.character(hgf$hyperarcs$a_idx[i]),
              NA_character_
            )
          }, character(1)))
        }, character(1))
      }

      if (identical(actionWLabelSpec, "weight")) {
        hgf$hyperarcs$action_w_label <- purrr::map_chr(hgf$hyperarcs$action_weights, function(w) {
          format_weight_vector(w)
        })
      } else if (identical(actionWLabelSpec, "custom")) {
        if (!"action_w_label" %in% names(hgf$hyperarcs)) {
          stop(
            "action_w_label = \"custom\" requires an action_w_label column in hgf$hyperarcs.",
            call. = FALSE
          )
        }
        hgf$hyperarcs$action_w_label <- as.character(hgf$hyperarcs$action_w_label)
      } else {
        hgf$hyperarcs$action_w_label <- NA_character_
      }
    }

    # internal functions
    g_map <- function(s_id) {
      return(hgf$nodes$g_id[hgf$nodes$s_id %in% s_id])
    } # return g_id given s_id
    s_map <- function(g_id) {
      return(hgf$nodes$s_id[hgf$nodes$g_id %in% g_id])
    } # return s_id given g_id
    ellipse_boundary_point <- function(mid, toward) {
      direction <- toward - mid
      if (all(direction == 0)) {
        return(mid)
      }
      scale <- 1 / sqrt((direction[1] / radx)^2 + (direction[2] / rady)^2)
      mid + scale * direction
    }
    ellipse_boundary_points <- function(mid, toward) {
      t(vapply(seq_len(nrow(mid)), function(i) {
        ellipse_boundary_point(mid[i, ], toward)
      }, numeric(2)))
    }
    pos <- coordinates(rep(grid_dim[2], grid_dim[1]), hor = TRUE) # coordinates of each point in the grid

    # reposition
    posN <- pos
    for (i in 1:nrow(pos)) {
      c <- (i - 1) %% grid_dim[2] + 1
      r <- (i - 1) %/% grid_dim[2] + 1
      id <- (c - 1) * grid_dim[1] + r
      # cat(i, r, c, id, "\n", sep= " ")
      posN[id, ] <- pos[i, ]
    }
    pos <- posN

    xlim <- c(min(pos[, 1]) - mar_x, max(pos[, 1]) + mar_x)
    ylim <- c(min(pos[, 2]) - mar_y, max(pos[, 2]) + mar_y)
    openplotmat(xlim = xlim, ylim = ylim) # main = "State expanded hypergraph"
    if (draw_border) {
      outsidePadding <- stats::setNames(graphics::par("mai"), c("bottom", "left", "top", "right"))
      insidePadding <- c(
        bottom = min(pos[, 2]) - ylim[1],
        left = min(pos[, 1]) - xlim[1],
        top = ylim[2] - max(pos[, 2]),
        right = xlim[2] - max(pos[, 1])
      )
      message(
        "plot_hypergraph padding: ",
        "outside figure margin in inches ",
        paste(names(outsidePadding), round(outsidePadding, 3), sep = "=", collapse = ", "),
        "; inside plot margin in user coordinates ",
        paste(names(insidePadding), round(insidePadding, 3), sep = "=", collapse = ", "),
        ". To remove outside spacing use par(mai = c(0, 0, 0, 0)); ",
        "to remove inside spacing use mar_x = 0 and mar_y = 0."
      )
    }

    # plot time index
    # if (addTime) {
    #    posT <- matrix(c(unique(pos[,1]), rep(0, grid_dim[2])), ncol = 2)  # coordinates for time index
    #    colnames(posT) <- colnames(pos)
    #    for (i in 1:grid_dim[2] - 1) textempty(posT[i+1, ], lab = parse(text = str_c("italic(t == ", i, ")")), cex=cex)
    # }


    # plot actions
    if (!is.null(hgf$hyperarcs)) {
      if (!"trans" %in% names(hgf$hyperarcs)) {
        stop("hgf$hyperarcs must contain a trans list-column.", call. = FALSE)
      }
      if (!identical(transLabelSpec, "none") && !identical(transLabelSpec, "custom") &&
        "prob" %in% transLabelSpec && !"pr" %in% names(hgf$hyperarcs)) {
        stop(
          "trans_labels containing \"prob\" requires a pr list-column in hgf$hyperarcs.",
          call. = FALSE
        )
      }
      if (!identical(transLabelSpec, "none") && !identical(transLabelSpec, "custom") &&
        "weights" %in% transLabelSpec && !"trans_weights" %in% names(hgf$hyperarcs)) {
        stop(
          "trans_labels containing \"weights\" requires a trans_weights list-column in hgf$hyperarcs.",
          call. = FALSE
        )
      }
      if (identical(transLabelSpec, "custom") && !"trans_labels" %in% names(hgf$hyperarcs)) {
        stop(
          "trans_labels = \"custom\" requires a trans_labels list-column in hgf$hyperarcs.",
          call. = FALSE
        )
      }

      # Pre-populate trans_labels based on trans_labels option
      if (!identical(transLabelSpec, "none") && !identical(transLabelSpec, "custom")) {
        hgf$hyperarcs$trans_labels <- purrr::pmap(
          list(
            trans = hgf$hyperarcs$trans,
            pr = if ("pr" %in% names(hgf$hyperarcs)) hgf$hyperarcs$pr else rep(list(NULL), nrow(hgf$hyperarcs)),
            trans_weights = if ("trans_weights" %in% names(hgf$hyperarcs)) hgf$hyperarcs$trans_weights else rep(list(NULL), nrow(hgf$hyperarcs))
          ),
          function(trans, pr, trans_weights) {
            trans <- as.numeric(trans)
            n_trans <- length(trans)
            weightLabels <- NULL
            if ("weights" %in% transLabelSpec) {
              if (!is.matrix(trans_weights)) {
                stop(
                  "trans_labels containing \"weights\" requires a transition-by-weight matrix in each trans_weights row.",
                  call. = FALSE
                )
              }
              if (ncol(trans_weights) == 0) {
                weightLabels <- rep(NA_character_, nrow(trans_weights))
              } else {
                weightLabels <- apply(trans_weights, 1, format_weight_vector)
              }
            }
            vapply(seq_len(n_trans), function(j) {
              collapse_label_parts(vapply(transLabelSpec, function(part) {
                switch(part,
                  label = hgf$nodes$label[match(trans[j], hgf$nodes$s_id)],
                  s_id = as.character(trans[j]),
                  prob = format_numeric_label(pr[j]),
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
          paste(hgf$hyperarcs$s_id[i], paste(trans, collapse = ","), sep = ":")
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
        fromPos <- pos[g_map(trans), , drop = FALSE]
        toPos <- pos[g_map(hgf$hyperarcs$s_id[i]), ]
        centre <- NULL
        if (actionOffsets[i] != 0 && action_offset != 0) {
          baseCentre <- colMeans(fromPos) + 0.5 * (toPos - colMeans(fromPos))
          direction <- toPos - colMeans(fromPos)
          directionLength <- sqrt(sum(direction^2))
          if (directionLength > 0) {
            perpendicular <- c(-direction[2], direction[1]) / directionLength
            centre <- baseCentre + actionOffsets[i] * action_offset * perpendicular
          }
        }
        splitCentre <- if (is.null(centre)) {
          colMeans(fromPos) + 0.5 * (toPos - colMeans(fromPos))
        } else {
          centre
        }
        fromBoundary <- ellipse_boundary_points(fromPos, splitCentre)
        toBoundary <- ellipse_boundary_point(toPos, splitCentre)
        # cat("i:",i,"highlight:",hgf$hyperarcs$highlight[i],"\n")
        # if (hgf$hyperarcs$highlight[i]) splitarrow(from = pos[g_map(trans), ], to = pos[g_map(hgf$hyperarcs[i,1]),], arr.side = 2, arr.pos = 0.1, lwd=2, lty=1,
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
        if ("action_w_label" %in% names(hgf$hyperarcs) && !is.na(hgf$hyperarcs$action_w_label[i]) && hgf$hyperarcs$action_w_label[i] != "") {
          textempty(
            (toBoundary + pt) / 2,
            lab = hgf$hyperarcs$action_w_label[i],
            adj = c(0.5, -0.6),
            cex = cex,
            ...
          )
        }
        if (!identical(transLabelSpec, "none")) {
          labs <- hgf$hyperarcs$trans_labels[[i]][validTrans]
          if (length(labs) != length(trans)) {
            stop(
              paste0("trans_labels = \"", trans_labels, "\" requires one label per plotted transition."),
              call. = FALSE
            )
          }
          for (j in seq_along(trans)) {
            stateIndex <- match(trans[j], hgf$nodes$s_id)
            if (is.na(stateIndex) || is.na(labs[j])) next
            transPos <- pos[hgf$nodes$g_id[stateIndex], ]
            transBoundary <- ellipse_boundary_point(transPos, splitCentre)
            labelPos <- (pt + transBoundary) / 2
            textempty(
              labelPos,
              lab = labs[j],
              adj = trans_label_adj,
              cex = trans_label_cex,
              ...
            )
          }
        }
      }
    }

    # plot states
    if (!is.null(hgf$nodes)) {
      for (i in seq_len(nrow(hgf$nodes))) {
        textellipse(pos[hgf$nodes$g_id[i], ], lab = hgf$nodes$label[i], radx = radx, rady = rady, shadow.size = 0, lwd = 0.5, cex = cex)
      }
    }

    # visual view of the point numbers (for figuring out how to map stateId to gridId)
    if (show_grid) {
      for (i in seq_len(dim(pos)[1])) textrect(pos[i, ], lab = i, radx = 0.0, cex = cex)
    }
    if (draw_border) {
      graphics::box(which = "plot")
    }
    return(invisible(NULL))
  }


#' Plot the state-expanded hypergraph of the MDP.
#'
#' @param x The MDP model.
#' @param ... Arguments passed to [plot_hypergraph()].
#'
#' @return No return value (NULL invisible), called for side effects (plotting).
#' @seealso [get_hypergraph()] and [plot_hypergraph()] for possible arguments.
#' @example inst/examples/plot-ex.R
#' @importFrom rlang .data
#' @export
plot.HMDP <- function(x, ...) {
  args <- list(...)
  mdp <- x

  if (mdp$levels != 1) {
    message(
      "Cannot plot a hierarchical MDP without specifying the placement of\n",
      "states and actions (use `plot_hypergraph` instead)."
    )
    return(invisible(NULL))
  }
  if (mdp$time_horizon < Inf) {
    statesCt <- purrr::map_dbl(paste0(1:mdp$time_horizon - 1), mdp$ptr$getStateSizeStage)
    grid_dim <- c(max(statesCt), mdp$time_horizon)
  } else {
    grid_dim <- c(mdp$founder_states_last, 2)
  }
  hgf <- get_hypergraph(mdp)
  hgf$nodes <- hgf$nodes %>%
    tidyr::separate(.data$state_str, into = c("c", "r"), remove = FALSE, convert = TRUE) %>%
    dplyr::mutate(
      c = .data$c + 1,
      r = .data$r + 1,
      g_id = (.data$c - 1) * max(.data$r) + .data$r
    )

  do.call(plot_hypergraph, args = c(list(hgf, grid_dim, mdp = mdp), args))
  return(invisible(NULL))
}


#' Return the (parts of) state-expanded hypergraph
#'
#' The function is useful together with [plot_hypergraph()].
#'
#' @param mdp The MDP loaded using [load_mdp()].
#' @param ... Arguments passed to [get_info()].
#'
#' @return A list representing the hypergraph with two elements: a tibble
#'   `nodes` and a tibble `hyperarcs`. `hyperarcs` stores `action_weights`,
#'   `trans`, and `pr` as list-columns of vectors. `trans_weights` is a
#'   list-column of matrices with one row per transition and one column per
#'   transition-weight namespace.
#' @seealso [plot_hypergraph()] and [plot.HMDP()].
#' @importFrom rlang .data
#' @export
#' @example inst/examples/plot-ex.R
get_hypergraph <- function(mdp, ...) {
  dat <- get_info(mdp, with_list = FALSE, ...)
  nActionWeights <- length(mdp$weight_action_names)
  nTransWeights <- length(mdp$weight_trans_names)
  normalize_action_weights <- function(x) {
    if (nActionWeights == 0) {
      return(numeric(0))
    }
    x <- as.numeric(x)
    if (length(x) == 0 || all(is.na(x))) {
      return(rep(NA_real_, nActionWeights))
    }
    if (length(x) != nActionWeights) {
      stop("Action weights do not match action weight names.", call. = FALSE)
    }
    x
  }
  normalize_trans_weights <- function(x, n_trans) {
    if (nTransWeights == 0) {
      return(matrix(numeric(0), nrow = n_trans, ncol = 0))
    }
    x <- as.numeric(x)
    if (length(x) == 0 || all(is.na(x))) x <- rep(NA_real_, n_trans * nTransWeights)
    if (length(x) != n_trans * nTransWeights) {
      stop("Transition weights do not match transitions and weight names.", call. = FALSE)
    }
    matrix(
      x,
      nrow = n_trans,
      ncol = nTransWeights,
      byrow = TRUE,
      dimnames = list(NULL, mdp$weight_trans_names)
    )
  }
  actions <- dat$df %>%
    tidyr::unnest_longer("actions") %>% # convert actions (one row for each action)
    tidyr::unnest_wider("actions", names_repair = tidyr::tidyr_legacy) %>%
    dplyr::rename_with(~ dplyr::recode(.x, aIdx = "a_idx", transWeights = "trans_weights", .default = .x)) %>%
    dplyr::rename(state_label = "label") %>%
    dplyr::filter(!is.na(.data$a_idx)) %>%
    dplyr::mutate(
      action_weights = purrr::map(.data$weights, normalize_action_weights),
      trans = purrr::map(.data$trans, as.numeric),
      pr = purrr::map(.data$pr, as.numeric),
      trans_weights = purrr::map2(
        .data$trans_weights,
        .data$trans,
        ~ normalize_trans_weights(.x, length(.y))
      ),
      trans_labels = purrr::map(.data$trans, ~ rep(NA_character_, length(.x))),
      action_w_label = NA_character_,
      label = as.character(.data$label1)
    ) %>%
    dplyr::mutate(lwd = 1, lty = 1, col = "black") %>%
    dplyr::select(-"state_str", -"state_label", -"weights", -"label1")

  states <- dat$df %>%
    dplyr::mutate(g_id = NA_integer_) %>%
    dplyr::select("s_id", "state_str", "label", "g_id")
  if (mdp$time_horizon == Inf) {
    states$label[1:mdp$founder_states_last] <- states$label[(nrow(states) - mdp$founder_states_last + 1):nrow(states)]
  }

  return(list(nodes = states, hyperarcs = actions))
}
