# Functions for converting different file formats


#' Convert a HMDP model stored in a hmp (xml) file to binary file format.
#'
#' The function simply parse the hmp file and create binary files using
#' the [binary_mdp_writer()].
#'
#' @param file The name of the HMP file (e.g. `r.hmp`).
#' @param prefix A character string with the prefix which will be added to the binary files.
#' @param get_log Output log text.
#'
#' @return NULL (invisible).
#' @note Note all indexes are starting from zero (C/C++ style).
#' @seealso [binary_mdp_writer()].
#' @example inst/examples/convert-ex.R
#' @export
convert_hmp_to_binary <- function(file, prefix = "", get_log = TRUE) {
  set_weights <- function(labels) {
    ctr_w <<- length(labels) + 1
    w$set_weights(c("Duration", labels))
  }

  # state_ctr<-function(g) {
  #    browser()
  #    #xml2::xml_length(g)
  #    length(xmlChildren(g))
  # }

  # @param p A process node
  process <- function(p) {
    w$process()
    states <- c(xml2::xml_length(xml2::xml_children(p)), 0) # number of states in each stage (add 0 to indicate last stage)
    for (i in 1:(length(states) - 1)) stage(xml2::xml_child(p, i), states[i + 1])
    w$end_process()
  }

  # @param g A stage node
  # @param states Number of states at next stage
  stage <- function(g, states) {
    w$stage()
    r <- xml2::xml_children(g)
    # cat("states:\n"); print(r)
    for (i in 1:length(r)) state(r[i], states)
    w$end_stage()
  }

  # @param s A state node
  # @param states Number of states at next stage
  state <- function(s, states) {
    w$state(label = xml2::xml_attr(s, "l"))
    r <- xml2::xml_children(s)
    if (length(r) > 0) {
      # cat("actions:\n"); print(r)
      for (i in 1:length(r)) action(r[i], states)
    }
    w$end_state()
  }

  # trim spaces in both ends
  trim <- function(x) {
    sub("[ \t\n\r]*$", "", sub("^[ \t\n\r]*", "", x))
  }

  # @param a An action node
  # @param states Number of states at next stage
  action <- function(a, states) {
    if (length(xml2::xml_find_all(a, "proc")) > 0) { # if subprocess
      w$action(label = xml2::xml_attr(a, "l"), weights = rep(0, ctr_w), prob = c(2, 0, 1))
      process(xml2::xml_child(a))
    } else { # normal action
      v <- paste("c(", gsub(" +", ",", trim(xml2::xml_text(xml2::xml_child(a, "q")))), ")", sep = "")
      v <- eval(parse(text = v))
      d <- paste("c(", gsub(" +", ",", trim(xml2::xml_text(xml2::xml_child(a, "d")))), ")", sep = "")
      d <- eval(parse(text = d))
      if (length(d) > 1) warning("More than one duration number in the action (see hmp file)! \nOnly one duration for each action is supported in the binary file format. \nUse the first one.", call. = FALSE)
      v <- c(d[1], v)
      type <- xml2::xml_attr(xml2::xml_child(a, "p"), "t")
      pr <- paste("c(", gsub(" +", ",", trim(xml2::xml_text(xml2::xml_child(a, "p")))), ")", sep = "")
      pr <- eval(parse(text = pr))
      if (type == "s") {
        idx <- pr[1:length(pr) %% 2 == 1]
        pr <- pr[1:length(pr) %% 2 == 0]
        scp <- rep(1, length(pr)) # set scp to 1 (default)
      }
      if (type == "d") {
        idx <- pr[1]
        pr <- 1
        scp <- 1
      }
      if (type == "e") {
        idx <- 1:length(pr) - 1
        scp <- rep(1, length(pr)) # set scp to 1 (default)
      }
      if (is_hmdp) {
        for (i in 1:length(idx)) {
          if (idx[i] >= states) {
            scp[i] <- 0
            idx[i] <- idx[i] - states
          }
        }
      }
      i <- which(pr != 0)
      scp <- scp[i]
      idx <- idx[i]
      pr <- pr[i]
      pr <- as.numeric(rbind(scp, idx, pr))
      w$action(label = xml2::xml_attr(a, "l"), weights = v, prob = pr)
    }
    w$end_action()
  }

  ptm <- proc.time()
  ctr_w <- 0
  doc <- xml2::read_xml(file)
  is_hmdp <- xml2::xml_find_num(doc, xpath = "count(.//proc)") > 1 # ordinary MDP or HMDP
  w <- binary_mdp_writer(prefix, get_log = get_log)
  r <- xml2::xml_find_all(doc, "./quantities")
  set_weights(xml2::xml_attr(r, "l"))
  process(xml2::xml_child(doc, "proc"))
  w$close_writer()
  if (get_log) {
    cat("Converted", file, "to binary format.\n\n")
    print(proc.time() - ptm)
  }
  invisible(NULL)
}

#' Convert a HMDP model stored in binary format to a `hmp` (XML) file.
#' The function simply parse the binary files and create `hmp` files using
#' the [hmp_mdp_writer()].
#'
#' @param prefix A character string with the prefix which will be added to the binary files.
#' @param bin_names A character vector of length 7 giving the names of the binary files storing the model.
#' @param out The name of the HMP file (e.g. `r.hmp`).
#' @param duration Weight number storing the duration (NULL if none).
#' @param get_log Output log text.
#'
#' @return NULL (invisible).
#'
#' @note Note all indexes are starting from zero (C/C++ style).
#'
#' @seealso [convert_hmp_to_binary()].
#' @example inst/examples/convert-ex.R
#' @export
convert_binary_to_hmp <- function(prefix = "", bin_names = c(
                                    "stateIdx.bin", "stateIdxLbl.bin", "actionIdx.bin",
                                    "actionIdxLbl.bin", "actionWeight.bin", "actionWeightLbl.bin", "transProb.bin"
                                  ),
                                  out = paste0(prefix, "converted.hmp"), duration = 1, get_log = TRUE) {
  # mat: matrix of state index
  process <- function(mat) {
    # cat("process\n"); print(mat)
    stages <- length(unique(mat[, 2]))
    w$process()
    for (i in 1:stages - 1) {
      # print(i); print(nrow(mat[mat[,2]==i+1,]))
      stage(mat[mat[, 2] == i, , drop = FALSE], states_next = nrow(mat[mat[, 2] == i + 1, ]))
    }
    w$end_process()
  }

  stage <- function(mat, states_next) {
    # cat("stage\n"); print(mat)
    states <- length(unique(mat[, 3]))
    w$stage()
    for (i in 1:states - 1) {
      # print(i); print(nrow(mat[mat[,3]==i+1,]))
      state(mat[mat[, 3] == i, , drop = FALSE], states_next = states_next)
    }
    w$end_stage()
  }

  state <- function(mat, states_next) {
    # cat("state\n"); print(mat)
    # level1<-sum(!is.na(mat[1,2:ncol(mat)])) %/% 3    # level of the first state in mat
    matA <- a_idx[a_idx[, 2] == mat[1, 1], , drop = FALSE] # actions to the first state in mat
    if (nrow(mat) > 1) {
      a_idx <- unique(mat[!is.na(mat[, 4]), 4]) # actions that define child processes
      a_ctr <- 1
      # print(a_idx)
    }
    w$state(label = sLabels[sLabels[, 1] == mat[1, 1], 2]) # create state in hmp
    if (nrow(matA) > 0) {
      for (i in 1:nrow(matA)) { # scan actions
        # matSA<-mat[2:nrow(mat),]
        # cat("action\n"); print(matA[i,])
        scp <- matA[i, 3:ncol(matA)]
        scp <- idx <- scp[!is.na(scp)]
        scp <- scp[1:length(scp) %% 2 == 1]
        idx <- idx[1:length(idx) %% 2 == 0]
        weights <- aW[aW[, 1] == matA[i, 1], 1:w_lth + 1]
        # print(weights)
        if (any(scp == 2)) { # new process
          if (length(scp) > 1) {
            stop("Only a deterministic transition to sub process allowed for action (aId) ", matA[i, 1], "!")
          }
          if (idx[1] != 0) {
            stop("Only a deterministic transition to state 0 in sub process allowed for action (aId)", matA[i, 1], "!")
          }
          if (any(weights != 0)) {
            stop("Only zero weights allowed for transition to sub process, action (aId)", matA[i, 1], "!")
          }
          w$action(label = aLabels[aLabels[, 1] == matA[i, 1], 2], weights = c(0, 0, 0), prob = c(2, 0, 1))
          process(mat[mat[, 4] == a_idx[a_ctr] & !is.na(mat[, 4]), c(1, 5:ncol(mat))])
          a_ctr <- a_ctr + 1
          w$end_action()
        } else {
          pr <- prMat[prMat[, 1] == matA[i, 1], 2:ncol(prMat)]
          pr <- pr[!is.na(pr)]
          pr <- as.numeric(rbind(scp, idx, pr))
          # print(pr); print(states_next)
          w$action(label = aLabels[aLabels[, 1] == matA[i, 1], 2], weights = weights, prob = pr, states_next = states_next)
          w$end_action()
        }
      }
    }
    w$end_state()
  }

  ptm <- proc.time()
  s_idx <- state_idx_mat(prefix, bin_names[1])
  sLabels <- state_idx_df(prefix, bin_names[1])
  sLabels <- sLabels[, c(1, ncol(sLabels))]
  a_idx <- action_idx_mat(prefix, bin_names[3])
  aLabels <- action_idx_df(prefix, bin_names[3])
  aLabels <- aLabels[, c(1, ncol(aLabels))]
  aW <- action_weight_mat(prefix, bin_names[5], bin_names[6])
  prMat <- trans_prob_mat(prefix, bin_names[7])
  w_names <- weight_names(prefix, bin_names[6])
  w_lth <- length(w_names)
  # level<-sum(!is.na(s_idx[i,2:cols])) %/% 3
  w <- hmp_mdp_writer(file = out, desc = "HMP file created by converting binary files", get_log = get_log)
  w$set_weights(w_names, duration)
  process(s_idx)
  w$close_writer()
  if (get_log) {
    cat("Converted binary files to hmp format.\n")
    print(proc.time() - ptm)
  }
  invisible(NULL)
}


#' Info about the states in the HMDP model under consideration.
#'
#' @param prefix A character string with the prefix added to til file(s).
#' @param file The HMDP binary file containing the description under consideration.
#'
#' @return A matrix with columns `(s_id, n0, s0, a0, ...)` where
#' `s_id` is the state row id, `n0` the index of the stage at level 0, `s0` the index
#' of the state and `a0` the index of the action. If the HMDP has more
#' than one level columns index `(d1, s1, a1, ...)` are added.
#' @keywords internal
state_idx_mat <- function(prefix = "", file = "stateIdx.bin") {
  file <- paste(prefix, file, sep = "")
  tmp <- readBin(file, integer(), n = file.info(file)$size / 4)
  rows <- length(tmp[tmp == -1])
  cols <- max(rle(tmp != -1)$length)
  mat <- matrix(NA, nrow = rows, ncol = cols + 1)
  idx <- c(0, which(tmp == -1))
  for (i in 1:(length(idx) - 1)) mat[i, 1:(idx[i + 1] - idx[i] - 1) + 1] <- tmp[(idx[i] + 1):(idx[i + 1] - 1)]
  levels <- cols %/% 3 + 1
  if (levels == 1) colnames(mat) <- c("s_id", paste(c("n", "s"), levels - 1, sep = ""))
  if (levels > 1) colnames(mat) <- c("s_id", paste(c("n", "s", "a"), rep(0:(levels - 2), each = 3), sep = ""), paste(c("n", "s"), levels - 1, sep = ""))
  mat[, 1] <- 1:nrow(mat) - 1
  return(mat)
}


#' Info about the states in the HMDP model under consideration.
#'
#' @param prefix A character string with the prefix added to the file(s).
#' @param file The HMDP binary file containing the description under consideration.
#' @param labels The HMDP binary file containing the labels under consideration.
#'
#' @return A data frame with the same columns as in
#' `state_idx_mat` plus another column containing the labels.
#' @keywords internal
state_idx_df <- function(prefix = "", file = "stateIdx.bin", labels = "stateIdxLbl.bin") {
  labels <- paste(prefix, labels, sep = "")
  mat <- state_idx_mat(prefix, file)
  tmp <- readBin(labels, character(), n = file.info(labels)$size)
  tmp <- as.data.frame(matrix(tmp, ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
  colnames(tmp) <- c("s_id", "label")
  mat <- merge(mat, tmp, all.x = TRUE)
  return(mat)
}


#' Info about the transition probabilities in the HMDP model under consideration.
#'
#' @param prefix A character string with the prefix added to til file(s).
#' @param file The HMDP binary file containing the description under consideration.
#'
#' @return A matrix with columns (`aId`, ...) where
#' `aId` is the action row id and ... are the probabilities of the action.
#' @keywords internal
trans_prob_mat <- function(prefix = "", file = "transProb.bin") {
  file <- paste(prefix, file, sep = "")
  tmp <- readBin(file, numeric(), n = file.info(file)$size / 8)
  rows <- length(tmp[tmp == -1])
  cols <- max(rle(tmp != -1)$length)
  mat <- matrix(NA, nrow = rows, ncol = cols + 1)
  idx <- c(0, which(tmp == -1))
  for (i in 1:(length(idx) - 1)) mat[i, 1:(idx[i + 1] - idx[i] - 1) + 1] <- tmp[(idx[i] + 1):(idx[i + 1] - 1)]
  colnames(mat) <- c("aId", paste("pr", 1:(ncol(mat) - 1) - 1, sep = ""))
  mat[, 1] <- 1:nrow(mat) - 1
  return(mat)
}


#' Info about the actions in the HMDP model under consideration.
#'
#' @param prefix A character string with the prefix added to til file(s).
#' @param file The HMDP binary file containing the description under consideration.
#'
#' @return A matrix with columns (`aId`, ...) where
#' `aId` is the action row id and `...` are alternating pairs `(scp, idx)`, one for each
#' possible transition where `scp` is the scope that can be 4 values:
#' 2 - A transition to a child process (stage zero in the child process), 1 - A transition
#' to next stage in the current process, 0 - A transition to the next stage in the father
#' process. the idx in the pair denote the index of the state at the stage considered.
#' Finally, if scope equals 3 then a transition to the state with `s_id = idx` is considered.
#' @keywords internal
action_idx_mat <- function(prefix = "", file = "actionIdx.bin") {
  file <- paste(prefix, file, sep = "")
  tmp <- readBin(file, integer(), n = file.info(file)$size / 4)
  rows <- length(tmp[tmp == -1])
  cols <- max(rle(tmp != -1)$length)
  mat <- matrix(NA, nrow = rows, ncol = cols + 1)
  idx <- c(0, which(tmp == -1))
  for (i in 1:(length(idx) - 1)) mat[i, 1:(idx[i + 1] - idx[i] - 1) + 1] <- tmp[(idx[i] + 1):(idx[i + 1] - 1)]
  colnames(mat) <- c("aId", "s_id", paste(c("scp", "idx"), rep(1:((ncol(mat) - 2) / 2) - 1, each = 2), sep = ""))
  mat[, 1] <- 1:nrow(mat) - 1
  return(mat)
}


#' Info about the actions in the HMDP model under consideration.
#'
#' @param prefix A character string with the prefix added to til file(s).
#' @param file The HMDP binary file containing the description under consideration.
#' @param labels The HMDP binary file containing the labels under consideration.
#'
#' @return A data frame with the same columns as in
#' `action_idx_mat` plus another column containing the labels.
#' @keywords internal
action_idx_df <- function(prefix = "", file = "actionIdx.bin", labels = "actionIdxLbl.bin") {
  labels <- paste(prefix, labels, sep = "")
  mat <- action_idx_mat(prefix, file)
  tmp <- readBin(labels, character(), n = file.info(labels)$size)
  tmp <- as.data.frame(matrix(tmp, ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
  colnames(tmp) <- c("aId", "label")
  tmp$aId <- as.numeric(tmp$aId)
  mat <- merge(mat, tmp, all.x = TRUE)
  colnames(mat) <- c("aId", "s_id", paste(c("scp", "idx"), rep(1:((ncol(mat) - 2) / 2) - 1, each = 2), sep = ""), "label")
  return(mat)
}


#' Info about the actions in the HMDP model under consideration.
#'
#' @param prefix A character string with the prefix added to til file(s).
#' @param file The HMDP binary file containing the description under consideration.
#' @param labels The HMDP binary file containing the labels under consideration.
#' @param weight_file The HMDP binary file containing the action costs.
#' @param trans_pr_file The HMDP binary file containing the transition probabilities.
#'
#' @return A matrix with columns from `action_idx_mat`,
#' `actionCostMat` and `trans_prob_mat` if labels is NULL. If labels
#' not are NULL then a data frame are returned with a label column too.
#' @keywords internal
action_info <- function(prefix = "", file = "actionIdx.bin", weight_file = "actionWeight.bin", trans_pr_file = "transProb.bin", labels = "actionIdxLbl.bin") {
  labels <- paste(prefix, labels, sep = "")
  mat <- action_idx_mat(prefix, file)
  mat1 <- action_weight_mat(prefix, weight_file)
  mat <- merge(mat, mat1, all.x = TRUE)
  mat2 <- trans_prob_mat(prefix, trans_pr_file)
  mat <- merge(mat, mat2, all.x = TRUE)
  i <- (ncol(mat) - 2 - ncol(mat1) + 1) / 3 # number of idx used for (scp, idx, pr) triple
  mat <- mat[, c("aId", "s_id", colnames(mat1[, 2:ncol(mat1), drop = FALSE]), paste(c("scp", "idx", "pr"), rep(1:i - 1, each = 3), sep = ""))]
  if (!is.null(labels)) {
    tmp <- readBin(labels, character(), n = file.info(labels)$size)
    tmp <- as.data.frame(matrix(tmp, ncol = 2, byrow = TRUE))
    colnames(tmp) <- c("aId", "label")
    mat <- merge(mat, tmp, all.x = TRUE)
    return(mat)
  }
  mat <- as.matrix(mat)
  return(mat)
}


#' Info about the weights of the actions in the HMDP model under consideration.
#'
#' @param prefix A character string with the prefix added to til file(s).
#' @param file The HMDP binary file containing the description under consideration.
#' @param labels The HMDP binary file containing the labels under consideration.
#'
#' @return A matrix with columns (`aId`, ...) where
#' `aId` is the action row id and ... are the weights of the action.
#' @keywords internal
action_weight_mat <- function(prefix = "", file = "actionWeight.bin", labels = "actionWeightLbl.bin") {
  file <- paste(prefix, file, sep = "")
  labels <- paste(prefix, labels, sep = "")
  tmp <- readBin(file, numeric(), n = file.info(file)$size / 8)
  col_names <- readBin(labels, character(), n = file.info(labels)$size)
  cols <- length(col_names)
  rows <- length(tmp) / cols
  mat <- matrix(NA, nrow = rows, ncol = cols + 1)
  for (i in 1:rows) mat[i, 1:cols + 1] <- tmp[(cols * (i - 1) + 1):(cols * i)]
  # colnames(mat)<-c("aId",paste("w",1:(ncol(mat)-1)-1,sep=""))
  colnames(mat) <- c("aId", col_names)
  mat[, 1] <- 1:nrow(mat) - 1
  return(mat)
}


#' Names of weights used in actions.
#'
#' @param prefix A character string with the prefix added to the binary file names.
#' @param labels The HMDP binary file containing the weight labels.
#' @return Vector of weight names.
#' @keywords internal
weight_names <- function(prefix = "", labels = "actionWeightLbl.bin") {
  labels <- paste(prefix, labels, sep = "")
  col_names <- readBin(labels, character(), n = file.info(labels)$size)
  return(col_names)
}
