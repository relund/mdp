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
#' * `set_weights(labels, ...)`: sets the labels of the weights used in the actions.
#'   `labels` is a vector of label names. `...` is currently ignored. Call this
#'   before building the model.
#' * `process()`: starts a (sub)process. It may also be used to specify a
#'   traditional MDP using matrices in `MDPtoolbox` style. In that style, `p` is
#'   a list of matrices, one per action, each of size `$S x S$` where `$S$` is
#'   the number of states. Each used row must sum to one, or all entries in a
#'   row must be zero if unused. `r` is a matrix of size `$S x A$`, where `$A$`
#'   is the number of actions, and `d` is a matrix of size `$S x A$` with
#'   durations. If `d` is omitted, all durations are assumed to be 1.
#' * `end_process()`: ends a (sub)process.
#' * `stage(label = NULL)`: starts a stage. `label` is currently unused in the
#'   binary format.
#' * `end_stage()`: ends a stage.
#' * `state(label = NULL)`: starts a state and returns, invisibly, the state id.
#'   That id can later be referenced with scope 3.
#' * `end_state()`: ends a state.
#' * `action(scope = NULL, id = NULL, pr = NULL, prob = NULL, weights,
#'   trans_weights = NULL, label = NULL, end = FALSE, ...)`: starts an action.
#'   `weights` must be a vector of action weights. `trans_weights` must contain
#'   transition weights ordered by transition, with all transition weight labels
#'   for the first transition followed by all labels for the second transition,
#'   and so on. Transition probabilities can be entered in two ways:
#'
#'   1. `prob` contains triples `(scope, id, pr)`.
#'   2. `id` and `pr` are vectors of equal length. If `scope` is omitted, all
#'      scopes default to 1.
#'
#'   See the description of `actionIdx.bin` below. If `end = TRUE`, calling
#'   `end_action()` is not necessary. `...` is currently ignored.
#' * `end_action()`: ends an action. Do not use this if `end = TRUE` was used
#'   when the action was specified.
#' * `include_process(prefix, label = NULL, weights, prob, term_states,
#'   trans_weights = NULL)`: includes an external process. External processes are
#'   loaded into memory only when needed, which helps with large models. `prefix`
#'   is the external process prefix. `weights` must be a vector of action
#'   weights, and `prob` must contain triples `(scope, idx, pr)`; see the
#'   description of `actionIdx.bin` below. `term_states` must specify the number
#'   of states at the last stage in the external process. Inside an
#'   `include_process ... end_include_process`
#'   block, you must specify the father jump actions of the last stage in the
#'   external process. The external process is represented by its first and last
#'   stage together with its jump actions. The function returns, invisibly, the
#'   state ids of the first stage in the external process, which can later be
#'   referenced with scope 3.
#' * `end_include_process()`: ends an `include_process` block.
#' * `close_writer()`: closes the writer. Call this when the model description is
#'   finished.
#'
#' Ten binary files are created:
#'
#' * `stateIdx.bin`: integers defining all states in the format
#'   `"n0 s0 -1 n0 s0 a0 n1 s1 -1 n0 s0 a0 n1 s1 a1 n2 s2 -1 n0 s0 ..."`.
#'   Here `-1` indicates that a new state is considered.
#' * `stateIdxLbl.bin`: character data in the format `s_idx label s_idx label ...`.
#'   Here `s_idx` corresponds to the index or line number in `stateIdxLbl.bin`,
#'   starting from 0. No delimiter is used.
#' * `actionIdx.bin`: integers defining all actions in the format
#'   `s_idx scope idx scope idx scope idx -1 s_idx scope idx scope idx -1 s_idx scope -1 ...`.
#'   `s_idx` corresponds to the index or line number in `stateIdx.bin`, starting
#'   from 0. The following `(scope, idx)` pairs indicate possible transitions.
#'   Scope can take four values:
#'
#'   * `2`: a transition to a child process, at stage zero in the child process.
#'   * `1`: a transition to the next stage in the current process.
#'   * `0`: a transition to the next stage in the father process.
#'   * `3`: a transition to a state specified by its state `s_idx`.
#'
#'   For example, if `scope = 1` and `idx = 2`, the transition is to state
#'   number 3 at the next stage in the current process. If `scope = 3` and
#'   `idx = 5`, the transition is to the state specified at line 6 in
#'   `stateIdxLbl.bin`. This is useful when considering shared child processes.
#' * `actionIdxLbl.bin`: character data in the format `a_idx label a_idx label ...`.
#'   Here `a_idx` corresponds to the index or line number in `actionIdx.bin`,
#'   starting from 0. No delimiter is used.
#' * `actionWeight.bin`: doubles containing action weights in the format
#'   `"c1 c2 c3 c1 c2 c3 ..."`, assuming three weights for each action.
#' * `actionWeightLbl.bin`: character data containing the weight labels in the
#'   format `label1 label2 label3`, assuming three weights for each action.
#' * `transProb.bin`: doubles containing transition probabilities defined in
#'   `actionIdx.bin`. The format is `"p1 p2 p3 -1 p1 -1 p1 p2 -1 ..."`. Here
#'   `-1` indicates that a new action is considered.
#' * `externalProcesses.bin`: character data containing links to external
#'   processes in the format `stage_str prefix stage_str prefix ...`. Here
#'   `stage_str` corresponds to the stage index, for example `n0 s0 a0 n1`, of
#'   the stage corresponding to the first stage in the external process, and
#'   `prefix` is the external process prefix. No delimiter is used.
#' * `transWeight.bin`: doubles containing transition weights in the format
#'   `"t11 t12 t21 t22 -1 ..."`, assuming two transition weights for each
#'   transition and two transitions in the first action.
#' * `transWeightLbl.bin`: character data containing the transition weight
#'   labels.
#'
#' @param prefix A character string with the prefix added to `bin_names`.
#' @param bin_names A character vector giving the names of the binary files storing the model.
#' @param get_log Output log text.
#'
#' @return A list of functions.
#' @note Note all indexes are starting from zero (C/C++ style).
#' @example inst/examples/binary_mdp_writer-ex.R
#' @export
binary_mdp_writer <-
  function(prefix = "",
           bin_names = c(
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
           get_log = TRUE) {
    push_context <- function(value) {
      writer_context <<- c(writer_context, value)
      invisible(NULL)
    }

    pop_context <- function() {
      writer_context <<- writer_context[-length(writer_context)]
      invisible(NULL)
    }

    current_context <- function() {
      if (length(writer_context) == 0) {
        return(NULL)
      }
      writer_context[length(writer_context)]
    }

    require_context <- function(expected, message) {
      if (!identical(current_context(), expected)) stop(message, call. = FALSE)
      invisible(NULL)
    }

    set_weights <- function(labels, ...) {
      if (w_fixed) stop("Weights already added!")
      w_ctr <<- length(labels)
      writeBin(as.character(labels), fACostLbl)
      w_fixed <<- TRUE
      invisible(NULL)
    }

    set_trans_weights <- function(labels, ...) {
      if (t_w_fixed) stop("Transition weights already added!")
      t_w_ctr <<- length(labels)
      writeBin(as.character(labels), fTransWLbl)
      t_w_fixed <<- TRUE
      invisible(NULL)
    }

    process <- function(p = NULL, r = NULL, d = NULL, .from_include = FALSE) {
      if (!w_fixed) {
        stop("Weights must be added using 'set_weights' before starting building the HMDP!")
      }
      if (length(writer_context) > 0 && !identical(current_context(), "action")) {
        stop("Cannot start a process before closing the current writer block.", call. = FALSE)
      }
      if (.from_include) {
        require_context("action", "Cannot start an included process unless an include-process action is open.")
      }
      push_context("process")
      d_ctr <<- -1 # reset stage ctr
      s_idx <<- c(s_idx, NA)
      if (!is.null(p) & !is.null(r)) { # MDP specified using MDPtoolbox style
        if (is.null(d)) d <- matrix(1, nrow = nrow(r), ncol = ncol(r))
        stage()
        for (i in 1:nrow(r)) {
          state(label = i)
          for (j in 1:ncol(r)) {
            jIdx <- which(p[[j]][i, ] > 0)
            if (length(jIdx) == 0) next
            action(label = j, pr = p[[j]][i, jIdx], id = jIdx - 1, weights = c(d[i, j], r[i, j]), end = TRUE)
          }
          end_state()
        }
        end_stage()
        end_process()
      }
      invisible(NULL)
    }

    end_process <- function() {
      require_context("process", "Cannot end a process unless a process is open.")
      if (length(s_idx) > 1) s_idx <<- s_idx[1:(length(s_idx) - 1)] else s_idx <<- NULL
      # set ctr's for current level
      d_ctr <<- idx[length(idx) - 2]
      s_ctr <<- idx[length(idx) - 1]
      a_ctr <<- idx[length(idx)]
      pop_context()
      invisible(NULL)
    }

    stage <- function(label = NULL) {
      require_context("process", "Cannot start a stage outside an open process.")
      push_context("stage")
      d_ctr <<- d_ctr + 1
      s_ctr <<- -1 # reset state ctr
      idx <<- c(idx, d_ctr) # add stage idx
      # cat(paste("d:(",paste(c(idx),collapse=","),"),",d_ctr,"|",sep=""))
      invisible(NULL)
    }

    end_stage <- function() {
      require_context("stage", "Cannot end a stage unless a stage is open.")
      if (length(idx) > 1) idx <<- idx[1:(length(idx) - 1)] else idx <<- NULL # remove stage index
      # cat(paste("-d:(",paste(c(idx),collapse=","),"),",d_ctr,"|",sep=""))
      pop_context()
      invisible(NULL)
    }

    state <- function(label = NULL, end = FALSE) {
      require_context("stage", "Cannot start a state outside an open stage.")
      last_auto_closed_action <<- FALSE
      push_context("state")
      # cat("(",label,") ",sep="")
      s_ctr <<- s_ctr + 1
      a_ctr <<- -1 # reset action ctr
      idx <<- c(idx, s_ctr) # add state idx
      writeBin(as.integer(c(idx, -1)), fS)
      s_row_id <<- s_row_id + 1
      s_idx[length(s_idx)] <<- s_row_id
      # cat(paste("s:(",paste(c(idx),collapse=","),")",s_row_id,"|",sep=""))
      if (!is.null(label)) writeBin(c(as.character(s_row_id), label), fSLbl) # s_row_id added before label
      if (end) end_state()
      invisible(s_row_id)
    }

    end_state <- function() {
      require_context("state", "Cannot end a state while another writer block is open. Call end_action() or use action(..., end = TRUE) before end_state().")
      idx <<- idx[1:(length(idx) - 1)] # remove state index
      # cat(paste("-s:(",paste(c(idx),collapse=","),")|",sep=""))
      pop_context()
      invisible(NULL)
    }

    action <-
      function(scope = NULL,
               id = NULL,
               pr = NULL,
               prob = NULL,
               weights,
               trans_weights = NULL,
               label = NULL,
               end = FALSE,
               ...) {
        require_context("state", "Cannot start an action outside an open state.")
        last_auto_closed_action <<- FALSE
        push_context("action")
        # prop contain tripeles (scope,idx,prob)
        # cat("action:\n")
        # print(weights)
        # print(prob)
        # if (is.null(label) | label=="") stop("label = null");
        # if (length(weights)!=w_ctr) stop("Weight length must be ",w_ctr,"!")
        a_ctr <<- a_ctr + 1
        idx <<- c(idx, a_ctr) # add action idx
        # cat(paste("a:(",paste(c(idx),collapse=","),")|",sep=""))
        # cat(paste("a: s_id=",s_idx[length(s_idx)],"|",sep=""))
        scpIdx <- NULL
        aRowId <<- aRowId + 1
        if (!is.null(prob)) {
          for (i in 0:(length(prob) / 3 - 1)) scpIdx <- c(scpIdx, prob[1:2 + 3 * i])
          probs <- prob[1:(length(prob) / 3) * 3]
          writeBin(as.integer(c(s_idx[length(s_idx)], scpIdx, -1)), fA)
          writeBin(as.numeric(c(probs, -1)), fTransP)
          # cat("end action\n")
        } else if (!is.null(pr)) {
          # cat("pr:",paste0(pr,collapse = ",")," id:",paste0(id,collapse = ",")," w:",paste0(weights,collapse = ","),"\n"); cat
          if (is.null(scope)) scope <- rep(1, length(pr))
          i <- 1:length(pr) - 1
          scpIdx[1 + i * 2] <- scope
          scpIdx[2 + i * 2] <- id
          writeBin(as.integer(c(s_idx[length(s_idx)], scpIdx, -1)), fA)
          writeBin(as.numeric(c(pr, -1)), fTransP)
        }
        n_trans <- length(scpIdx) / 2
        if (t_w_ctr > 0) {
          if (is.null(trans_weights)) trans_weights <- rep(0, n_trans * t_w_ctr)
          if (length(trans_weights) != n_trans * t_w_ctr) {
            stop("trans_weights must have length number of transitions times number of transition weights.")
          }
          writeBin(as.numeric(c(trans_weights, -1)), fTransW)
        }
        writeBin(as.numeric(weights), fACost)
        if (!is.null(label)) writeBin(c(as.character(aRowId), label), fALbl) # aRowId added before label
        if (end) {
          end_action()
          last_auto_closed_action <<- TRUE
        }
        invisible(NULL)
      }

    end_action <- function() {
      if (!identical(current_context(), "action") && identical(current_context(), "state") && last_auto_closed_action) {
        last_auto_closed_action <<- FALSE
        return(invisible(NULL))
      }
      require_context("action", "Cannot end an action unless an action is open.")
      idx <<- idx[1:(length(idx) - 1)] # remove action index
      # cat(paste("-a:(",paste(c(idx),collapse=","),")|",sep=""))
      pop_context()
      last_auto_closed_action <<- FALSE
      invisible(NULL)
    }

    include_process <- function(prefix, label = NULL, weights, prob, term_states, trans_weights = NULL) { # prop contain tripeles (scope,idx,prob) - Here all scope must be 2!!
      require_context("state", "Cannot include a process outside an open state.")
      push_context("action")
      stateId <- NULL # to store state id's
      # cat("action:\n")
      # print(weights)
      # print(prob)
      # if (is.null(label) | label=="") stop("label = null");
      # if (length(weights)!=w_ctr) stop("Weight length must be ",w_ctr,"!")
      a_ctr <<- a_ctr + 1
      idx <<- c(idx, a_ctr) # add action idx
      # cat(paste("a:(",paste(c(idx),collapse=","),")|",sep=""))
      # cat(paste("a: s_id=",s_idx[length(s_idx)],"|",sep=""))
      scpIdx <- NULL
      aRowId <<- aRowId + 1
      for (i in 0:(length(prob) / 3 - 1)) scpIdx <- c(scpIdx, prob[1:2 + 3 * i])
      probs <- prob[1:(length(prob) / 3) * 3]
      #        if (any(scpIdx<0) | any(probs<0)) {
      #            print(label)
      #            print(prob)
      #            print(scpIdx)
      #            print(probs)
      #            stop()
      #        }
      writeBin(as.integer(c(s_idx[length(s_idx)], scpIdx, -1)), fA)
      if (!is.null(label)) writeBin(c(as.character(aRowId), label), fALbl) # aRowId added before label
      writeBin(as.numeric(c(probs, -1)), fTransP)
      if (t_w_ctr > 0) {
        n_trans <- length(scpIdx) / 2
        if (is.null(trans_weights)) trans_weights <- rep(0, n_trans * t_w_ctr)
        if (length(trans_weights) != n_trans * t_w_ctr) {
          stop("trans_weights must have length number of transitions times number of transition weights.")
        }
        writeBin(as.numeric(c(trans_weights, -1)), fTransW)
      }
      writeBin(as.numeric(weights), fACost)
      # cat("end action\n")
      maxId <- max(scpIdx[2 * (1:(length(scpIdx) / 2))]) # number of states to create at the first stage of the child
      process(.from_include = TRUE) # start external subprocess
      stage() # first stage of the external process
      writeBin(c(paste(idx, collapse = ","), prefix), fExt) # store the external process' name
      pr <- as.numeric(t(matrix(c(rep(1, term_states), 1:term_states - 1, rep(1 / term_states, term_states)), ncol = 3)))
      for (i in 0:maxId) {
        # create the states in the first stage (with no actions)
        stateId <- c(stateId, state())
        action(
          weights = rep(0, length(weights)),
          prob = pr,
          end = TRUE
        ) # dummy action of external process with transition to all terminal states
        end_state()
      }
      end_stage()
      # now the user has to include the last stage using the normal syntax
      invisible(stateId)
    }

    end_include_process <- function() {
      end_process() # end external subprocess
      require_context("action", "Cannot end an included process unless an include-process action is open.")
      idx <<- idx[1:(length(idx) - 1)] # remove action index
      # cat(paste("-a:(",paste(c(idx),collapse=","),")|",sep=""))
      pop_context()
      last_auto_closed_action <<- FALSE
      invisible(NULL)
    }

    close_writer <- function() {
      if (length(writer_context) > 0) {
        stop(
          paste0("Cannot close writer while a ", current_context(), " is still open."),
          call. = FALSE
        )
      }
      if (get_log) {
        cat("\n  Statistics:\n")
        cat("    states :", s_row_id + 1, "\n")
        cat("    actions:", aRowId + 1, "\n")
        cat("    weights:", w_ctr, "\n\n")
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

    bin_names <- paste(prefix, bin_names, sep = "")
    fS <- file(bin_names[1], "wb")
    fSLbl <- file(bin_names[2], "wb")
    fA <- file(bin_names[3], "wb")
    fALbl <- file(bin_names[4], "wb")
    fACost <- file(bin_names[5], "wb")
    fACostLbl <- file(bin_names[6], "wb")
    fTransP <- file(bin_names[7], "wb")
    fExt <- file(bin_names[8], "wb")
    fTransW <- file(bin_names[9], "wb")
    fTransWLbl <- file(bin_names[10], "wb")
    idx <- NULL # containing the stage, state or action idx's
    s_idx <- NULL # containing the state row id's (used to find the state id the action is defined under)
    d_ctr <- -1 # current stage at current level
    s_ctr <- -1 # current state at current stage
    a_ctr <- -1 # current action at current state
    w_ctr <- 0 # number of weights in the model
    t_w_ctr <- 0 # number of transition weights in the model
    s_row_id <- -1 # current row/line of state in stateIdx file
    aRowId <- -1 # current row/line of action in action_idx file
    w_fixed <- FALSE # TRUE if size of weights are fixed
    t_w_fixed <- FALSE # TRUE if size of transition weights are fixed
    writer_context <- character()
    last_auto_closed_action <- FALSE
    v <-
      list(
        set_weights = set_weights,
        set_trans_weights = set_trans_weights,
        stage = stage,
        end_stage = end_stage,
        state = state,
        end_state = end_state,
        action = action,
        end_action = end_action,
        include_process = include_process,
        end_include_process = end_include_process,
        process = process,
        end_process = end_process,
        close_writer = close_writer
      )
    class(v) <- c("binary_mdp_writer")
    return(v)
  }

#' Function for building an HMDP model directly in memory.
#'
#' `memory_mdp_writer()` defines the same main sub-functions as
#' [binary_mdp_writer()], but stores states and actions directly in C++ memory
#' instead of writing intermediate binary files. `close_writer()` compiles the
#' model and returns the loaded `"HMDP"` object.
#'
#' External or included processes are not supported by `memory_mdp_writer()`.
#'
#' @param prefix A character string kept for compatibility and stored in the
#'   returned object metadata.
#' @param eps The sum of transition probabilities must at most differ `eps`
#'   from one when `check = TRUE`.
#' @param check Check if the MDP seems correct before returning it.
#' @param verbose More output when compiling and running algorithms.
#' @param get_log Output the log messages.
#' @return A list of functions. Calling `close_writer()` returns an `"HMDP"`
#'   object.
#' @note Note all indexes are starting from zero (C/C++ style).
#' @example inst/examples/memory_mdp_writer-ex.R
#' @export
memory_mdp_writer <- function(prefix = "",
                              eps = 0.00001,
                              check = TRUE,
                              verbose = FALSE,
                              get_log = TRUE) {
  if (!is.logical(verbose)) verbose <- FALSE
  builder <- methods::new(HMDPBuilder, verbose)
  closed <- FALSE

  assert_open <- function() {
    if (closed) stop("memory_mdp_writer is closed.", call. = FALSE)
    invisible(NULL)
  }

  push_context <- function(value) {
    writer_context <<- c(writer_context, value)
    invisible(NULL)
  }

  pop_context <- function() {
    writer_context <<- writer_context[-length(writer_context)]
    invisible(NULL)
  }

  current_context <- function() {
    if (length(writer_context) == 0) {
      return(NULL)
    }
    writer_context[length(writer_context)]
  }

  require_context <- function(expected, message) {
    if (!identical(current_context(), expected)) stop(message, call. = FALSE)
    invisible(NULL)
  }

  set_weights <- function(labels, ...) {
    assert_open()
    if (w_fixed) stop("Weights already added!")
    w_ctr <<- length(labels)
    builder$setWeights(as.character(labels))
    w_fixed <<- TRUE
    invisible(NULL)
  }

  set_trans_weights <- function(labels, ...) {
    assert_open()
    if (t_w_fixed) stop("Transition weights already added!")
    t_w_ctr <<- length(labels)
    builder$setTransWeights(as.character(labels))
    t_w_fixed <<- TRUE
    invisible(NULL)
  }

  process <- function(p = NULL, r = NULL, d = NULL, .from_include = FALSE) {
    assert_open()
    if (!w_fixed) {
      stop("Weights must be added using 'set_weights' before starting building the HMDP!")
    }
    if (.from_include) {
      stop("memory_mdp_writer() does not support external processes.", call. = FALSE)
    }
    if (length(writer_context) > 0 && !identical(current_context(), "action")) {
      stop("Cannot start a process before closing the current writer block.", call. = FALSE)
    }
    push_context("process")
    d_ctr <<- -1
    s_idx <<- c(s_idx, NA)
    if (!is.null(p) & !is.null(r)) {
      if (is.null(d)) d <- matrix(1, nrow = nrow(r), ncol = ncol(r))
      stage()
      for (i in 1:nrow(r)) {
        state(label = i)
        for (j in 1:ncol(r)) {
          jIdx <- which(p[[j]][i, ] > 0)
          if (length(jIdx) == 0) next
          action(
            label = j, pr = p[[j]][i, jIdx], id = jIdx - 1,
            weights = c(d[i, j], r[i, j]), end = TRUE
          )
        }
        end_state()
      }
      end_stage()
      end_process()
    }
    invisible(NULL)
  }

  end_process <- function() {
    assert_open()
    require_context("process", "Cannot end a process unless a process is open.")
    if (length(s_idx) > 1) s_idx <<- s_idx[1:(length(s_idx) - 1)] else s_idx <<- NULL
    d_ctr <<- idx[length(idx) - 2]
    s_ctr <<- idx[length(idx) - 1]
    a_ctr <<- idx[length(idx)]
    pop_context()
    invisible(NULL)
  }

  stage <- function(label = NULL) {
    assert_open()
    require_context("process", "Cannot start a stage outside an open process.")
    push_context("stage")
    d_ctr <<- d_ctr + 1
    s_ctr <<- -1
    idx <<- c(idx, d_ctr)
    invisible(NULL)
  }

  end_stage <- function() {
    assert_open()
    require_context("stage", "Cannot end a stage unless a stage is open.")
    if (length(idx) > 1) idx <<- idx[1:(length(idx) - 1)] else idx <<- NULL
    pop_context()
    invisible(NULL)
  }

  state <- function(label = NULL, end = FALSE) {
    assert_open()
    require_context("stage", "Cannot start a state outside an open stage.")
    last_auto_closed_action <<- FALSE
    push_context("state")
    s_ctr <<- s_ctr + 1
    a_ctr <<- -1
    idx <<- c(idx, s_ctr)
    s_row_id <<- builder$addState(as.integer(idx), if (is.null(label)) "" else as.character(label))
    s_idx[length(s_idx)] <<- s_row_id
    if (end) end_state()
    invisible(s_row_id)
  }

  end_state <- function() {
    assert_open()
    require_context("state", "Cannot end a state while another writer block is open. Call end_action() or use action(..., end = TRUE) before end_state().")
    idx <<- idx[1:(length(idx) - 1)]
    pop_context()
    invisible(NULL)
  }

  action <- function(scope = NULL,
                     id = NULL,
                     pr = NULL,
                     prob = NULL,
                     weights,
                     trans_weights = NULL,
                     label = NULL,
                     end = FALSE,
                     ...) {
    assert_open()
    require_context("state", "Cannot start an action outside an open state.")
    last_auto_closed_action <<- FALSE
    push_context("action")
    a_ctr <<- a_ctr + 1
    idx <<- c(idx, a_ctr)
    aRowId <<- aRowId + 1
    scpIdx <- NULL
    probs <- NULL
    if (!is.null(prob)) {
      for (i in 0:(length(prob) / 3 - 1)) scpIdx <- c(scpIdx, prob[1:2 + 3 * i])
      probs <- prob[1:(length(prob) / 3) * 3]
    } else if (!is.null(pr)) {
      if (is.null(scope)) scope <- rep(1, length(pr))
      i <- 1:length(pr) - 1
      scpIdx[1 + i * 2] <- scope
      scpIdx[2 + i * 2] <- id
      probs <- pr
    } else {
      stop("Either 'pr' or 'prob' must be provided.", call. = FALSE)
    }
    n_trans <- length(scpIdx) / 2
    if (n_trans == 0) stop("An action must define at least one transition.", call. = FALSE)
    if (t_w_ctr > 0) {
      if (is.null(trans_weights)) trans_weights <- rep(0, n_trans * t_w_ctr)
      if (length(trans_weights) != n_trans * t_w_ctr) {
        stop("trans_weights must have length number of transitions times number of transition weights.")
      }
    } else {
      trans_weights <- numeric()
    }
    builder$addAction(
      as.integer(s_idx[length(s_idx)]),
      as.integer(scpIdx[2 * (1:n_trans) - 1]),
      as.integer(scpIdx[2 * (1:n_trans)]),
      as.numeric(probs),
      as.numeric(weights),
      as.numeric(trans_weights),
      if (is.null(label)) "" else as.character(label)
    )
    if (end) {
      end_action()
      last_auto_closed_action <<- TRUE
    }
    invisible(NULL)
  }

  end_action <- function() {
    assert_open()
    if (!identical(current_context(), "action") && identical(current_context(), "state") && last_auto_closed_action) {
      last_auto_closed_action <<- FALSE
      return(invisible(NULL))
    }
    require_context("action", "Cannot end an action unless an action is open.")
    idx <<- idx[1:(length(idx) - 1)]
    pop_context()
    last_auto_closed_action <<- FALSE
    invisible(NULL)
  }

  include_process <- function(...) {
    assert_open()
    stop("memory_mdp_writer() does not support external processes.", call. = FALSE)
  }

  end_include_process <- function(...) {
    assert_open()
    stop("memory_mdp_writer() does not support external processes.", call. = FALSE)
  }

  close_writer <- function() {
    assert_open()
    if (length(writer_context) > 0) {
      stop(
        paste0("Cannot close writer while a ", current_context(), " is still open."),
        call. = FALSE
      )
    }
    mdpPtr <- builder$close()
    closed <<- TRUE
    builder <<- NULL
    if (get_log) {
      cat("\n  Statistics:\n")
      cat("    states :", s_row_id + 1, "\n")
      cat("    actions:", aRowId + 1, "\n")
      cat("    weights:", w_ctr, "\n\n")
      cat("  Closing memory MDP writer.\n\n")
    }
    .make_mdp_list(
      mdpPtr,
      bin_names = paste0(prefix, "<memory>"),
      eps = eps,
      check = check,
      get_log = get_log
    )
  }

  idx <- NULL
  s_idx <- NULL
  d_ctr <- -1
  s_ctr <- -1
  a_ctr <- -1
  w_ctr <- 0
  t_w_ctr <- 0
  s_row_id <- -1
  aRowId <- -1
  w_fixed <- FALSE
  t_w_fixed <- FALSE
  writer_context <- character()
  last_auto_closed_action <- FALSE
  v <- list(
    set_weights = set_weights,
    set_trans_weights = set_trans_weights,
    stage = stage,
    end_stage = end_stage,
    state = state,
    end_state = end_state,
    action = action,
    end_action = end_action,
    include_process = include_process,
    end_include_process = end_include_process,
    process = process,
    end_process = end_process,
    close_writer = close_writer
  )
  class(v) <- c("memory_mdp_writer")
  return(v)
}


#' Function for writing actions of a HMDP model to binary files. The function defines
#' sub-functions which can be used to define actions saved in a set of binary
#' files. It is assumed that the states have been defined using `binary_mdp_writer`
#' and that the id of the states is known (can be retrieved using e.g. `state_idx_df`).
#'
#' Binary files are efficient for storing large models. Compared to the HMP (XML)
#' format the binary files use less storage space and loading the model is faster.
#'
#' The returned writer exposes these functions:
#'
#' * `set_weights(labels, ...)`: sets the labels of the weights used in the
#'   actions. `labels` is a vector of label names. `...` is currently ignored.
#'   Call this before building the model.
#' * `add_action(label = NULL, s_idx, weights, prob, ...)`: adds an action. `s_idx`
#'   is the id of the state defining the action. `weights` must be a vector of
#'   action weights. `prob` is a matrix `(s_idx, pr)` where the first column
#'   contains the id of the transition state; see the description of
#'   `actionIdx.bin` below, where scope is assumed to be 3. `...` is currently
#'   ignored.
#' * `end_action()`: ends an action.
#' * `close_writer()`: closes the writer. Call this when the model description is
#'   finished.
#'
#' Five binary files are created:
#'
#' * `actionIdx.bin`: integers defining all actions in the format
#'   `s_idx scope idx scope idx scope idx -1 s_idx scope idx scope idx -1 s_idx scope -1 ...`.
#'   `s_idx` corresponds to the index or line number in `stateIdx.bin`, starting
#'   from 0. The following `(scope, idx)` pairs indicate possible transitions.
#'   Scope can take four values:
#'
#'   * `2`: a transition to a child process, at stage zero in the child process.
#'   * `1`: a transition to the next stage in the current process.
#'   * `0`: a transition to the next stage in the father process.
#'   * `3`: a transition to a state specified by its state `s_idx`.
#'
#'   For example, if `scope = 1` and `idx = 2`, the transition is to state
#'   number 3 at the next stage in the current process. If `scope = 3` and
#'   `idx = 5`, the transition is to the state specified at line 6 in
#'   `stateIdxLbl.bin`. This is useful when considering shared child processes.
#' * `actionIdxLbl.bin`: character data in the format `a_idx label a_idx label ...`.
#'   Here `a_idx` corresponds to the index or line number in `actionIdx.bin`,
#'   starting from 0. No delimiter is used.
#' * `actionWeight.bin`: doubles containing action weights in the format
#'   `"c1 c2 c3 c1 c2 c3 ..."`, assuming three weights for each action.
#' * `actionWeightLbl.bin`: character data containing the weight labels in the
#'   format `label1 label2 label3`, assuming three weights for each action.
#' * `transProb.bin`: doubles containing the transition probabilities defined in
#'   `actionIdx.bin`. The format is `"p1 p2 p3 -1 p1 -1 p1 p2 -1 ..."`. Here
#'   `-1` indicates that a new action is considered.
#'
#' @param prefix A character string with the prefix added to `bin_names`.
#' @param bin_names A character vector of length 5 giving the names of the binary
#'     files storing the model.
#' @param append Logical indicating whether should keep the currents actions (default - TRUE)
#' defined or delete them and start over (FALSE).
#' @return A list of functions.
#' @note Note all indexes are starting from zero (C/C++ style).
#' @example inst/examples/binary_mdp_writer-ex.R
#' @export
binary_action_writer <- function(prefix = "",
                                 bin_names = c(
                                   "actionIdx.bin",
                                   "actionIdxLbl.bin",
                                   "actionWeight.bin",
                                   "actionWeightLbl.bin",
                                   "transProb.bin",
                                   "transWeight.bin",
                                   "transWeightLbl.bin"
                                 ),
                                 append = TRUE) {
  set_weights <- function(labels, ...) {
    if (w_fixed) stop("Weights already added!")
    w_ctr <<- length(labels)
    writeBin(as.character(labels), fACostLbl)
    w_fixed <<- TRUE
    invisible(NULL)
  }

  set_trans_weights <- function(labels, ...) {
    if (t_w_fixed) stop("Transition weights already added!")
    t_w_ctr <<- length(labels)
    writeBin(as.character(labels), fTransWLbl)
    t_w_fixed <<- TRUE
    invisible(NULL)
  }

  add_action <- function(label = NULL, s_idx, weights, prob, trans_weights = NULL, ...) { # do not hold now: prop is a matrix with columns (id_s,prob)
    # 		cat("action:\n")
    # 		print(weights)
    # 		print(prob)
    # 		if (length(weights)!=w_ctr) stop("Weight length must be ",w_ctr,"!")
    # cat(paste("a:(",paste(c(idx),collapse=","),")|",sep=""))
    # cat(paste("a: s_id=",s_idx[length(s_idx)],"|",sep=""))
    aRowId <<- aRowId + 1
    scpIdx <- NULL
    for (i in 0:(length(prob) / 3 - 1)) scpIdx <- c(scpIdx, prob[1:2 + 3 * i])
    probs <- prob[1:(length(prob) / 3) * 3]
    writeBin(as.integer(c(s_idx, scpIdx, -1)), fA)
    if (!is.null(label)) writeBin(c(as.character(aRowId), label), fALbl) # aRowId added before label
    writeBin(as.numeric(c(probs, -1)), fTransP)
    if (t_w_ctr > 0) {
      n_trans <- length(scpIdx) / 2
      if (is.null(trans_weights)) trans_weights <- rep(0, n_trans * t_w_ctr)
      if (length(trans_weights) != n_trans * t_w_ctr) {
        stop("trans_weights must have length number of transitions times number of transition weights.")
      }
      writeBin(as.numeric(c(trans_weights, -1)), fTransW)
    }
    writeBin(as.numeric(weights), fACost)
    # cat("end action\n")
    invisible(NULL)
  }

  close_writer <- function() {
    if (!w_fixed) stop("Weights must be added using 'set_weights'!")
    cat("\n  Statistics:\n")
    cat("    actions:", aRowId + 1, "\n")
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

  bin_names <- paste(prefix, bin_names, sep = "")
  if (append) {
    # find number of actions already written
    tmp <- readBin(bin_names[1], integer(), n = file.info(bin_names[1])$size / 4)
    aRowId <- length(tmp[tmp == -1]) - 1 # current number of actions defined
    w_fixed <- TRUE # TRUE if size of weights are fixed
    t_w_fixed <- TRUE
    t_w_ctr <- 0
  } else {
    aRowId <- -1 # current row/line of action in action_idx file
    w_ctr <- 0 # number of weights in the model
    t_w_ctr <- 0
    w_fixed <- FALSE # TRUE if size of weights are fixed
    t_w_fixed <- FALSE
  }
  mode <- ifelse(append, "ab", "wb")
  fA <- file(bin_names[1], mode)
  fALbl <- file(bin_names[2], mode)
  fACost <- file(bin_names[3], mode)
  fACostLbl <- file(bin_names[4], mode)
  fTransP <- file(bin_names[5], mode)
  fTransW <- file(bin_names[6], mode)
  fTransWLbl <- file(bin_names[7], mode)
  v <- list(set_weights = set_weights, set_trans_weights = set_trans_weights, add_action = add_action, close_writer = close_writer)
  class(v) <- c("binary_action_writer")
  return(v)
}


#' Info about the states in the binary files of the HMDP model under consideration.
#'
#' @param prefix A character string with the prefix added to til binary files.
#' @param labels Should labels be extracted.
#' @param state_str Should state strings be extracted. If false then add columns (n0, s0, a0, ...)
#'   where n0 the index of the stage at level 0, s0 the index of the state and a0 the index of the
#'   action. If the HMDP has more than one level columns index (d1, s1, a1, ...) are added.
#' @param file_s The binary file containing the description of states.
#' @param label_s The binary file containing the state labels.
#'
#' @note The model don't have to be loaded, i.e only read the binary files. The state id (`s_id`) will
#' not be the same as in the loaded model!
#'
#' @return A data frame with the information.
#' @export
get_bin_info_states <-
  function(prefix = "",
           labels = TRUE,
           state_str = TRUE,
           file_s = "stateIdx.bin",
           label_s = "stateIdxLbl.bin") {
    file_s <- paste(prefix, file_s, sep = "")
    tmp <- readBin(file_s, integer(), n = file.info(file_s)$size / 4)
    rows <- length(tmp[tmp == -1])
    if (!state_str) {
      cols <- max(rle(tmp != -1)$length)
      mat <- as.data.frame(matrix(NA, nrow = rows, ncol = cols + 1))
      idx <- c(0, which(tmp == -1))
      for (i in 1:(length(idx) - 1)) {
        mat[i, 1:(idx[i + 1] - idx[i] - 1) + 1] <-
          tmp[(idx[i] + 1):(idx[i + 1] - 1)]
      }
      levels <- cols %/% 3 + 1
      if (levels == 1) {
        colnames(mat) <- c("s_id", paste(c("n", "s"), levels - 1, sep = ""))
      }
      if (levels > 1) {
        colnames(mat) <-
          c("s_id", paste(c("n", "s", "a"), rep(0:(levels - 2), each = 3), sep = ""), paste(c("n", "s"), levels -
            1, sep = ""))
      }
    } else {
      mat <- as.data.frame(matrix(NA, nrow = rows, ncol = 2))
      idx <- c(0, which(tmp == -1))
      for (i in 1:(length(idx) - 1)) {
        mat[i, 2] <- paste(tmp[(idx[i] + 1):(idx[i + 1] - 1)], collapse = ",")
      }
      colnames(mat) <- c("s_id", "stage_str")
    }
    mat[, 1] <- 1:nrow(mat) - 1
    if (labels) {
      label_s <- paste(prefix, label_s, sep = "")
      tmp <- readBin(label_s, character(), n = file.info(label_s)$size)
      tmp <-
        as.data.frame(matrix(tmp, ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
      colnames(tmp) <- c("s_id", "label")
      mat <- merge(mat, tmp, all.x = TRUE)
    }
    return(dplyr::as_tibble(mat))
  }


#' Info about the actions in the HMDP model under consideration.
#'
#' @param prefix A character string with the prefix added to til binary files.
#' @param labels Should labels be extracted.
#' @param file_a The binary file containing the description of actions.
#' @param file_pr The binary file containing the description of transition probabilities.
#' @param file_w The binary file containing the description of weights.
#' @param file_label_a The binary file containing the action labels.
#' @param file_label_w The binary file containing the weight labels.
#'
#' @return A data frame with the information. Scope string contain the scope of the transitions and
#'   can be 4 values:
#'   * 0: A transition to the next stage in the father process,
#'   * 1: A transition to next stage in the current process,
#'   * 2: A transition to a child process (stage zero in the child process),
#'   * 3: A transition to the state with `s_id = idx` is considered.
#'
#'   The index string denote the index (id is scope = 3) of the state at the next stage.
#'
#' @note The model don't have to be loaded, i.e only read the binary files. The state id (`s_id`) will
#'   not be the same as in the loaded model!
#' @example inst/examples/binary_mdp_writer-ex.R
#' @export
get_bin_info_actions <- function(prefix = "", labels = TRUE, file_a = "actionIdx.bin",
                                 file_pr = "transProb.bin", file_w = "actionWeight.bin",
                                 file_label_w = "actionWeightLbl.bin", file_label_a = "actionIdxLbl.bin") {
  file_a <- paste(prefix, file_a, sep = "")
  file_pr <- paste(prefix, file_pr, sep = "")
  file_w <- paste(prefix, file_w, sep = "")
  file_label_w <- paste(prefix, file_label_w, sep = "")

  tmpA <- readBin(file_a, integer(), n = file.info(file_a)$size / 4)
  tmpPr <- readBin(file_pr, numeric(), n = file.info(file_pr)$size / 8)
  tmpW <- readBin(file_w, numeric(), n = file.info(file_w)$size / 8)
  col_names <- readBin(file_label_w, character(), n = file.info(file_label_w)$size)
  rows <- length(tmpA[tmpA == -1])
  cols <- 5 + length(col_names)

  mat <- as.data.frame(matrix(NA, nrow = rows, ncol = cols))
  mat[, 1] <- 1:nrow(mat) - 1
  idxA <- c(0, which(tmpA == -1))
  idxPr <- c(0, which(tmpPr == -1))
  for (i in 1:(length(idxA) - 1)) {
    v <- tmpA[(idxA[i] + 1):(idxA[i + 1] - 1)]
    mat[i, 2] <- v[1]
    mat[i, 3] <- paste(v[seq(2, length(v), 2)], collapse = ",")
    mat[i, 4] <- paste(v[seq(3, length(v), 2)], collapse = ",")
    v <- tmpPr[(idxPr[i] + 1):(idxPr[i + 1] - 1)]
    mat[i, 5] <- paste(v, collapse = ",")
  }

  for (i in 1:rows) {
    mat[i, 6:cols] <- tmpW[(length(col_names) * (i - 1) + 1):(length(col_names) * i)]
  }
  colnames(mat) <- c("aId", "s_id", "scope", "index", "pr", col_names)

  if (labels) {
    file_label_a <- paste(prefix, file_label_a, sep = "")
    tmp <- readBin(file_label_a, character(), n = file.info(file_label_a)$size)
    tmp <- as.data.frame(matrix(tmp, ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
    colnames(tmp) <- c("aId", "label")
    tmp$aId <- as.numeric(tmp$aId)
    mat <- merge(mat, tmp, all.x = TRUE)
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
#' * `set_weights(labels, duration)`: sets the labels of the weights used in the
#'   actions. `labels` is a vector of label names. `duration` identifies which
#'   label corresponds to duration or time. For example, if the first entry in
#'   `labels` is time, then `duration = 1`. Call this before building the model.
#' * `set_trans_weights(labels)`: sets the labels of transition-level weights.
#' * `process()`: starts a (sub)process.
#' * `end_process()`: ends a (sub)process.
#' * `stage(label = NULL)`: starts a stage.
#' * `end_stage()`: ends a stage.
#' * `state(label = NULL)`: starts a state and returns the state index `s_idx`.
#' * `end_state()`: ends a state.
#' * `action(label = NULL, weights, prob, states_next = NULL, trans_weights = NULL)`: starts an
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
#'   is not supported in the `hmp` file format. `states_next` is the number of
#'   states in the next stage of the process and is only needed when there is a
#'   transition to the father.
#' * `end_action()`: ends an action.
#' * `close_writer()`: closes the writer. Call this when the model description is
#'   finished.
#'
#' @param file The name of the file storing the model (e.g. `r.hmp`).
#' @param rate The interest rate (used if consider discounting).
#' @param rate_base The time where the `rate` is taken over, e.g. if the `rate` is 0.1 and `rate_base` is 365 days
#'   then we have an interest rate of 10 percent over the year.
#' @param precision The precision used when checking if probabilities sum to one.
#' @param desc Description of the model.
#' @param get_log Output log text.
#' @return A list of functions.
#' @note Note all indexes are starting from zero (C/C++ style).
#' @example inst/examples/hmp_mdp_writer-ex.R
#' @export
hmp_mdp_writer <- function(file = "r.hmp", rate = 0.1, rate_base = 1, precision = 0.00001, desc = "HMP file created using hmp_mdp_writer in R", get_log = TRUE) {
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

  set_weights <- function(labels, duration) {
    if (is.null(duration)) {
      durIdx <<- -1
    } # no duration specified by negative number
    else {
      durIdx <<- duration
    }
    # tr$addTag("i",rate)
    xml2::xml_add_child(doc, "i", rate)

    if (w_fixed) stop("Weights already added!")
    for (i in 1:length(labels)) {
      if (i != durIdx) {
        # tr$addTag("quantities",attrs=c(l=labels[i]))
        xml2::xml_add_child(doc, "quantities", l = labels[i])
      }
    }
    w_fixed <<- TRUE
    # tr$addTag("sources","0 1")
    xml2::xml_add_child(doc, "sources", "0 1")
    invisible(NULL)
  }

  set_trans_weights <- function(labels) {
    for (i in seq_along(labels)) {
      xml2::xml_add_child(doc, "transQuantities", l = labels[i])
    }
    invisible(NULL)
  }

  process <- function() {
    if (!w_fixed) stop("Weights must be added using 'set_weights' before starting building the HMDP!")
    # tr$addTag("proc",close=FALSE)
    n <<- xml2::xml_add_child(n, "proc")
    invisible(NULL)
  }

  end_process <- function() {
    # tr$closeTag()
    n <<- xml2::xml_parent(n)
    invisible(NULL)
  }

  stage <- function(label = NULL) {
    if (is.null(label)) {
      # tr$addTag("g",close=FALSE)
      n <<- xml2::xml_add_child(n, "g")
    } else {
      # tr$addTag("g",attrs=c(l=label),close=FALSE)
      n <<- xml2::xml_add_child(n, "g", l = label)
    }
    invisible(NULL)
  }

  end_stage <- function() {
    # tr$closeTag()
    n <<- xml2::xml_parent(n)
    invisible(NULL)
  }

  state <- function(label = NULL) {
    if (is.null(label)) {
      # tr$addTag("s",close=FALSE)
      n <<- xml2::xml_add_child(n, "s")
    } else {
      # tr$addTag("s",attrs=c(l=label),close=FALSE)
      n <<- xml2::xml_add_child(n, "s", l = label)
    }
    invisible(NULL)
  }

  end_state <- function() {
    # tr$closeTag()
    n <<- xml2::xml_parent(n)
    invisible(NULL)
  }

  action <- function(label = NULL, weights, prob, states_next = NULL, trans_weights = NULL) { # prop contain tripeles (scope,idx,prob), states_next: Number of states in the next stage of the process, only needed if have a transition to the father
    scope <- prob[3 * 0:(length(prob) / 3 - 1) + 1] # scopes we consider
    if (any(scope == 3)) {
      stop("Scope = 3 is not supported in hmp files!")
    }
    term <- FALSE
    if (any(scope == 0)) { # we have an prob that return to the father
      if (is.null(states_next)) stop("Number of states at the next stage must be specified!")
      if (states_next != 0) term <- TRUE
      idx <- 3 * (which(scope == 0) - 1) + 1 # index of scope==0
      prob[idx + 1] <- prob[idx + 1] + states_next # add number of states at next stage to father idx
    }
    n <<- xml2::xml_add_child(n, "a")
    tags <- NULL
    if (!is.null(label)) tags <- c(tags, l = label)
    if (term) tags <- c(tags, term = "t")
    if (is.null(tags)) {
      # tr$addTag("a",close=FALSE)
    } else {
      # tr$addTag("a",attrs=tags,close=FALSE)
      xml2::xml_attrs(n) <- tags
    }
    if (any(scope == 2)) { # we have an prob to a new child process
      if (!all(prob == c(2, 0, 1))) stop("Only a deterministic transition to the dummy stage in the child process allowed (prop=(2,0,1))!")
      return(invisible(NULL)) # only a deterministic transition with zero weights allowed in the hmp format
    }
    # tr$addTag("q",paste(weights[which(1:length(weights)!=durIdx)],collapse=" "))  # quantities
    xml2::xml_add_child(n, "q", paste(weights[which(1:length(weights) != durIdx)], collapse = " "))
    if (!is.null(trans_weights)) {
      xml2::xml_add_child(n, "qt", paste(trans_weights, collapse = " "))
    }
    probs <- prob[which((1:length(prob) - 1) %% 3 != 0)] # probs contain pairs (idx,prob)
    if (length(probs) == 2) { # deterministic transition
      # tr$addTag("p",probs[1],attrs=c(t='d'))
      xml2::xml_add_child(n, "p", probs[1], t = "d")
    } else {
      # tr$addTag("p",paste(probs,collapse=" "),attrs=c(t='s'))
      xml2::xml_add_child(n, "p", paste(probs, collapse = " "), t = "s")
    }
    if (durIdx < 0) {
      # tr$addTag("d", 1)
      xml2::xml_add_child(n, "d", 1)
    } else {
      names(weights) <- NULL
      # tr$addTag("d", weights[durIdx])
      xml2::xml_add_child(n, "d", weights[durIdx])
    }
    invisible(NULL)
  }

  end_action <- function() {
    # tr$closeTag()
    n <<- xml2::xml_parent(n)
    invisible(NULL)
  }

  close_writer <- function() {
    # saveXML(tr$value(),file="old.hmp",compression=0,prefix = NULL)
    xml2::write_xml(doc, file)
    if (get_log) cat("\nModel saved to file:", file, "\n")
  }

  w_fixed <- FALSE # have weights been added
  durIdx <- NULL # index of weight storing the duration (number from 1)
  # tr<-xmlTree("mlhmp",dtd=NULL,attrs=c(l=desc,b=rate,dsl=rate_base,precision=precision,version="1.1"))
  doc <- xml2::xml_new_root("mlhmp", l = desc, b = rate, dsl = rate_base, precision = precision, version = "1.1")
  n <- doc # current node

  v <- list(
    set_weights = set_weights, set_trans_weights = set_trans_weights,
    stage = stage, end_stage = end_stage, state = state, end_state = end_state,
    action = action, end_action = end_action, process = process, end_process = end_process,
    close_writer = close_writer
  )
  class(v) <- c("hmp_mdp_writer")
  return(v)
}
