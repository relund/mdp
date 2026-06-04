#' Load the HMDP model defined in the binary files. The model are created in memory
#' using the external C++ library.
#'
#' @param prefix A character string with the prefix added to `bin_names`. Used to identify a
#'   specific model.
#' @param bin_names A character vector of length 7 giving the names of the binary
#'     files storing the model.
#' @param eps The sum of the transition probabilities must at most differ `eps` from one.
#' @param check Check if the MDP seems correct.
#' @param verbose More output when running algorithms.
#' @param get_log Output the log messages.
#'
#' @return A list containing relevant information about the model such as model file names
#'    (`bin_names`), time horizon (`time_horizon`), number of states (`states`), number of states at
#'    last stage of the founder process (`founder_states_last`), number of actions (`actions`),
#'    number of levels (`levels`), names of the weights associated to each action (`weight_names`)
#'    and a pointer `ptr` to the model object in memory. Note for models with an infinite
#'    time-horizon the states at the founder level is repeated at stage two so have something aka
#'    a double array representation.
#' @example inst/examples/machine-ex.R
#' @export
load_mdp <-
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
           eps = 0.00001,
           check = TRUE,
           verbose = FALSE,
           get_log = TRUE) {
    bin_names <- paste(prefix, bin_names, sep = "")
    if (!is.logical(verbose)) verbose <- FALSE
    mdp <- methods::new(HMDP, bin_names, verbose)
    .make_mdp_list(mdp, bin_names, eps = eps, check = check, get_log = get_log)
  }

.make_mdp_list <- function(mdp,
                           bin_names = character(),
                           eps = 0.00001,
                           check = TRUE,
                           get_log = TRUE) {
  if (!mdp$okay) {
    message(mdp$getLog())
    rm(mdp)
    return(invisible(NULL))
  } else if (get_log) message(mdp$getLog())

  if (check) {
    msg <- mdp$checkHMDP(as.numeric(eps))
    if (msg == 2) {
      stop(mdp$getLog(), call. = FALSE)
      return(invisible(NULL))
    } else if (get_log) message(mdp$getLog())
  }

  time_horizon <- mdp$timeHorizon
  if (time_horizon >= 1000000000) time_horizon <- Inf
  if (time_horizon >= Inf) {
    founder_states_last <- mdp$getStateSizeStage("1")
    states <- mdp$getStateSize() - founder_states_last
  } else {
    founder_states_last <- mdp$getStateSizeStage(as.character(time_horizon - 1))
    states <- mdp$getStateSize()
  }
  actions <- mdp$getActionSize()
  levels <- mdp$levels
  weight_action_names <- mdp$wActionNames
  weight_trans_names <- mdp$wTransNames
  weight_names <- mdp$wNames
  v <- list(
    bin_names = bin_names, time_horizon = time_horizon, states = states,
    founder_states_last = founder_states_last, actions = actions, levels = levels,
    weight_names = weight_names, weight_action_names = weight_action_names,
    weight_trans_names = weight_trans_names, ptr = mdp
  )
  if (mdp$externalProc) {
    v$external <- as.data.frame(matrix(mdp$getExternalInfo(), ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
    colnames(v$external) <- c("stage_str", "prefix")
  }
  class(v) <- c("HMDP", "list")
  return(v)
}


#' Internal function. Check if the indexes given are okay. Should not be used except you know what you are doing
#'
#' @aliases .check_w_dur_idx
#' @param i_w Index of the weight we want to optimize.
#' @param i_dur Index of the duration/time.
#' @param w_lth Number of weights in the model.
#' @return Nothing.
#' @name checkWDurIdx
#' @keywords internal
.check_w_dur_idx <- function(i_w, i_dur, w_lth) {
  if (length(i_w) != 1) stop("Index i_w must be of length one!", call. = FALSE)
  if (i_w > w_lth - 1) stop("Global weight index i_w must be less than ", w_lth, "!", call. = FALSE)
  if (i_w < 0) stop("Global weight index i_w must be greater or equal zero!", call. = FALSE)
  if (!is.null(i_dur)) {
    if (length(i_dur) != 1) stop("Index i_dur must be of length one!", call. = FALSE)
    if (i_w == i_dur) stop("Indices i_w and i_dur must not be the same!", call. = FALSE)
    if (i_dur > w_lth - 1) stop("Global duration weight index i_dur must be less than ", w_lth, "!", call. = FALSE)
    if (i_dur < 0) stop("Global duration weight index i_dur must be greater or equal zero!", call. = FALSE)
  }
  invisible()
}


#' Internal function. Check if the index of the weight is okay. Should not be used except you know what you are doing
#'
#' @aliases .check_w_idx
#' @param i_w Index of the weight we want to optimize.
#' @param w_lth Number of weights in the model.
#' @return Nothing.
#' @name checkWIdx
#' @keywords internal
.check_w_idx <- function(i_w, w_lth) {
  if (max(i_w) > w_lth - 1) stop("Global weight index i_w must be less than ", w_lth, "!", call. = FALSE)
  if (min(i_w) < 0) stop("Global weight index i_w must be greater or equal zero!", call. = FALSE)
  invisible()
}


#' Return the index of a weight in the model. Note that index always start from zero (C++ style), i.e. the first weight, the first state at a stage etc has index 0.
#'
#' @param mdp The MDP loaded using [load_mdp()].
#' @param w_lbl The label/string of the weight.
#' @return The index (integer).
#' @export
get_w_idx <- function(mdp, w_lbl) {
  idx <- which(mdp$weight_names == w_lbl)
  if (length(idx) == 0) idx <- which(grepl(w_lbl, mdp$weight_names, fixed = TRUE))
  if (length(idx) == 0) {
    stop("The weight name does not seem to exist in the global weight namespace.", call. = FALSE)
  }
  if (length(idx) > 1) {
    stop("The weight name is ambiguous in the global weight namespace.", call. = FALSE)
  }
  return(idx - 1)
}

.opt_sense_idx <- function(objective) {
  objective <- match.arg(objective, c("max", "min"))
  if (objective == "max") {
    return(0)
  }
  1
}

.bellman_op_idx <- function(bellman_op, include_variance = TRUE) {
  if (length(bellman_op) > 1) bellman_op <- bellman_op[1]
  choices <- c("auto", "discount", "average", "expected", "min", "max", "second_moment")
  if (include_variance) choices <- c(choices, "variance")
  bellman_op <- match.arg(bellman_op, choices)
  switch(bellman_op,
    discount = 0,
    average = 1,
    expected = 2,
    min = 5,
    max = 6,
    second_moment = 7,
    variance = 8,
    auto = NA_integer_
  )
}


#' Perform policy iteration using the average expected-weight Bellman operator on the MDP.
#'
#' The policy can afterwards be received using functions `get_policy` and `get_policy_w`.
#'
#' @param mdp The MDP loaded using [load_mdp()].
#' @param w The label of the weight we optimize.
#' @param dur The label of the duration/time such that discount rates can be calculated.
#' @param max_ite Max number of iterations. If the model does not satisfy the unichain assumption the algorithm may loop.
#' @param objective Optimize by maximizing (`"max"`) or minimizing (`"min"`) the Bellman value.
#' @param get_log Output the log messages.
#'
#' @return The optimal gain (g) calculated.
#' @seealso [get_policy()].
#' @export
run_policy_ite_ave <- function(mdp, w, dur, max_ite = 100, objective = c("max", "min"), get_log = TRUE) {
  i_w <- get_w_idx(mdp, w)
  i_dur <- get_w_idx(mdp, dur)
  sense <- .opt_sense_idx(objective)
  .check_w_dur_idx(i_w, i_dur, length(mdp$weight_names))
  g <- mdp$ptr$policyIte(1, sense, as.integer(max_ite), as.integer(i_w), as.integer(i_dur), discount_factor = 1)
  # message(mdp$ptr$getLog())
  if (get_log) cat(mdp$ptr$getLog())
  return(g)
}


#' Perform policy iteration using the discounted expected-weight Bellman operator on the MDP.
#'
#' The policy can afterwards be received using functions `get_policy` and `get_policy_w`.
#'
#' @param mdp The MDP loaded using [load_mdp()].
#' @param w The label of the weight we optimize.
#' @param dur The label of the duration/time such that discount rates can be calculated.
#' @param rate The interest rate.
#' @param rate_base The time-horizon the rate is valid over.
#' @param discount_factor The discount rate for one time unit. If specified `rate` and `rate_base` are not used to calculate the discount rate.
#' @param max_ite Max number of iterations. If the model does not satisfy the unichain assumption the algorithm may loop.
#' @param discount_method Either 'continuous' or 'discrete', corresponding to discount factor `exp(-rate/rate_base)` or `1/(1 + rate/rate_base)`, respectively. Only used if `discount_factor` is `NULL`.
#' @param objective Optimize by maximizing (`"max"`) or minimizing (`"min"`) the Bellman value.
#' @param get_log Output the log messages.
#'
#' @return Nothing.
#' @seealso [get_policy()].
#' @export
run_policy_ite_discount <- function(mdp, w, dur, rate = 0, rate_base = 1, discount_factor = NULL, max_ite = 100,
                                    discount_method = "continuous", objective = c("max", "min"), get_log = TRUE) {
  i_w <- get_w_idx(mdp, w)
  i_dur <- get_w_idx(mdp, dur)
  sense <- .opt_sense_idx(objective)
  .check_w_dur_idx(i_w, i_dur, length(mdp$weight_names))
  if (is.null(discount_factor)) {
    if (discount_method == "continuous") discount_factor <- exp(-rate / rate_base)
    if (discount_method == "discrete") discount_factor <- 1 / (1 + rate / rate_base)
  }
  g <- mdp$ptr$policyIte(0, sense, as.integer(max_ite), as.integer(i_w), as.integer(i_dur), discount_factor)
  if (get_log) cat(mdp$ptr$getLog())
  invisible()
}


#' Perform value iteration on the MDP.
#'
#' If the MDP has a finite time-horizon then arguments `times` and `eps`
#' are ignored.
#'
#' @param mdp The MDP loaded using [load_mdp()].
#' @param w The label of the weight we optimize.
#' @param dur The label of the duration/time such that discount rates can be calculated.
#' @param rate Interest rate.
#' @param rate_base The time-horizon the rate is valid over.
#' @param discount_factor The discount rate for one time unit. If specified `rate` and `rate_base` are not used to calculate the discount rate.
#' @param max_ite The max number of iterations value iteration is performed.
#' @param eps Stopping tolerance. If $max(w(t)-w(t+1)) < `eps`$ then stop the algorithm, i.e the policy becomes epsilon optimal (see Puterman p161).
#' @param term_values The terminal values used (values of the last stage in the MDP).
#' @param g Average weight. If specified then do a single iteration using the update equations under the average expected-weight Bellman operator with the specified g value.
#' @param objective Optimize by maximizing (`"max"`) or minimizing (`"min"`) the Bellman value.
#' @param bellman_op Bellman operator. Use `"auto"` for existing behavior, `"min"` for the minimum-successor operator, `"max"` for the maximum-successor operator, or `"second_moment"` for the second moment of total accumulated weight.
#' @param get_log Output the log messages.
#' @param discount_method Either 'continuous' or 'discrete', corresponding to discount factor `exp(-rate/rate_base)` or `1/(1 + rate/rate_base)`, respectively. Only used if `discount_factor` is `NULL`.
#'
#' @return NULL (invisible)
#' @references Puterman, M. Markov Decision Processes, Wiley-Interscience, 1994.
#' @example inst/examples/machine-ex.R
#' @export
run_value_ite <- function(mdp, w, dur = NULL, rate = 0, rate_base = 1, discount_factor = NULL, max_ite = 100,
                          eps = 1e-05, term_values = NULL, g = NULL, objective = c("max", "min"),
                          bellman_op = c("auto", "expected", "discount", "average", "min", "max", "second_moment"),
                          get_log = TRUE, discount_method = "continuous") {
  i_w <- get_w_idx(mdp, w)
  i_dur <- NULL
  sense <- .opt_sense_idx(objective)
  op <- .bellman_op_idx(bellman_op, include_variance = FALSE)
  if (!is.null(dur)) i_dur <- get_w_idx(mdp, dur)
  .check_w_dur_idx(i_w, i_dur, length(mdp$weight_names))
  if (is.null(discount_factor)) {
    if (discount_method == "continuous") discount_factor <- exp(-rate / rate_base)
    if (discount_method == "discrete") discount_factor <- 1 / (1 + rate / rate_base)
  }
  if (is.null(term_values)) term_values <- rep(0, mdp$founder_states_last)
  if (!is.na(op)) {
    if (op %in% c(0L, 1L) && is.null(i_dur)) {
      stop("A duration index must be specified for this Bellman operator.", call. = FALSE)
    }
    if (op == 1L && is.null(g)) {
      stop("The average weight g must be specified for the average Bellman operator.", call. = FALSE)
    }
    mdp$ptr$valueIte(
      op, sense, as.integer(ifelse(mdp$time_horizon >= Inf, max_ite, 1)),
      as.numeric(eps), as.integer(i_w), as.integer(ifelse(is.null(i_dur), 0, i_dur)),
      as.numeric(term_values), as.numeric(ifelse(is.null(g), 0, g)), as.numeric(discount_factor)
    )
  } else if (is.null(g)) {
    if (mdp$time_horizon >= Inf) {
      if (is.null(i_dur)) stop("A duration index must be specified under infinite time-horizon!")
      mdp$ptr$valueIte(
        0, sense, as.integer(max_ite),
        as.numeric(eps), as.integer(i_w), as.integer(i_dur), as.numeric(term_values),
        as.numeric(0), as.numeric(discount_factor)
      )
    } else {
      if (!is.null(i_dur)) {
        mdp$ptr$valueIte(
          0, sense, as.integer(1),
          as.numeric(0), as.integer(i_w), as.integer(i_dur), as.numeric(term_values),
          as.numeric(0), as.numeric(discount_factor)
        )
      }
      if (is.null(i_dur)) {
        mdp$ptr$valueIte(
          2, sense, as.integer(1),
          as.numeric(0), as.integer(i_w), as.integer(0), as.numeric(term_values),
          as.numeric(0), as.numeric(1)
        )
      }
    }
  } else { # value ite under average expected-weight Bellman operator
    if (is.null(i_dur)) stop("A duration index must be specified under the average expected-weight Bellman operator!")
    mdp$ptr$valueIte(
      1, sense, as.integer(1),
      as.numeric(eps), as.integer(i_w), as.integer(i_dur), as.numeric(term_values),
      as.numeric(g), as.numeric(1)
    )
  }
  if (get_log) cat(mdp$ptr$getLog())
  invisible(NULL)
}


#' Get parts of the optimal policy.
#'
#' @param mdp The MDP loaded using [load_mdp()].
#' @param s_id Vector of id's of the states we want to retrieve.
#' @param stage_str Stage string. If specified then find `s_id` based on the stage string.
#' @param state_labels Add state labels.
#' @param action_labels Add action labels of policy.
#' @param action_idx Add action index.
#' @param rewards Add weights calculated for each state.
#' @param state_str Add the state string for each state.
#' @param external A vector of stage strings corresponding to external processes we want the optimal policy of.
#' @param ... Parameters passed on when find the optimal policy of the external processes.
#'
#' Note if external is specified then it must contain stage strings from `mdp$external`. Moreover you
#' must specify further arguments passed on to [run_value_ite()] used for recreating the optimal policy e.g.
#' the g value and the label for weight and duration. See the vignette about external processes.
#'
#' @return The policy (data frame).
#' @example inst/examples/machine-ex.R
#' @export
get_policy <- function(mdp, s_id = ifelse(mdp$time_horizon >= Inf, mdp$founder_states_last + 1, 1):
                       ifelse(mdp$time_horizon >= Inf, mdp$states + mdp$founder_states_last, mdp$states) - 1,
                       stage_str = NULL, state_labels = TRUE, action_labels = TRUE, action_idx = TRUE,
                       rewards = TRUE, state_str = TRUE, external = NULL, ...) {
  if (!is.null(stage_str)) s_id <- mdp$ptr$getStateIdsStages(stage_str)
  max_s <- ifelse(mdp$time_horizon >= Inf, mdp$states + mdp$founder_states_last, mdp$states)
  if (max(s_id) >= max_s | min(s_id) < 0) {
    stop("Out of range (s_id). Need to be a subset of 0,...,", max_s - 1, "!")
  }
  cols <- 1 + state_labels + action_idx + action_labels + rewards + state_str
  policy <- data.frame(matrix(NA, nrow = length(s_id), ncol = cols))
  cols <- 1
  policy[, cols] <- s_id
  col_names <- "s_id"
  cols <- cols + 1
  if (state_str) {
    policy[, cols] <- mdp$ptr$getStateStr(s_id)
    col_names <- c(col_names, "state_str")
    cols <- cols + 1
  }
  if (state_labels) {
    policy[, cols] <- mdp$ptr$getStateLabel(s_id)
    col_names <- c(col_names, "state_label")
    cols <- cols + 1
  }
  if (action_idx) {
    policy[, cols] <- mdp$ptr$getPolicy(s_id)
    col_names <- c(col_names, "a_idx")
    cols <- cols + 1
  }
  if (action_labels) {
    policy[, cols] <- mdp$ptr$getPolicyLabel(s_id)
    col_names <- c(col_names, "action_label")
    cols <- cols + 1
  }
  if (rewards) {
    policy[, cols] <- mdp$ptr$getPolicyW(s_id)
    col_names <- c(col_names, "weight")
    cols <- cols + 1
  }
  colnames(policy) <- col_names

  if (!is.null(external)) {
    policy <- list(main = policy)
    for (s in external) {
      prefix <- subset(mdp$external, stage_str == s, select = "prefix", drop = TRUE)
      last_stage <- mdp$ptr$getNextStageStr(s)
      term_values <- get_policy(mdp, stage_str = last_stage)$weight
      ext_mdp <- load_mdp(prefix, get_log = FALSE)
      run_value_ite(ext_mdp, term_values = term_values, get_log = FALSE, ...)
      ext_policy <- get_policy(ext_mdp)
      policy[[s]] <- ext_policy
      rm(ext_mdp)
    }
  }
  return(dplyr::as_tibble(policy))
}


#' Information about the MDP
#'
#' @param mdp The MDP loaded using [load_mdp()].
#' @param s_id The id of the state(s) considered.
#' @param state_str A character vector containing the index of the state(s) (e.g. "n0,s0,a0,n1,s1").
#'   Parameter `s_id` are ignored if not NULL.
#' @param stage_str A character vector containing the index of the stage(s) (e.g. "n0,s0,a0,n1").
#'   Parameter `s_id` and `idx_s` are ignored if not NULL.
#' @param with_list Output info as a list `lst`.
#' @param with_df Output the info as a data frame.
#' @param df_level If `with_df` and equal `"state"` the data frame contains a row for each state. If equal `"action"` the data frame contains a row for each action.
#' @param as_strings_state Write state vector as a string; otherwise, output it as columns.
#' @param as_strings_actions Write action vectors (weights, transitions and probabilities) as strings; otherwise, output it as columns.
#' @param with_harc Output a hyperarcs data frame. Each row contains a hyperarc with the first column denoting the
#'   head (`s_id`), the tails (`s_id`) and the label.
#'
#' @return A list containing the list, data frame(s).
#' @example inst/examples/machine-ex.R
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
get_info <- function(mdp,
                     s_id = 1:ifelse(mdp$time_horizon < Inf, mdp$states, mdp$states + mdp$founder_states_last) - 1,
                     state_str = NULL, stage_str = NULL,
                     with_list = TRUE,
                     with_df = TRUE, df_level = "state", as_strings_state = TRUE, as_strings_actions = FALSE,
                     with_harc = FALSE) {
  if (!is.null(stage_str)) {
    s_id <- mdp$ptr$getStateIdsStages(stage_str)
    state_str <- mdp$ptr$getStateStr(s_id)
  } else {
    if (!is.null(state_str)) {
      s_id <- mdp$ptr$getStateIdsStates(state_str)
    } else {
      state_str <- mdp$ptr$getStateStr(s_id)
    }
  }
  max_s <- ifelse(mdp$time_horizon >= Inf, mdp$states + mdp$founder_states_last, mdp$states)
  if (max(s_id) >= max_s | min(s_id) < 0) {
    stop("Out of range (s_id). Need to be a subset of 0,...,", max_s - 1, "!")
  }
  l <- vector("list", length(s_id))
  lapply(l, function(x) x <- list(s_id = NULL, state_str = NULL, label = NULL, actions = NULL))

  labels <- mdp$ptr$getStateLabel(s_id)
  for (i in 1:length(l)) {
    l[[i]]$s_id <- s_id[i]
    l[[i]]$state_str <- state_str[i]
    l[[i]]$label <- labels[i]
    l[[i]]$actions <- mdp$ptr$getActionInfo(s_id[i])
  }
  names(l) <- s_id
  lst <- list()
  if (with_list) lst$lst <- l
  if (with_df) {
    df <- dplyr::tibble(s_id = l) # add list
    df <- df %>% tidyr::unnest_wider(s_id) # convert states to columns
    if (df_level == "action") {
      df <- df %>%
        tidyr::unnest_longer("actions") %>% # convert actions (one row for each action)
        tidyr::unnest_wider("actions", names_repair = tidyr::tidyr_legacy) %>% # convert action to columns
        dplyr::rename_with(~ dplyr::recode(.x, aIdx = "a_idx", transWeights = "trans_weights", .default = .x))
      df <- df %>%
        dplyr::rename(label_action = "label1")
      if (as_strings_actions) {
        df <- df %>%
          dplyr::mutate(
            weights = sapply(.data$weights, function(x) paste0(x, collapse = ",")),
            trans = sapply(.data$trans, function(x) paste0(x, collapse = ",")),
            pr = sapply(.data$pr, function(x) paste0(x, collapse = ","))
          ) %>%
          dplyr::mutate(
            weights = dplyr::na_if(.data$weights, ""),
            trans = dplyr::na_if(.data$trans, ""),
            pr = dplyr::na_if(.data$pr, "")
          )
      }
    } else {
      if (as_strings_actions) {
        df <- df %>%
          tidyr::unnest_longer("actions") %>% # convert actions (one row for each action)
          tidyr::unnest_wider("actions", names_repair = tidyr::tidyr_legacy) %>% # convert action to columns
          dplyr::rename_with(~ dplyr::recode(.x, aIdx = "a_idx", transWeights = "trans_weights", .default = .x))
        df <- df %>%
          dplyr::rename(label_action = .data$label1)
        df <- df %>%
          dplyr::mutate(
            weights = sapply(.data$weights, function(x) paste0(x, collapse = ",")),
            trans = sapply(.data$trans, function(x) paste0(x, collapse = ",")),
            pr = sapply(.data$pr, function(x) paste0(x, collapse = ","))
          ) %>%
          dplyr::mutate(
            weights = dplyr::na_if(.data$weights, ""),
            trans = dplyr::na_if(.data$trans, ""),
            pr = dplyr::na_if(.data$pr, "")
          )
        df <- df %>%
          dplyr::group_by(s_id, .data$state_str, .data$label) %>%
          tidyr::nest() %>%
          dplyr::mutate(data = lapply(.data$data, function(x) {
            if (all(is.na(x$a_idx))) {
              return(NULL)
            } else {
              return(x)
            }
          })) %>%
          dplyr::rename(actions = .data$data)
      }
    }
    if (!as_strings_state) {
      levels <- (max(stringr::str_count(df$state_str, ",")) + 1) %/% 3 + 1
      if (levels == 1) {
        nm <- paste(c("n", "s"), levels - 1, sep = "")
      }
      if (levels > 1) {
        nm <-
          c(
            paste(c("n", "s", "a"), rep(0:(levels - 2), each = 3), sep = ""),
            paste(c("n", "s"), levels - 1, sep = "")
          )
      }
      df <- df %>%
        tidyr::separate(.data$state_str, into = nm, sep = ",", remove = FALSE, fill = "right")
    }
    lst$df <- df
  }
  if (with_harc) {
    df <- dplyr::tibble(s_id = l) %>%
      tidyr::unnest_wider(s_id) %>%
      tidyr::unnest_longer("actions") %>% # convert actions (one row for each action)
      tidyr::unnest_wider("actions", names_repair = tidyr::tidyr_legacy) %>%
      dplyr::rename_with(~ dplyr::recode(.x, aIdx = "a_idx", transWeights = "trans_weights", .default = .x)) %>%
      tidyr::unnest_wider(.data$trans, names_sep = "") %>%
      dplyr::filter(!is.na(.data$a_idx)) %>%
      dplyr::select(.data$s_id, tidyr::contains("trans"), label = .data$label1)
    colnames(df) <- stringr::str_replace(colnames(df), "trans", "tail")
    colnames(df)[1] <- "head"
    lst$harcDF <- df
  }
  return(lst)
}


#' Modify the current policy by setting policy action of states.
#'
#' If the policy does not contain all states then the actions from the previous optimal
#' policy are used.
#'
#' @param mdp The MDP loaded using [load_mdp()].
#' @param policy A data frame with two columns state id `s_id` and action index `a_idx`.
#' @return NULL (invisible)
#' @example inst/examples/machine-ex.R
#' @export
set_policy <- function(mdp, policy) {
  if (!all(c("s_id", "a_idx") %in% colnames(policy))) stop("You must specify `s_id` and action index `a_idx`.")
  # if (dim(policy)[2]!=2) stop("You must specify two columns in policy.")
  mdp$ptr$setPolicy(as.integer(policy$s_id), as.integer(policy$a_idx))
  return(invisible(NULL))
}


#' Calculate weights based on current policy. Normally run after an optimal policy has been found.
#'
#' @param mdp The MDP loaded using [load_mdp()].
#' @param w_lbl The label of the weight we consider.
#' @param criterion The Bellman operator shortcut. If `expected` use expected weights, if `discount` use discounted expected weights, if `average` use average expected weights, if `min` use minimum-successor weights, if `max` use maximum-successor weights, if `second_moment` use the second moment of total accumulated weight, and if `variance` use the law-of-total-variance recursion under the current policy.
#' @param dur_lbl The label of the duration/time such that discount rates can be calculated.
#' @param rate The interest rate.
#' @param rate_base The time-horizon the rate is valid over.
#' @param discount_factor The discount rate for one time unit. If specified `rate` and `rate_base` are not used to calculate the discount rate.
#' @param term_values The terminal values used (values of the last stage in the MDP).
#' @param discount_method Either 'continuous' or 'discrete', corresponding to discount factor `exp(-rate/rate_base)` or `1/(1 + rate/rate_base)`, respectively. Only used if `discount_factor` is `NULL`.
#'
#' @return Nothing.
#' @example inst/examples/machine-ex.R
#' @export
run_calc_weights <- function(mdp, w_lbl, criterion = "expected", dur_lbl = NULL, rate = 0, rate_base = 1,
                             discount_factor = NULL, term_values = NULL, discount_method = "continuous") {
  i_w <- get_w_idx(mdp, w_lbl)
  if (!is.null(dur_lbl)) i_dur <- get_w_idx(mdp, dur_lbl)
  .check_w_idx(i_w, length(mdp$weight_names))
  if (is.null(discount_factor)) {
    if (discount_method == "continuous") discount_factor <- exp(-rate / rate_base)
    if (discount_method == "discrete") discount_factor <- 1 / (1 + rate / rate_base)
  }
  if (mdp$time_horizon < Inf) {
    if (is.null(term_values)) stop("Terminal values must be specified under finite time-horizon!")
    if (criterion == "expected") mdp$ptr$calcPolicy(2, i_w, 0, 1, discount_factor)
    if (criterion == "min") mdp$ptr$calcPolicy(5, i_w, 0, 1, discount_factor)
    if (criterion == "max") mdp$ptr$calcPolicy(6, i_w, 0, 1, discount_factor)
    if (criterion == "second_moment") mdp$ptr$calcPolicy(7, i_w, 0, 1, discount_factor)
    if (criterion == "variance") {
      mdp$ptr$setTerminalW(as.numeric(term_values))
      mdp$ptr$calcPolicy(8, i_w, 0, 1, discount_factor)
    }
    if (criterion == "discount") mdp$ptr$calcPolicy(0, i_w, 0, i_dur, discount_factor)
  } else {
    if (criterion == "discount") mdp$ptr$policyIteFixedPolicy(0, i_w, i_dur, discount_factor)
    if (criterion == "average") {
      return(mdp$ptr$policyIteFixedPolicy(1, i_w, i_dur, discount_factor))
    }
    # if (criterion=="expected") .Call("MDP_CalcWeightsFinite", mdp$ptr, as.integer(i_w), as.numeric(term_values), PACKAGE="MDP")
  }
  invisible(NULL)
}


#' Calculate the retention pay-off (RPO) or opportunity cost for some states.
#'
#' The RPO is defined as the difference between
#' the weight of the state when using action `i_a` and the maximum
#' weight of the node when using another predecessor different from `i_a`.
#'
#' @param mdp The MDP loaded using [load_mdp()].
#' @param w The label of the weight we calculate RPO for.
#' @param i_a  The action index we calculate the RPO with respect to (same size as `s_id`).
#' @param s_id Vector of id's of the states we want to retrieve.
#' @param criterion The Bellman operator shortcut. If `expected` use expected weights, if `discount` use discounted expected weights, if `average` use average expected weights.
#' @param dur The label of the duration/time such that discount rates can be calculated.
#' @param rate The interest rate.
#' @param rate_base The time-horizon the rate is valid over.
#' @param discount_factor The discount rate for one time unit. If specified `rate` and `rate_base` are not used to calculate the discount rate.
#' @param g The optimal gain (g) calculated (used if `criterion = "average"`).
#' @param objective Optimize by maximizing (`"max"`) or minimizing (`"min"`) the Bellman value.
#' @param discount_method Either 'continuous' or 'discrete', corresponding to discount factor `exp(-rate/rate_base)` or `1/(1 + rate/rate_base)`, respectively. Only used if `discount_factor` is `NULL`.
#' @param state_str Output the state string.
#'
#' @return The RPO (matrix/data frame).
#' @importFrom magrittr %>%
#' @export
get_rpo <- function(mdp, w, i_a, s_id = ifelse(mdp$time_horizon >= Inf, mdp$founder_states_last + 1, 1):
                    ifelse(mdp$time_horizon >= Inf, mdp$states + mdp$founder_states_last, mdp$states) - 1,
                    criterion = "expected", dur = "", rate = 0, rate_base = 1, discount_factor = NULL,
                    g = 0, objective = c("max", "min"), discount_method = "continuous", state_str = TRUE) {
  i_w <- get_w_idx(mdp, w)
  sense <- .opt_sense_idx(objective)
  if (criterion != "expected" && !nzchar(dur)) {
    stop("A duration weight must be specified for this Bellman operator.", call. = FALSE)
  }
  i_dur <- if (nzchar(dur)) get_w_idx(mdp, dur) else 0
  .check_w_idx(i_w, length(mdp$weight_names))
  if (is.null(discount_factor)) {
    if (discount_method == "continuous") discount_factor <- exp(-rate / rate_base)
    if (discount_method == "discrete") discount_factor <- 1 / (1 + rate / rate_base)
  }
  max_s <- ifelse(mdp$time_horizon >= Inf, mdp$states + mdp$founder_states_last, mdp$states)
  if (max(s_id) >= max_s | min(s_id) < 0) {
    stop("Out of range (s_id). Need to be a subset of 0,...,", max_s - 1, "!")
  }
  if (length(s_id) != length(i_a)) {
    stop("Vectors s_id and i_a must have same length!")
  }
  rpo <- NA
  if (criterion == "expected") rpo <- mdp$ptr$calcRPO(2, sense, as.integer(s_id), i_w, as.integer(i_a), g, i_dur, discount_factor)
  if (criterion == "discount") rpo <- mdp$ptr$calcRPO(0, sense, as.integer(s_id), i_w, as.integer(i_a), g, i_dur, discount_factor)
  if (criterion == "average") rpo <- mdp$ptr$calcRPO(1, sense, as.integer(s_id), i_w, as.integer(i_a), g, i_dur, discount_factor)
  rpo[rpo <= -1.8e+16] <- NA # less than 2 actions
  rpo <- dplyr::tibble(s_id = s_id, rpo = rpo)
  if (state_str) {
    rpo <- rpo %>%
      dplyr::transmute(s_id, state_str = mdp$ptr$getStateStr(s_id), rpo)
  }
  return(rpo)
}

#' Save the MDP to binary files
#'
#' Currently do not save external files.
#'
#' @param mdp The MDP loaded using [load_mdp()].
#' @param prefix A character string with the prefix added to `bin_names`. Used to identify a specific model.
#' @param get_log Output the log as a message.
#'
#' @return ???
#' @export
save_mdp <- function(mdp, prefix = "", get_log = TRUE) {
  mdp$ptr$save2Binary(prefix)
  if (get_log) message(mdp$ptr$getLog())
}


#' Calculate the steady state transition probabilities for the founder process (level 0).
#'
#' Assume that we consider an ergodic/irreducible time-homogeneous Markov chain specified using a policy in the MDP.
#'
#' @param mdp The MDP loaded using [load_mdp()].
#' @param get_log Output log text.
#'
#' @return A vector with steady state probabilities for all the states at the founder level.
#' @export
get_steady_state_pr <- function(mdp, get_log = FALSE) {
  pr <- mdp$ptr$steadyStatePr()
  if (get_log) message(mdp$ptr$getLog())
  return(pr)
}


# #' Set the weight of an action.
# #'
# #' @param mdp The MDP loaded using \link{load_mdp}.
# #' @param w The weight.
# #' @param s_id The state id of the state.
# #' @param idxA The action index.
# #' @param w_lbl The label of the weight we consider.
# #' @return Nothing.
# #' @example inst/examples/machine.R
# #' @export
# setActionWeight<-function(mdp, w, s_id, i_a, w_lbl) {
# 	i_w<-get_w_idx(mdp,w_lbl)
#
# 	.Call("MDP_SetActionW", mdp$ptr, as.numeric(w), as.integer(s_id), as.integer(i_a), as.integer(i_w), PACKAGE="MDP")
# 	invisible(NULL)
# }
#


#
#
#
# #' Fix the action of a state. That is, the other actions are removed from the HMDP.
# #'
# #' The actions can be reset using \code{resetActions}.
# #'
# #' @param mdp The MDP loaded using \link{load_mdp}.
# #' @param s_id The state id of the state we want to fix the action for.
# #' @param i_a  The action index of the state.
# #' @return Nothing.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @seealso \code{\link{resetActions}}, \code{\link{removeAction}}.
# #' @export
# fixAction<-function(mdp, s_id, i_a) {
# 	.Call("MDP_FixAction", mdp$ptr, as.integer(s_id), as.integer(i_a), PACKAGE="MDP")
# 	invisible(NULL)
# }
#
#
# #' Remove the action of a state from the HMDP.
# #'
# #' The actions can be reset using \code{resetActions}.
# #'
# #' @param mdp The MDP loaded using \link{load_mdp}.
# #' @param s_id The state id of the state we want to remove the action for.
# #' @param i_a  The action index of the state.
# #' @return Nothing.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @seealso \code{\link{resetActions}}, \code{\link{fixAction}}.
# #' @example inst/examples/machine.R
# #' @export
# removeAction<-function(mdp, s_id, i_a) {
# 	.Call("MDP_RemoveAction", mdp$ptr, as.integer(s_id), as.integer(i_a), PACKAGE="MDP")
# 	invisible(NULL)
# }
#
#
# #' Reset the actions of a state.
# #'
# #' @param mdp The MDP loaded using \link{load_mdp}.
# #' @return Nothing.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @seealso \code{\link{resetActions}}, \code{\link{fixAction}}.
# #' @example inst/examples/machine.R
# #' @export
# resetActions<-function(mdp) {
# 	.Call("MDP_ResetActions", mdp$ptr, PACKAGE="MDP")
# 	invisible(NULL)
# }
#

#
#
# #' Set the weight of a state.
# #'
# #' @param mdp The MDP loaded using \link{load_mdp}.
# #' @param w The weight.
# #' @param s_id The state id of the state.
# #' @param w_lbl The label of the weight we consider.
# #' @return Nothing.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @export
# setStateWeight<-function(mdp, w, s_id, w_lbl) {
# 	i_w<-get_w_idx(mdp,w_lbl)
# 	.Call("MDP_SetStateW", mdp$ptr, as.numeric(w), as.integer(s_id), as.integer(i_w), PACKAGE="MDP")
# 	invisible(NULL)
# }
#
#
# #' Return ids for states in a stage.
# #'
# #' @param mdp The MDP loaded using \link{load_mdp}.
# #' @param stages A char vector of index in the form "n0,s0,a0,n1", i.e. 3*level+1 elements in the string.
# #' @return A vector of ids for the states.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @example inst/examples/machine.R
# #' @export
# getIdSStages<-function(mdp, stages) {
# 	v<-.Call("MDP_GetIdSStage", mdp$ptr, as.character(stages), PACKAGE="MDP")
# 	return(v)
# }
#
#
# #' Return the index strings for states having id id_s.
# #'
# #' @param mdp The MDP loaded using \link{load_mdp}.
# #' @param id_s A vector of state ids.
# #' @return A vector of index for the states.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @example inst/examples/machine.R
# #' @export
# getStrIdxS<-function(mdp, id_s) {
# 	n<- mdp$states + ifelse(mdp$time_horizon>=Inf,mdp$founder_states_last,0)
# 	id_s <- id_s[id_s<n & id_s>=0]
# 	v<-.Call("MDP_GetIdxS", mdp$ptr, as.integer(id_s), PACKAGE="MDP")
# 	return(v)
# }
#
#
# #' Return the label of states having id id_s.
# #'
# #' @param mdp The MDP loaded using \link{load_mdp}.
# #' @param id_s A vector of state ids.
# #' @return A vector of labels for the states.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @example inst/examples/machine.R
# #' @export
# getLabel<-function(mdp, id_s) {
# 	n<- mdp$states + ifelse(mdp$time_horizon>=Inf,mdp$founder_states_last,0)
# 	id_s <- id_s[id_s<n & id_s>=0]
# 	v<-.Call("MDP_GetLabel", mdp$ptr, as.integer(id_s), PACKAGE="MDP")
# 	return(v)
# }
#
#
# #' Get the weights of an action.
# #'
# #' @param mdp The MDP loaded using \link{load_mdp}.
# #' @param id_s The state id.
# #' @param idxA The action index.
# #' @return A vector of weights for the action.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @example inst/examples/machine.R
# #' @export
# getActionW<-function(mdp, id_s, idxA) {
# 	l<-info(mdp, id_s[1])
# 	l<-l[[1]]$actions[idxA+1]
# 	l<-substring(l,regexpr("w",l)+3)
# 	l<-gsub(").*","",l)
# 	zz<-textConnection(l)
# 	l<-scan(zz, sep=",", quiet = TRUE)
# 	close(zz)
# 	return(l)
# }
#
#
# #' Get the ids of the transition states of an action.
# #'
# #' @param mdp The MDP loaded using \link{load_mdp}.
# #' @param id_s The state id.
# #' @param idxA The action index.
# #' @return A vector of weights for the action.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @example inst/examples/machine.R
# #' @export
# getActionTransIdS<-function(mdp, id_s, idxA) {
# 	l<-info(mdp, id_s[1])
# 	l<-l[[1]]$actions[idxA+1]
# 	l<-substring(l,regexpr("trans",l)+7)
# 	l<-gsub(").*","",l)
# 	zz<-textConnection(l)
# 	l<-scan(zz, sep=",", quiet = TRUE)
# 	close(zz)
# 	return(l)
# }
#
#
# #' Get the transition probabilities of the transition states of an action.
# #'
# #' @param mdp The MDP loaded using \link{load_mdp}.
# #' @param id_s The state id.
# #' @param idxA The action index (c++ style starting from zero).
# #' @return A vector of weights for the action.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @example inst/examples/machine.R
# #' @export
# getActionTransPr<-function(mdp, id_s, idxA) {
#   return(.Call("MDP_GetActionTransPr", mdp$ptr, as.integer(id_s), as.integer(idxA), PACKAGE="MDP") )
# }
# # getActionTransPr<-function(mdp, id_s, idxA) {
# # 	l<-info(mdp, id_s[1])
# # 	l<-l[[1]]$actions[idxA+1]
# # 	l<-substring(l,regexpr("pr",l)+4)
# # 	l<-gsub(").*","",l)
# # 	zz<-textConnection(l)
# # 	l<-scan(zz, sep=",", quiet = TRUE)
# # 	close(zz)
# # 	return(l)
# # }
#
#

#
# #' Get the transition probability matrix P for the founder process (level 0).
# #'
# #' @param mdp The MDP loaded using \link{load_mdp}.
# #' @return The state probability matrix.
# #' @author Lars Relund \email{lars@@relund.dk}
# #' @export
# get_trans_pr<-function(mdp) {
# 	v<-.Call("MDP_GetTransPr", mdp$ptr, PACKAGE="MDP")
# 	v<-matrix(v,nrow=mdp$states)
# 	return(v)
# }
