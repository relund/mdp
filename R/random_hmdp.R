#' Generate a "random" HMDP stored in a set of binary files.
#'
#' @param prefix A character string with the prefix added to the file(s).
#' @param levels Maximum number of levels. Set `child_process_pr = 1` if want exact this number of levels.
#' @param time_horizon The time horizon for each level (vector). For the founder the time-horizon can be Inf.
#' @param states Number of states at each stage at a given level (vector of length levels)
#' @param actions Min and max number of actions at a state.
#' @param child_process_pr Probability of creating a child process when define action.
#' @param external_process_pr Probability of creating an external process given that we create a child process. Only works if levels>2 and and currently does not generate external processes which include external processes.
#' @param rewards Min and max reward used.
#' @param durations Min and max duration used.
#' @param reward_name Weight name used for reward.
#' @param duration_name Weight name used for duration.
#'
#' @return The file prefix (character).
#'
#' @export
random_hmdp <- function(prefix = "", levels = 3, time_horizon = c(Inf, 3, 4), states = c(2, 4, 5), actions = c(1, 2),
                        child_process_pr = 0.5, external_process_pr = 0, rewards = c(0, 100), durations = c(1, 10),
                        reward_name = "Reward", duration_name = "Duration") {
  # gen finite time-horizon process function
  gen_process <- function(levels, time_horizon, states, actions, child_process_pr, rewards, durations, states_father = NULL) {
    w$process()
    for (l1 in 1:time_horizon[1] - 1) {
      w$stage()
      for (s1 in 1:states[1] - 1) {
        w$state(s1)
        aSize <- sample(actions[1]:actions[2], 1)
        for (a1 in 1:aSize - 1) {
          if (levels > 1) isChild <- stats::rbinom(1, 1, child_process_pr) == 1 else isChild <- FALSE
          if (isChild) {
            idx <- sample(1:states[2] - 1, states[2] / 2)
            pr <- rep(1 / length(idx), length(idx))
            scp <- rep(2, length(idx))
            isExt <- stats::rbinom(1, 1, external_process_pr) == 1
            if (isExt) {
              message("\n External: ", appendLF = FALSE)
              pfx <- paste(prefix, l1, "-", s1, "-", a1, "_", sub("\\.", "-", format(Sys.time(), "%H-%M-%OS4")), "_", sep = "")
              random_hmdp(pfx, levels - 1, time_horizon[2:length(time_horizon)], states[2:length(states)], actions, child_process_pr, rewards, durations)
              # stop("tst")
              w$include_process(pfx, label = a1, weights = c(sample(rewards[1]:rewards[2], 1), sample(durations[1]:durations[2], 1)), prob = as.vector(t(matrix(c(scp, idx, pr), ncol = 3))), term_states = states[2])
              w$stage() # jump actions of last stage in the external process
              for (s2 in 1:states[2] - 1) {
                w$state(s2)
                idx <- sample(1:states[1] - 1, states[1] / 2)
                pr <- rep(1 / length(idx), length(idx))
                scp <- rep(0, length(idx))
                w$action(label = "rep", weights = c(sample(rewards[1]:rewards[2], 1), sample(durations[1]:durations[2], 1)), prob = as.vector(t(matrix(c(scp, idx, pr), ncol = 3))), end = TRUE)
                w$end_state()
              }
              w$end_stage()
              w$end_include_process()
            } else {
              w$action(label = a1, weights = c(sample(rewards[1]:rewards[2], 1), sample(durations[1]:durations[2], 1)), prob = as.vector(t(matrix(c(scp, idx, pr), ncol = 3))))
              gen_process(levels - 1, time_horizon[2:length(time_horizon)], states[2:length(states)], actions, child_process_pr, rewards, durations, states[1])
              w$end_action()
            }
          } else {
            idx <- sample(1:states[1] - 1, states[1] / 2)
            pr <- rep(1 / length(idx), length(idx))
            scp <- rep(1, length(idx))
            w$action(label = a1, weights = c(sample(rewards[1]:rewards[2], 1), sample(durations[1]:durations[2], 1)), prob = as.vector(t(matrix(c(scp, idx, pr), ncol = 3))), end = TRUE)
          }
        }
        w$end_state()
      }
      w$end_stage()
    }
    w$stage() # last stage
    for (s1 in 1:states[1] - 1) {
      w$state(s1)
      if (!is.null(states_father)) {
        idx <- sample(1:states_father - 1, states_father / 2)
        pr <- rep(1 / length(idx), length(idx))
        scp <- rep(0, length(idx))
        w$action(
          label = a1, weights = c(sample(rewards[1]:rewards[2], 1), sample(durations[1]:durations[2], 1)),
          prob = as.vector(t(matrix(c(scp, idx, pr), ncol = 3)))
        )
        w$end_action()
      }
      w$end_state()
    }
    w$end_stage()
    w$end_process()
  }

  message("Create random HMDP '", prefix, "' with at most ", levels, " levels ... ", appendLF = FALSE)
  w <- binary_mdp_writer(prefix)
  w$set_weights(c(reward_name, duration_name))
  if (!is.infinite(time_horizon[1])) {
    gen_process(levels, time_horizon, states, actions, child_process_pr, rewards, durations)
  } else {
    w$process()
    w$stage()
    for (s1 in 1:states[1]) {
      w$state(s1)
      aSize <- sample(actions[1]:actions[2], 1)
      for (a1 in 1:aSize - 1) {
        if (levels > 1) isChild <- stats::rbinom(1, 1, child_process_pr) == 1 else isChild <- FALSE
        if (isChild) idx <- sample(1:states[2] - 1, states[2] / 2) else idx <- 1:states[1] - 1
        pr <- rep(1 / length(idx), length(idx))
        if (sum(pr) != 1) stop("Pr don't sum to one!")
        if (isChild) scp <- rep(2, length(idx)) else scp <- rep(1, length(idx))
        # print(as.vector( t(matrix(c(scp,idx,pr), ncol=3)) ))
        w$action(
          label = a1, weights = c(sample(rewards[1]:rewards[2], 1), sample(durations[1]:durations[2], 1)),
          prob = as.vector(t(matrix(c(scp, idx, pr), ncol = 3)))
        )
        if (isChild) {
          gen_process(
            levels - 1, time_horizon[2:length(time_horizon)],
            states[2:length(states)], actions, child_process_pr, rewards, durations, states[1]
          )
        }
        w$end_action()
      }

      w$end_state()
    }
    w$end_stage()
    w$end_process()
  }
  w$close_writer()
  message(" finished.")
  return(prefix)
}
