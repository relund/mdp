# Return the (parts of) state-expanded hypergraph

The function is useful together with
[`plot_hypergraph()`](http://relund.github.io/mdp/reference/plot_hypergraph.md).

## Usage

``` r
get_hypergraph(mdp, ...)
```

## Arguments

- mdp:

  The MDP loaded using
  [`load_mdp()`](http://relund.github.io/mdp/reference/load_mdp.md).

- ...:

  Arguments passed to
  [`get_info()`](http://relund.github.io/mdp/reference/get_info.md).

## Value

A list representing the hypergraph with two elements: a tibble `nodes`
and a tibble `hyperarcs`. `hyperarcs` stores `action_weights`, `trans`,
and `pr` as list-columns of vectors. `trans_weights` is a list-column of
matrices with one row per transition and one column per
transition-weight namespace.

## See also

[`plot_hypergraph()`](http://relund.github.io/mdp/reference/plot_hypergraph.md)
and [`plot.HMDP()`](http://relund.github.io/mdp/reference/plot.HMDP.md).

## Examples

``` r
## Set working dir
wd <- setwd(system.file("models", package = "MDP2"))

#### A finite-horizon replacement problem ####
mdp<-load_mdp("machine1_")
#> Read binary files (0.000195055 sec.)
#> Build the HMDP (3.5113e-05 sec.)
#> Checking MDP and found no errors (3.325e-06 sec.)
plot(mdp)

plot(mdp, action_color = "label")  # colors based on labels

plot(mdp, trans_labels = "state")  # label transitions with target state labels

plot(mdp, trans_labels = "prob")  # label transitions with transition probabilities

plot(mdp, action_color = "label", state_label = "s_id|label")  # state labels are 's_id | label'

plot(mdp, state_label = "s_idx|label", radx = 0.01)  # adjust radx in states

plot(mdp, state_label = "label", action_w_label = "none", action_label = "label", 
     trans_labels = "s_id", radx = 0.01)


scrapValues <- c(30, 10, 5, 0)  # scrap values (the values of the 4 states at stage 4)
run_value_ite(mdp, "Net reward" , term_values = scrapValues)
#> Run value iteration with epsilon = 0 at most 1 time(s)
#> using weight 'Net reward' under expected-weight Bellman operator.
#>  Finished. Cpu time 8.363e-06 sec.
plot(mdp, action_color = "policy")  # highlight optimal policy

plot(mdp, actions_visible = "policy", state_label = "weight")  # show only optimal policy



#### An infinite-horizon maintenance problem ####
mdp<-load_mdp("hct611-1_")
#> Read binary files (0.000150197 sec.)
#> Build the HMDP (2.5618e-05 sec.)
#> Checking MDP and found no errors (1.022e-06 sec.)
plot(mdp)  # plot the first two stages

plot(mdp, action_color = "label")  # colors based on labels

plot(mdp, action_color = "label", state_label = "s_id|label")  # state labels are 's_id | label'

run_policy_ite_ave(mdp,"Net reward","Duration")
#> Run policy iteration under average expected-weight Bellman operator using 
#> weight 'Net reward' over 'Duration'. Iterations (g): 
#> 1 (-0.512821) 2 (-0.446154) 3 (-0.43379) 4 (-0.43379) finished. Cpu time: 1.022e-06 sec.
#> [1] -0.43379
plot(mdp, action_color = "policy")  # highlight optimal policy

plot(mdp, actions_visible = "policy")  # show only optimal policy



#### An infinite-horizon hierarchical replacement problem ####
library(magrittr)
mdp<-load_mdp("cow_")
#> Read binary files (0.000246241 sec.)
#> Build the HMDP (0.000175665 sec.)
#> Checking MDP and found no errors (2.775e-06 sec.)
hgf <- get_hypergraph(mdp)
# modify labels
dat <- hgf$nodes %>% 
   dplyr::mutate(label = dplyr::case_when(
      label == "Low yield" ~ "L",
      label == "Avg yield" ~ "A",
      label == "High yield" ~ "H",
      label == "Dummy" ~ "D",
      label == "Bad genetic level" ~ "Bad",
      label == "Avg genetic level" ~ "Avg",
      label == "Good genetic level" ~ "Good",
      TRUE ~ "Error"
   ))
# assign nodes to grid ids
dat$g_id[1:3]<-85:87
dat$g_id[43:45]<-1:3
get_g_id<-function(process,stage,state) {
   if (process==0) start=18
   if (process==1) start=22
   if (process==2) start=26
   return(start + 14 * stage + state)
}
idx<-43
for (process in 0:2)
   for (stage in 0:4)
      for (state in 0:2) {
         if (stage==0 & state>0) break
         idx<-idx-1
         #cat(idx,process,stage,state,get_g_id(process,stage,state),"\n")
         dat$g_id[idx]<-get_g_id(process,stage,state)
      }
hgf$nodes <- dat
# modify labels
dat <- hgf$hyperarcs %>% 
   dplyr::mutate(label = dplyr::case_when(
      label == "Replace" ~ "R",
      label == "Keep" ~ "K",
      label == "Dummy" ~ "D",
      TRUE ~ "Error"
   ),
   col = dplyr::case_when(
      label == "R" ~ "deepskyblue3",
      label == "K" ~ "darkorange1",
      label == "D" ~ "black",
      TRUE ~ "Error"
   ),
   lwd = 0.5,
   label = ""
   ) 
hgf$hyperarcs <- dat
# plot hypergraph
oldpar <- par(mai = c(0, 0, 0, 0))
plot_hypergraph(grid_dim = c(14, 7), hgf, cex = 0.8, radx = 0.02, rady = 0.03)

par(oldpar)


## A simple finite-horizon MDP with action and transition weights
prefix <- file.path(tempdir(), "plot_transition_rewards_")
w <- binary_mdp_writer(prefix)
w$set_weights("Cost")
w$set_trans_weights(c("Reward", "Disease"))
w$process()
   w$stage()
      w$state(label = "S1")
         w$action(
            label = "A1", weights = 2, id = c(1), pr = c(1),
            trans_weights = c(20, 0.3), end = TRUE
         )
         w$action(
            label = "A2", weights = 1, id = c(0, 1), pr = c(0.3, 0.7),
            trans_weights = c(25, 0.4, 15, 0.2), end = TRUE
         )
      w$end_state()
   w$end_stage()
   w$stage()
      w$state(label = "S2")
         w$action(
            label = "A3", weights = 3, id = c(0, 1, 2), pr = c(0.5, 0.3, 0.2),
            trans_weights = c(0, 0.05, 12, 0.2, 30, 0.8), end = TRUE
         )
         w$action(
            label = "A4", weights = 2, id = c(1, 2), pr = c(0.6, 0.4),
            trans_weights = c(22, 0.35, 27, 0.7), end = TRUE
         )
      w$end_state()
      w$state(label = "S3")
         w$action(
            label = "A5", weights = 1, id = c(0, 1), pr = c(0.4, 0.6),
            trans_weights = c(5, 0, 16, 0.25), end = TRUE
         )
         w$action(
            label = "A6", weights = 4, id = c(0, 1, 2), pr = c(0.1, 0.3, 0.6),
            trans_weights = c(14, 0.15, 21, 0.45, 29, 1), end = TRUE
         )
      w$end_state()
   w$end_stage()
   w$stage()
      w$state(label = "S4", end = TRUE)
      w$state(label = "S5", end = TRUE)
      w$state(label = "S6", end = TRUE)
   w$end_stage()
w$end_process()
w$close_writer()
#> 
#>   Statistics:
#>     states : 6 
#>     actions: 6 
#>     weights: 1 
#> 
#>   Closing binary MDP writer.
#> 

mdp <- load_mdp(prefix, get_log = FALSE)
plot(mdp, action_color = "label", trans_labels = "weights", action_w_label = "weight", 
     radx = 0.005, rady = 0.01)


## Reset working dir
setwd(wd)
```
