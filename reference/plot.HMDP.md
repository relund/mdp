# Plot the state-expanded hypergraph of the MDP.

Plot the state-expanded hypergraph of the MDP.

## Usage

``` r
# S3 method for class 'HMDP'
plot(x, ...)
```

## Arguments

- x:

  The MDP model.

- ...:

  Arguments passed to
  [`plotHypergraph()`](http://relund.github.io/mdp/reference/plotHypergraph.md).

## Value

No return value (NULL invisible), called for side effects (plotting).

## See also

[`getHypergraph()`](http://relund.github.io/mdp/reference/getHypergraph.md)
and
[`plotHypergraph()`](http://relund.github.io/mdp/reference/plotHypergraph.md)
for possible arguments.

## Examples

``` r
## Set working dir
wd <- setwd(system.file("models", package = "MDP2"))

#### A finite-horizon replacement problem ####
mdp<-loadMDP("machine1_")
#> Read binary files (0.000114744 sec.)
#> Build the HMDP (3.5156e-05 sec.)
#> Checking MDP and found no errors (9.61e-07 sec.)
plot(mdp)

plot(mdp, actionColor = "label")  # colors based on labels

plot(mdp, transLabels = "state")  # label transitions with target state labels

plot(mdp, transLabels = "prob")  # label transitions with transition probabilities

plot(mdp, actionColor = "label", stateLabel = "sId|label")  # state labels are 'sId | label'

plot(mdp, stateLabel = "sIdx|label", radx = 0.01)  # adjust radx in states

plot(mdp, stateLabel = "label", actionWLabel = "none", actionLabel = "label", 
     transLabels = "sId", radx = 0.01)


scrapValues <- c(30, 10, 5, 0)  # scrap values (the values of the 4 states at stage 4)
runValueIte(mdp, "Net reward" , termValues = scrapValues)
#> Run value iteration with epsilon = 0 at most 1 time(s)
#> using weight 'Net reward' under expected-weight Bellman operator.
#>  Finished. Cpu time 8.156e-06 sec.
plot(mdp, actionColor = "policy")  # highlight optimal policy

plot(mdp, actionsVisible = "policy", stateLabel = "weight")  # show only optimal policy



#### An infinite-horizon maintenance problem ####
mdp<-loadMDP("hct611-1_")
#> Read binary files (0.000301964 sec.)
#> Build the HMDP (2.8052e-05 sec.)
#> Checking MDP and found no errors (2.084e-06 sec.)
plot(mdp)  # plot the first two stages

plot(mdp, actionColor = "label")  # colors based on labels

plot(mdp, actionColor = "label", stateLabel = "sId|label")  # state labels are 'sId | label'

runPolicyIteAve(mdp,"Net reward","Duration")
#> Run policy iteration under average expected-weight Bellman operator using 
#> weight 'Net reward' over 'Duration'. Iterations (g): 
#> 1 (-0.512821) 2 (-0.446154) 3 (-0.43379) 4 (-0.43379) finished. Cpu time: 2.084e-06 sec.
#> [1] -0.43379
plot(mdp, actionColor = "policy")  # highlight optimal policy

plot(mdp, actionsVisible = "policy")  # show only optimal policy



#### An infinite-horizon hierarchical replacement problem ####
library(magrittr)
mdp<-loadMDP("cow_")
#> Read binary files (0.00026235 sec.)
#> Build the HMDP (0.000145461 sec.)
#> Checking MDP and found no errors (4.168e-06 sec.)
hgf <- getHypergraph(mdp)
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
dat$gId[1:3]<-85:87
dat$gId[43:45]<-1:3
getGId<-function(process,stage,state) {
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
         #cat(idx,process,stage,state,getGId(process,stage,state),"\n")
         dat$gId[idx]<-getGId(process,stage,state)
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
plotHypergraph(gridDim = c(14, 7), hgf, cex = 0.8, radx = 0.02, rady = 0.03)

par(oldpar)


## A simple finite-horizon MDP with action and transition weights
prefix <- file.path(tempdir(), "plot_transition_rewards_")
w <- binaryMDPWriter(prefix)
w$setWeights("Cost")
w$setTransWeights(c("Reward", "Disease"))
w$process()
   w$stage()
      w$state(label = "S1")
         w$action(
            label = "A1", weights = 2, id = c(1), pr = c(1),
            transWeights = c(20, 0.3), end = TRUE
         )
         w$action(
            label = "A2", weights = 1, id = c(0, 1), pr = c(0.3, 0.7),
            transWeights = c(25, 0.4, 15, 0.2), end = TRUE
         )
      w$endState()
   w$endStage()
   w$stage()
      w$state(label = "S2")
         w$action(
            label = "A3", weights = 3, id = c(0, 1, 2), pr = c(0.5, 0.3, 0.2),
            transWeights = c(0, 0.05, 12, 0.2, 30, 0.8), end = TRUE
         )
         w$action(
            label = "A4", weights = 2, id = c(1, 2), pr = c(0.6, 0.4),
            transWeights = c(22, 0.35, 27, 0.7), end = TRUE
         )
      w$endState()
      w$state(label = "S3")
         w$action(
            label = "A5", weights = 1, id = c(0, 1), pr = c(0.4, 0.6),
            transWeights = c(5, 0, 16, 0.25), end = TRUE
         )
         w$action(
            label = "A6", weights = 4, id = c(0, 1, 2), pr = c(0.1, 0.3, 0.6),
            transWeights = c(14, 0.15, 21, 0.45, 29, 1), end = TRUE
         )
      w$endState()
   w$endStage()
   w$stage()
      w$state(label = "S4", end = TRUE)
      w$state(label = "S5", end = TRUE)
      w$state(label = "S6", end = TRUE)
   w$endStage()
w$endProcess()
w$closeWriter()
#> 
#>   Statistics:
#>     states : 6 
#>     actions: 6 
#>     weights: 1 
#> 
#>   Closing binary MDP writer.
#> 

mdp <- loadMDP(prefix, getLog = FALSE)
plot(mdp, actionColor = "label", transLabels = "weights", actionWLabel = "weight", 
     radx = 0.005, rady = 0.01)


## Reset working dir
setwd(wd)
```
