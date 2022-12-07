#' Generate a "random" HMDP stored in a set of binary files.
#'
#' @param prefix A character string with the prefix added to til file(s).
#' @param levels Maximum number of levels. Set childProcessPr=1 if want exact this number of levels.
#' @param timeHorizon The time horizon for each level (vector). For the founder the timehorizon can be Inf.
#' @param states Number of states at each stage at a given level (vector of length levels)
#' @param actions Min and max number of actions at a state.
#' @param childProcessPr Probability of creating a child process when define action.
#' @param externalProcessPr Probability of creating an external process given that we create a child process. Only works if levels>2 and and currently does not generate external processes which include external processes.
#' @param rewards Min and max reward used.
#' @param durations Min and max duration used.
#' @param rewardName Weight name used for reward.
#' @param durationName Weight name used for duration.
#'
#' @return NULL
#'
#' @author Lars Relund \email{lars@@relund.dk}
#'
#' @export
randomHMDP<-function(prefix="", levels=3, timeHorizon=c(Inf,3,4), states=c(2,4,5), actions=c(1,2),
                     childProcessPr = 0.5, externalProcessPr=0, rewards=c(0,100), durations=c(1,10), 
                     rewardName="Reward", durationName="Duration" ) 
{
   # gen finite timehorizon process function
   genProcess<-function(levels, timeHorizon, states, actions, childProcessPr, rewards, durations, statesFather=NULL) {
      w$process()
         for(l1 in 1:timeHorizon[1]-1 ) {
            w$stage()
               for (s1 in 1:states[1]-1) {
                  w$state(s1)
                     aSize = sample(actions[1]:actions[2],1)
                     for (a1 in 1:aSize-1) {
                        if (levels>1) isChild = rbinom(1,1,childProcessPr)==1 else isChild = FALSE
                        if (isChild) {
                           idx<-sample(1:states[2]-1,states[2]/2) 
                           pr<-rep(1/length(idx),length(idx))
                           scp<-rep(2,length(idx))
                           isExt = rbinom(1,1,externalProcessPr)==1
                           if (isExt) {
                              message("\n External: ", appendLF = FALSE)
                              pfx<-paste(prefix,l1,"-",s1,"-",a1,"_",sub("\\.","-",format(Sys.time(), "%H-%M-%OS4")),"_",sep="")
                              randomHMDP(pfx, levels-1, timeHorizon[2:length(timeHorizon)], states[2:length(states)], actions, childProcessPr, rewards, durations) 
                              #stop("tst")
                              w$includeProcess(pfx, label=a1, weights=c(sample(rewards[1]:rewards[2],1), sample(durations[1]:durations[2],1)), prob = as.vector( t(matrix(c(scp,idx,pr), ncol=3)) ), termStates=states[2])
                                 w$stage()   # jump actions of last stage in the external process
                                 for (s2 in 1:states[2]-1) {
                                    w$state(s2)
                                       idx<-sample(1:states[1]-1,states[1]/2) 
                                       pr<-rep(1/length(idx),length(idx))
                                       scp<-rep(0,length(idx))
                                       w$action(label="rep", weights=c(sample(rewards[1]:rewards[2],1), sample(durations[1]:durations[2],1)), prob = as.vector( t(matrix(c(scp,idx,pr), ncol=3)) ), end=TRUE)
                                    w$endState()
                                 }
                                 w$endStage()
                              w$endIncludeProcess()
                           } else {
                              w$action(label=a1, weights=c(sample(rewards[1]:rewards[2],1), sample(durations[1]:durations[2],1)), prob = as.vector( t(matrix(c(scp,idx,pr), ncol=3)) ))
                                 genProcess(levels-1, timeHorizon[2:length(timeHorizon)], states[2:length(states)], actions, childProcessPr, rewards, durations, states[1])
                              w$endAction()
                           }
                           
                        } else {
                           idx<-sample(1:states[1]-1,states[1]/2)
                           pr<-rep(1/length(idx),length(idx))
                           scp<-rep(1,length(idx))
                           w$action(label=a1, weights=c(sample(rewards[1]:rewards[2],1), sample(durations[1]:durations[2],1)), prob = as.vector( t(matrix(c(scp,idx,pr), ncol=3)) ), end=TRUE)
                        }
                     }
                  w$endState()
               }
            w$endStage()
         }
         w$stage()   # last stage
            for (s1 in 1:states[1]-1) {
               w$state(s1)
                  if (!is.null(statesFather)) {
                     idx<-sample(1:statesFather-1,statesFather/2)
                     pr<-rep(1/length(idx),length(idx))
                     scp<-rep(0,length(idx))
                     w$action(label=a1, weights=c(sample(rewards[1]:rewards[2],1), sample(durations[1]:durations[2],1)), 
                              prob = as.vector( t(matrix(c(scp,idx,pr), ncol=3)) ))
                     w$endAction()
                  }   
               w$endState()
            }
         w$endStage()
      w$endProcess()
   }
   
   message("Create random HMDP '", prefix, "' with at most ", levels, " levels ... ", appendLF = FALSE)
   w<-binaryMDPWriter(prefix)
   w$setWeights(c(rewardName,durationName))
   if (!is.infinite(timeHorizon[1])) genProcess(levels, timeHorizon, states, actions, childProcessPr, rewards, durations)
   else {
      w$process()
         w$stage()
            for (s1 in 1:states[1]) {
               w$state(s1)
                  aSize = sample(actions[1]:actions[2],1)
                  for (a1 in 1:aSize-1) {
                     if (levels>1) isChild = stats::rbinom(1,1,childProcessPr)==1 else isChild = FALSE
                     if (isChild) idx<-sample(1:states[2]-1,states[2]/2) else idx<-1:states[1]-1 
                     pr<-rep(1/length(idx),length(idx))
                     if (sum(pr)!=1) stop("Pr don't sum to one!")
                     if (isChild) scp<-rep(2,length(idx)) else scp<-rep(1,length(idx))
                     #print(as.vector( t(matrix(c(scp,idx,pr), ncol=3)) ))
                     w$action(label=a1, weights=c(sample(rewards[1]:rewards[2],1), sample(durations[1]:durations[2],1)), 
                              prob = as.vector( t(matrix(c(scp,idx,pr), ncol=3)) ))
                        if (isChild) genProcess(levels-1, timeHorizon[2:length(timeHorizon)], 
                                                states[2:length(states)], actions, childProcessPr, rewards, durations, states[1])
                     w$endAction()
                  }
                  
               w$endState()
            }
         w$endStage()
      w$endProcess()
   }
   w$closeWriter()
   message(" finished.")
}
