# # Create the small machine repleacement problem used as an example in L.R.
# # Nielsen and A.R. Kristensen. Finding the K best policies in a finite-horizon
# # Markov decision process. European Journal of Operational Research,
# # 175(2):1164?1179, 2006. doi:10.1016/j.ejor.2005.06.011.
# 
# ## Create the MDP using a dummy replacement node
# prefix<-"machine_"
# w <- binary_mdp_writer(prefix)
# w$set_weights(c("Net reward"))
# w$process()
#    w$stage()   # stage n=0
#       w$state(label="Dummy")          # v=(0,0)
#          w$action(label="buy", weights=-100, prob=c(1,0,0.7, 1,1,0.3), end=T)
#       w$end_state()
#    w$end_stage()
#    w$stage()   # stage n=1
#       w$state(label="good")           # v=(1,0)
#          w$action(label="mt", weights=55, prob=c(1,0,1), end=T)
#          w$action(label="nmt", weights=70, prob=c(1,0,0.6, 1,1,0.4), end=T)
#       w$end_state()
#       w$state(label="average")        # v=(1,1)
#          w$action(label="mt", weights=40, prob=c(1,0,1), end=T)
#          w$action(label="nmt", weights=50, prob=c(1,1,0.6, 1,2,0.4), end=T)
#       w$end_state()
#    w$end_stage()
#    w$stage()   # stage n=2
#       w$state(label="good")           # v=(2,0)
#          w$action(label="mt", weights=55, prob=c(1,0,1), end=T)
#          w$action(label="nmt", weights=70, prob=c(1,0,0.5, 1,1,0.5), end=T)
#       w$end_state()
#       w$state(label="average")        # v=(2,1)
#          w$action(label="mt", weights=40, prob=c(1,0,1), end=T)
#          w$action(label="nmt", weights=50, prob=c(1,1,0.5, 1,2,0.5), end=T)
#       w$end_state()
#       w$state(label="not working")    # v=(2,2)
#          w$action(label="mt", weights=30, prob=c(1,0,1), end=T)
#          w$action(label="rep", weights=5, prob=c(1,3,1), end=T)
#       w$end_state()
#    w$end_stage()
#    w$stage()   # stage n=3
#       w$state(label="good")           # v=(3,0)
#          w$action(label="mt", weights=55, prob=c(1,0,1), end=T)
#          w$action(label="nmt", weights=70, prob=c(1,0,0.2, 1,1,0.8), end=T)
#       w$end_state()
#       w$state(label="average")        # v=(3,1)
#          w$action(label="mt", weights=40, prob=c(1,0,1), end=T)
#          w$action(label="nmt", weights=50, prob=c(1,1,0.2, 1,2,0.8), end=T)
#       w$end_state()
#       w$state(label="not working")    # v=(3,2)
#          w$action(label="mt", weights=30, prob=c(1,0,1), end=T)
#          w$action(label="rep", weights=5, prob=c(1,3,1), end=T)
#       w$end_state()
#       w$state(label="replaced")       # v=(3,3)
#          w$action(label="Dummy", weights=0, prob=c(1,3,1), end=T)
#       w$end_state()
#    w$end_stage()
#    w$stage()   # stage n=4
#       w$state(label="good", end=T)        # v=(4,0)
#       w$state(label="average", end=T)     # v=(4,1)
#       w$state(label="not working", end=T) # v=(4,2)
#       w$state(label="replaced", end=T)    # v=(4,3)
#    w$end_stage()
# w$end_process()
# w$close_writer()
# 
# ## Some info about the model
# state_idx_df(prefix)      # states of the MDP with labels returned as a data frame
# action_info(prefix)      # all action information of the MDP returned in a single data frame
# 
# ## Load the model into memory
# mdp<-load_mdp(prefix)
# mdp
# 
# ## Perform value iteration
# w<-"Net reward"             # label of the weight we want to optimize
# scrapValues<-c(30,10,5,0)   # scrap values (the values of the 4 states at stage 4)
# run_value_ite(mdp, w, term_values=scrapValues)
# 
# ## Print the optimal policy
# policy<-get_policy(mdp, labels=TRUE)     # optimal policy for each s_id
# states<-state_idx_df(prefix)              # information about the states
# policy<-merge(states,policy)            # merge the two data frames
# policyW<-get_policy_w(mdp, w)             # the optimal rewards of the policy
# policy<-merge(policy,policyW)           # add the rewards
# policy
# 
# ## Calculate the weights of the policy always to maintain
# policy<-data.frame(s_id=states$s_id,i_a=0)
# set_policy(mdp, policy)
# calcWeights(mdp, w, term_values=scrapValues)
# policy<-get_policy(mdp, labels=TRUE)     # optimal policy for each s_id
# states<-state_idx_df(prefix)              # information about the states
# policy<-merge(states,policy)            # merge the two data frames
# policyW<-get_policy_w(mdp, w)             # the optimal rewards of the policy
# policy<-merge(policy,policyW)           # add the rewards
# policy
# 
# ## Modify the MDP in memory: remove the maintain action in the states of stage 1
# removeAction(mdp, s_id=1, i_a=0)  # remove action 0 at the state with s_id=1
# removeAction(mdp, s_id=2, i_a=0)
# 
# ## Perform value iteration on the modified MDP
# run_value_ite(mdp, w, term_values=scrapValues)
# policy<-get_policy(mdp, labels=TRUE)     # optimal policy for each s_id
# states<-state_idx_df(prefix)              # information about the states
# policy<-merge(states,policy)            # merge the two data frames
# policyW<-get_policy_w(mdp, w)             # the optimal rewards of the policy
# policy<-merge(policy,policyW)           # add the rewards
# policy
# 
# resetActions(mdp)   # reset the MDP such that all actions are used
# 
# ## Modify the weight of action 'buy'
# setActionWeight(mdp, w=-50, s_id=0, i_a=0, w_lbl=w)
# 
# ## Perform value iteration on the modified MDP
# run_value_ite(mdp, w, term_values=scrapValues)
# policy<-get_policy(mdp, labels=TRUE)     # optimal policy for each s_id
# states<-state_idx_df(prefix)              # information about the states
# policy<-merge(states,policy)            # merge the two data frames
# policyW<-get_policy_w(mdp, w)             # the optimal rewards of the policy
# policy<-merge(policy,policyW)           # add the rewards
# policy
# 
# 
# 
# 
# # The example given in L.R. Nielsen and A.R. Kristensen. Finding the K best 
# # policies in a finite-horizon Markov decision process. European Journal of 
# # Operational Research, 175(2):1164?1179, 2006. doi:10.1016/j.ejor.2005.06.011, 
# # does actually not have any dummy replacement node as in the MDP above. The same 
# # model can be created using a single dummy node at the end of the process.   
# 
# ## Create the MDP using a single dummy node
# prefix<-"machine_"
# w <- binary_mdp_writer(prefix)
# w$set_weights(c("Net reward"))
# w$process()
#    w$stage()   # stage n=0
#       w$state(label="Dummy")          # v=(0,0)
#          w$action(label="buy", weights=-100, prob=c(1,0,0.7, 1,1,0.3), end=T)
#       w$end_state()
#    w$end_stage()
#    w$stage()   # stage n=1
#       w$state(label="good")           # v=(1,0)
#          w$action(label="mt", weights=55, prob=c(1,0,1), end=T)
#          w$action(label="nmt", weights=70, prob=c(1,0,0.6, 1,1,0.4), end=T)
#       w$end_state()
#       w$state(label="average")        # v=(1,1)
#          w$action(label="mt", weights=40, prob=c(1,0,1), end=T)
#          w$action(label="nmt", weights=50, prob=c(1,1,0.6, 1,2,0.4), end=T)
#       w$end_state()
#    w$end_stage()
#    w$stage()   # stage n=2
#       w$state(label="good")           # v=(2,0)
#          w$action(label="mt", weights=55, prob=c(1,0,1), end=T)
#          w$action(label="nmt", weights=70, prob=c(1,0,0.5, 1,1,0.5), end=T)
#       w$end_state()
#       w$state(label="average")        # v=(2,1)
#          w$action(label="mt", weights=40, prob=c(1,0,1), end=T)
#          w$action(label="nmt", weights=50, prob=c(1,1,0.5, 1,2,0.5), end=T)
#       w$end_state()
#       w$state(label="not working")    # v=(2,2)
#          w$action(label="mt", weights=30, prob=c(1,0,1), end=T)
#          w$action(label="rep", weights=5, prob=c(3,12,1), end=T)     # transition to the node with s_id=12 (Dummy)
#       w$end_state()
#    w$end_stage()
#    w$stage()   # stage n=3
#       w$state(label="good")           # v=(3,0)
#          w$action(label="mt", weights=55, prob=c(1,0,1), end=T)
#          w$action(label="nmt", weights=70, prob=c(1,0,0.2, 1,1,0.8), end=T)
#       w$end_state()
#       w$state(label="average")        # v=(3,1)
#          w$action(label="mt", weights=40, prob=c(1,0,1), end=T)
#          w$action(label="nmt", weights=50, prob=c(1,1,0.2, 1,2,0.8), end=T)
#       w$end_state()
#       w$state(label="not working")    # v=(3,2)
#          w$action(label="mt", weights=30, prob=c(1,0,1), end=T)
#          w$action(label="rep", weights=5, prob=c(3,12,1), end=T)
#       w$end_state()
#    w$end_stage()
#    w$stage()   # stage n=4
#       w$state(label="good")        # v=(4,0)
#          w$action(label="rep", weights=30, prob=c(1,0,1), end=T)
#       w$end_state()
#       w$state(label="average")     # v=(4,1)
#          w$action(label="rep", weights=10, prob=c(1,0,1), end=T)
#       w$end_state()
#       w$state(label="not working") # v=(4,2)
#          w$action(label="rep", weights=5, prob=c(1,0,1), end=T)
#       w$end_state()
#    w$end_stage()
#    w$stage()   # stage n=5
#       w$state(label="Dummy", end=T)        # v=(5,0)
#    w$end_stage()
# w$end_process()
# w$close_writer()
# 
# 
# ## Some info about the model
# state_idx_df(prefix)      # states of the MDP with labels returned as a data frame
# action_info(prefix)      # all action information of the MDP returned in a single data frame
# 
# ## Load the model into memory
# mdp<-load_mdp(prefix)
# mdp
# 
# ## Perform value iteration
# w<-"Net reward"             # label of the weight we want to optimize
# scrapValues<-c(30,10,5,0)   # scrap values (the values of the 4 states at stage 4)
# run_value_ite(mdp, w, term_values=scrapValues)
