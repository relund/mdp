#' Create and optimize MDPs or hierarchical MDPs with discrete time steps and state space.
#' 
#' @section History: 
#' 
#' 
#'
#' @section To do: 
#' 
#' Dynamic hypergraph data type
#' 
#' Nested loading in memory (specify a HMDP with special actions containing child + father jump actions)
#' Idea when define the proc with an external nested process use 
#' w$includeProcess(prefix, transPr, index) (specify the child jump action)
#'   w$fatherJumpAction(sIdx, ...) (specify father jump action for the last stage of the external proc) 
#'   w$fatherJumpAction(sIdx, ...) 
#' w$includeProcess()
#' 
#' The hgf then is formed with a subprocess mimic the 1. and last stage of the external proc, i.e. we include the jump pr in the hgf
#' We need a new binary file "externalProcess.bin" for storing the nested process in the format "n0 s0 a0 n1 s1 prefix -1 ..." which specify which stage contain the states corresponding to the 1. stage of the nested process (it is here the nested hfg must be loaded and calculated
#' 
#' 
#' 
#' Change precision when reading trans pr integers from hgf file. Infact better to change the loading procedure!
#' 
#' getActionXX must be changed since is not precise enough (read numbers from a text string). 
#' 
#' For function getPolicy add labels.actions = T and labels.states = T and getW = T
#' 
#' Specifiy how to calculate the discount factor (discrete or continious)
#' 
#' Split prob into 3 values when define the MDP
#' 
#' Update policy ite such that can start with a specified policy.
#' 
#' Index must start from 1 (R style).
#' 
#' MDPtoolbox style loading of model. 
#' 
#' Possiblity to specify a model without a duration weight.
#' 
#' Value iteration under ave criterion
#' 
#' @name MDP
#' @aliases MDP-package MDP
#' @docType package
#' @title Markov Decision Processes (MDPs) in R
#' @author Lars Relund \email{lars@@relund.dk}
#' @useDynLib MDP
NA

