#' Create and optimize MDPs or hierarchical MDPs with discrete time steps and state space.
#' 
#' @section History: 
#' 
#' 
#'
#' @section To do: 
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

