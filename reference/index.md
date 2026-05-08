# Package index

## Building an MDP model to a file

- [`binaryMDPWriter()`](http://relund.github.io/mdp/reference/binaryMDPWriter.md)
  : Function for writing an HMDP model to binary files. The function
  defines sub-functions which can be used to define an HMDP model saved
  in a set of binary files.

- [`binaryActionWriter()`](http://relund.github.io/mdp/reference/binaryActionWriter.md)
  :

  Function for writing actions of a HMDP model to binary files. The
  function defines sub-functions which can be used to define actions
  saved in a set of binary files. It is assumed that the states have
  been defined using `binaryMDPWriter` and that the id of the states is
  known (can be retrieved using e.g. `stateIdxDf`).

- [`hmpMDPWriter()`](http://relund.github.io/mdp/reference/hmpMDPWriter.md)
  : Function for writing an HMDP model to a hmp file (XML). The function
  define sub-functions which can be used to define an HMDP model stored
  in a hmp file.

- [`getBinInfoActions()`](http://relund.github.io/mdp/reference/getBinInfoActions.md)
  : Info about the actions in the HMDP model under consideration.

- [`getBinInfoStates()`](http://relund.github.io/mdp/reference/getBinInfoStates.md)
  : Info about the states in the binary files of the HMDP model under
  consideration.

- [`randomHMDP()`](http://relund.github.io/mdp/reference/randomHMDP.md)
  : Generate a "random" HMDP stored in a set of binary files.

## Loading the MDP

- [`loadMDP()`](http://relund.github.io/mdp/reference/loadMDP.md) : Load
  the HMDP model defined in the binary files. The model are created in
  memory using the external C++ library.
- [`saveMDP()`](http://relund.github.io/mdp/reference/saveMDP.md) : Save
  the MDP to binary files

## Get info about the MDP

- [`getInfo()`](http://relund.github.io/mdp/reference/getInfo.md) :
  Information about the MDP
- [`plot(`*`<HMDP>`*`)`](http://relund.github.io/mdp/reference/plot.HMDP.md)
  : Plot the state-expanded hypergraph of the MDP.
- [`plotHypergraph()`](http://relund.github.io/mdp/reference/plotHypergraph.md)
  : Plot parts of the state expanded hypergraph (experimental).
- [`getHypergraph()`](http://relund.github.io/mdp/reference/getHypergraph.md)
  : Return the (parts of) state-expanded hypergraph
- [`getWIdx()`](http://relund.github.io/mdp/reference/getWIdx.md) :
  Return the index of a weight in the model. Note that index always
  start from zero (C++ style), i.e. the first weight, the first state at
  a stage etc has index 0.

## Finding the optimal policy

- [`runPolicyIteAve()`](http://relund.github.io/mdp/reference/runPolicyIteAve.md)
  : Perform policy iteration (average reward criterion) on the MDP.
- [`runPolicyIteDiscount()`](http://relund.github.io/mdp/reference/runPolicyIteDiscount.md)
  : Perform policy iteration (discounted reward criterion) on the MDP.
- [`runValueIte()`](http://relund.github.io/mdp/reference/runValueIte.md)
  : Perform value iteration on the MDP.

## Get info about the current policy

- [`setPolicy()`](http://relund.github.io/mdp/reference/setPolicy.md) :
  Modify the current policy by setting policy action of states.
- [`getPolicy()`](http://relund.github.io/mdp/reference/getPolicy.md) :
  Get parts of the optimal policy.
- [`runCalcWeights()`](http://relund.github.io/mdp/reference/runCalcWeights.md)
  : Calculate weights based on current policy. Normally run after an
  optimal policy has been found.
- [`getSteadyStatePr()`](http://relund.github.io/mdp/reference/getSteadyStatePr.md)
  : Calculate the steady state transition probabilities for the founder
  process (level 0).
- [`getRPO()`](http://relund.github.io/mdp/reference/getRPO.md) :
  Calculate the retention pay-off (RPO) or opportunity cost for some
  states.

## Convert to different file formats

- [`convertBinary2HMP()`](http://relund.github.io/mdp/reference/convertBinary2HMP.md)
  :

  Convert a HMDP model stored in binary format to a `hmp` (XML) file.
  The function simply parse the binary files and create `hmp` files
  using the
  [`hmpMDPWriter()`](http://relund.github.io/mdp/reference/hmpMDPWriter.md).

- [`convertHMP2Binary()`](http://relund.github.io/mdp/reference/convertHMP2Binary.md)
  : Convert a HMDP model stored in a hmp (xml) file to binary file
  format.
