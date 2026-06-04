# Package index

## Building an MDP model to a file

- [`binary_mdp_writer()`](http://relund.github.io/mdp/reference/binary_mdp_writer.md)
  : Function for writing an HMDP model to binary files. The function
  defines sub-functions which can be used to define an HMDP model saved
  in a set of binary files.

- [`binary_action_writer()`](http://relund.github.io/mdp/reference/binary_action_writer.md)
  :

  Function for writing actions of a HMDP model to binary files. The
  function defines sub-functions which can be used to define actions
  saved in a set of binary files. It is assumed that the states have
  been defined using `binary_mdp_writer` and that the id of the states
  is known (can be retrieved using e.g. `state_idx_df`).

- [`hmp_mdp_writer()`](http://relund.github.io/mdp/reference/hmp_mdp_writer.md)
  : Function for writing an HMDP model to a hmp file (XML). The function
  define sub-functions which can be used to define an HMDP model stored
  in a hmp file.

- [`memory_mdp_writer()`](http://relund.github.io/mdp/reference/memory_mdp_writer.md)
  : Function for building an HMDP model directly in memory.

- [`get_bin_info_actions()`](http://relund.github.io/mdp/reference/get_bin_info_actions.md)
  : Info about the actions in the HMDP model under consideration.

- [`get_bin_info_states()`](http://relund.github.io/mdp/reference/get_bin_info_states.md)
  : Info about the states in the binary files of the HMDP model under
  consideration.

- [`random_hmdp()`](http://relund.github.io/mdp/reference/random_hmdp.md)
  : Generate a "random" HMDP stored in a set of binary files.

## Loading the MDP

- [`load_mdp()`](http://relund.github.io/mdp/reference/load_mdp.md) :
  Load the HMDP model defined in the binary files. The model are created
  in memory using the external C++ library.
- [`save_mdp()`](http://relund.github.io/mdp/reference/save_mdp.md) :
  Save the MDP to binary files

## Get info about the MDP

- [`get_info()`](http://relund.github.io/mdp/reference/get_info.md) :
  Information about the MDP
- [`plot(`*`<HMDP>`*`)`](http://relund.github.io/mdp/reference/plot.HMDP.md)
  : Plot the state-expanded hypergraph of the MDP.
- [`plot_hypergraph()`](http://relund.github.io/mdp/reference/plot_hypergraph.md)
  : Plot parts of the state expanded hypergraph.
- [`get_hypergraph()`](http://relund.github.io/mdp/reference/get_hypergraph.md)
  : Return the (parts of) state-expanded hypergraph
- [`get_w_idx()`](http://relund.github.io/mdp/reference/get_w_idx.md) :
  Return the index of a weight in the model. Note that index always
  start from zero (C++ style), i.e. the first weight, the first state at
  a stage etc has index 0.

## Finding the optimal policy

- [`run_policy_ite_ave()`](http://relund.github.io/mdp/reference/run_policy_ite_ave.md)
  : Perform policy iteration using the average expected-weight Bellman
  operator on the MDP.
- [`run_policy_ite_discount()`](http://relund.github.io/mdp/reference/run_policy_ite_discount.md)
  : Perform policy iteration using the discounted expected-weight
  Bellman operator on the MDP.
- [`run_value_ite()`](http://relund.github.io/mdp/reference/run_value_ite.md)
  : Perform value iteration on the MDP.

## Get info about the current policy

- [`set_policy()`](http://relund.github.io/mdp/reference/set_policy.md)
  : Modify the current policy by setting policy action of states.
- [`get_policy()`](http://relund.github.io/mdp/reference/get_policy.md)
  : Get parts of the optimal policy.
- [`run_calc_weights()`](http://relund.github.io/mdp/reference/run_calc_weights.md)
  : Calculate weights based on current policy. Normally run after an
  optimal policy has been found.
- [`get_steady_state_pr()`](http://relund.github.io/mdp/reference/get_steady_state_pr.md)
  : Calculate the steady state transition probabilities for the founder
  process (level 0).
- [`get_rpo()`](http://relund.github.io/mdp/reference/get_rpo.md) :
  Calculate the retention pay-off (RPO) or opportunity cost for some
  states.

## Convert to different file formats

- [`convert_binary_to_hmp()`](http://relund.github.io/mdp/reference/convert_binary_to_hmp.md)
  :

  Convert a HMDP model stored in binary format to a `hmp` (XML) file.
  The function simply parse the binary files and create `hmp` files
  using the
  [`hmp_mdp_writer()`](http://relund.github.io/mdp/reference/hmp_mdp_writer.md).

- [`convert_hmp_to_binary()`](http://relund.github.io/mdp/reference/convert_hmp_to_binary.md)
  : Convert a HMDP model stored in a hmp (xml) file to binary file
  format.
