# The MDP2 package

The `MDP2` package in R is a package for solving Markov decision
processes (MDPs) with discrete time-steps, states and actions. Both
traditional MDPs (Puterman 1994), semi-Markov decision processes
(semi-MDPs) (Tijms 2003) and hierarchical-MDPs (HMDPs) (Kristensen and
Jørgensen 2000) can be solved under a finite and infinite time-horizon.

Building and solving an MDP is done in two steps. First, the MDP is
built and saved in a set of binary files. Next, you load the MDP into
memory from the binary files and apply various algorithms to the model.

The package implement well-known algorithms such as policy iteration and
value iteration under different criteria e.g. average reward per time
unit and expected total discounted reward. The model is stored using an
underlying data structure based on the *state-expanded directed
hypergraph* of the MDP (Nielsen and Kristensen (2006)) implemented in
`C++` for fast running times.

To illustrate the package capabilities have a look at the vignettes:

- Building MDP models
  [`vignette("building")`](http://relund.github.io/mdp/articles/building.md).
- Solving an infinite-horizon MDP (semi-MDP)
  [`vignette("infinite-mdp")`](http://relund.github.io/mdp/articles/infinite-mdp.md).
- Solving a finite-horizon MDP (semi-MDP)
  [`vignette("finite-mdp")`](http://relund.github.io/mdp/articles/finite-mdp.md).
- Solving an infinite-horizon HMDP
  [`vignette("infinite-hmdp")`](http://relund.github.io/mdp/articles/infinite-hmdp.md).

## References

Kristensen, A. R., and E. Jørgensen. 2000. “Multi-Level Hierarchic
Markov Processes as a Framework for Herd Management Support.” *Annals of
Operations Research* 94: 69–89.
<https://doi.org/10.1023/A:1018921201113>.

Nielsen, L. R., and A. R. Kristensen. 2006. “Finding the $`K`$ Best
Policies in a Finite-Horizon Markov Decision Process.” *European Journal
of Operational Research* 175 (2): 1164–79.
<https://doi.org/10.1016/j.ejor.2005.06.011>.

Puterman, M. L. 1994. *Markov Decision Processes*. Wiley Series in
Probability and Mathematical Statistics. Wiley-Interscience.

Tijms, Henk. C. 2003. *A First Course in Stochastic Models*. John Wiley
& Sons Ltd.
