
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/relund/mdp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/relund/mdp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Markov Decision Processes (MDPs) in R

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

## Installation

Install the latest stable release from CRAN:

``` r
install.packages("MDP2")
```

Alternatively, install the latest development version from GitHub
(recommended):

``` r
remotes::install_github("relund/mdp")
```

We load the package using

``` r
library(MDP2)
```

Help about the package can be seen by writing

``` r
?MDP2
```

To illustrate the package capabilities, we use a few examples, namely,
an infinite and finite-horizon semi-MDP and a HMDP. Before each example
a short introduction to these models are given.

## Learning more

To get started, first read vignette(“MDP2”).

For more examples see `example("MDP2")`.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Kristensen00" class="csl-entry">

Kristensen, A. R., and E. Jørgensen. 2000. “Multi-Level Hierarchic
Markov Processes as a Framework for Herd Management Support.” *Annals of
Operations Research* 94: 69–89.
<https://doi.org/10.1023/A:1018921201113>.

</div>

<div id="ref-Relund06" class="csl-entry">

Nielsen, L. R., and A. R. Kristensen. 2006. “Finding the $K$ Best
Policies in a Finite-Horizon Markov Decision Process.” *European Journal
of Operational Research* 175 (2): 1164–79.
<https://doi.org/10.1016/j.ejor.2005.06.011>.

</div>

<div id="ref-Puterman94" class="csl-entry">

Puterman, M. L. 1994. *Markov Decision Processes*. Wiley Series in
Probability and Mathematical Statistics. Wiley-Interscience.

</div>

<div id="ref-Tijms03" class="csl-entry">

Tijms, Henk. C. 2003. *A First Course in Stochastic Models*. John Wiley
& Sons Ltd.

</div>

</div>
