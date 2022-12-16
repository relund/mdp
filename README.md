
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/relund/mdp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/relund/mdp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Markov Decision Processes (MDPs) in R

The `MDP2` package in R is a package for solving Markov decision
processes (MDPs) with discrete time-steps, states and actions. Both
traditional MDPs \[@Puterman94\], semi-Markov decision processes
(semi-MDPs) \[@Tijms03\] and hierarchical-MDPs (HMDPs) \[@Kristensen00\]
can be solved under a finite and infinite time-horizon.

Building and solving an MDP is done in two steps. First, the MDP is
built and saved in a set of binary files. Next, you load the MDP into
memory from the binary files and apply various algorithms to the model.

The package implement well-known algorithms such as policy iteration and
value iteration under different criteria e.g. average reward per time
unit and expected total discounted reward. The model is stored using an
underlying data structure based on the *state-expanded directed
hypergraph* of the MDP (@Relund06) implemented in `C++` for fast running
times.

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
