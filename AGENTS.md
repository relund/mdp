# AI agent rules

## Purpose

This repository contains an R package with integrated C++ source code
via `Rcpp`.

## General Guidelines

- Keep changes minimal and focused.
- Preserve existing coding style and file structure.
- Prefer small, reviewable commits.
- Ignore commented out code and TODOs unless they are directly relevant
  to the changes being made.

## R Code

- Instructions for you are given as comments starting with “# AI:”
- Follow standard R package structure.
- Use roxygen2 comments for documentation and alway use markdown
  formatting.
- Keep functions vectorised where practical.
- Avoid introducing unnecessary dependencies.
- Change package version when make package changes using the 4th digit
  e.g. 2.1.3.1. to 2.1.3.2.

## R API naming

- Use `snake_case` for the new R API.
- Rename exported and internal R functions, function arguments, local
  helper functions, variables, list fields, returned data frame columns,
  tests, examples, vignettes, README code, roxygen documentation, and
  other R-facing identifiers to `snake_case` when they are part of the R
  API or user-visible R workflow.
- Do not preserve backwards compatibility for old camelCase names. Do
  not add aliases, wrappers, soft-deprecation layers, or lifecycle
  compatibility shims unless explicitly requested.
- Update all call sites in R code, tests, documentation, examples, and
  vignettes in the same change so the package is internally consistent.
- Keep S3 method names only where R requires the generic/class
  convention, for example `plot.HMDP`, unless the class name or method
  system is explicitly changed.
- Do not rename C++ identifiers or refactor C++ implementation code
  solely for snake-case migration. Only update the minimal R-to-C++
  interface names needed for the R API to work.

## C++ Code

- Use modern C++ (C++11 or newer if already enabled).

- Prefer clear and deterministic implementations over
  micro-optimisations.

- Keep interfaces between R and C++ explicit and minimal.

- Export functions with `Rcpp::export` only when intended for R access.

- Keep the implementation CPU-efficient!

- Use Doxygen for C++ documentation:

  /\*\*

  - @brief Short summary.
  - 
  - Longer explanation.
  - 
  - @param x Description.
  - @return Description. \*/

  Add this documentation in the header files (only keep a single line in
  .cpp files). Document classes, functions, procedues, methods, enums,
  variables and important implementation details. This will help
  maintain clarity and ease of use for future developers and users of
  the package.

## Embedded instructions for you (AI) to follow

Instructions for you are given as comments starting with \# AI \# … \#
EndAI in R/Rmd/qmd files.

## Testing

- Run:
  - `devtools::load_all()`
  - `devtools::document()`
  - `devtools::test()`
  - `R CMD check`
- Ensure examples and tests pass before committing.

## File Conventions

- R code: `R/`
- C++ source: `src/`
- Tests: `tests/testthat/`
- Package metadata: `DESCRIPTION`, `NAMESPACE`

## Avoid

- Reformatting unrelated code.
- Adding large dependencies for small tasks.
- Editing `NAMESPACE` manually when roxygen2 is used.
- Rendering/compile qmd and rmd files unless explicitly instructed.

## Preferred Tools

- `devtools`
- `testthat`
- `roxygen2`
- `Rcpp`
- `ggplot`
- `styler`
