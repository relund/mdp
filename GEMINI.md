# AI agent rules

## Purpose
This repository contains an R package with integrated C++ source code via `Rcpp`.

## General Guidelines
- Keep changes minimal and focused.
- Preserve existing coding style and file structure.
- Prefer small, reviewable commits.
- Ignore commented out code and TODOs unless they are directly relevant to the changes being made.

## R Code
- Instructions for you are given as comments starting with "# AI:"
- Follow standard R package structure.
- Use roxygen2 comments for documentation and alway use markdown formatting.
- Keep functions vectorised where practical.
- Avoid introducing unnecessary dependencies.
- Change package version when make package changes using the 4th digit e.g. 2.1.3.1. to 2.1.3.2.

## C++ Code
- Use modern C++ (C++11 or newer if already enabled).
- Prefer clear and deterministic implementations over micro-optimisations.
- Keep interfaces between R and C++ explicit and minimal.
- Export functions with `Rcpp::export` only when intended for R access.
- Keep the implementation CPU-efficient!
- Use Doxygen for C++ documentation:

    /**
     * @brief Short summary.
     *
     * Longer explanation.
     *
     * @param x Description.
     * @return Description.
     */

   Add this documentation in the header files. Document classes, functions, procedues, methods, enums, 
   variables and important implementation details. This will help maintain clarity and ease of use 
   for future developers and users of the package.

## Embedded instructions for you (AI) to follow
Instructions for you are given as comments starting with 
# AI
# ...
# EndAI
in R/Rmd/qmd files.

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
