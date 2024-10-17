#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .env
## usethis namespace: end
NULL

# common_docs ------------------
#' Common arguments and documentation for various functions
#'
#' @param socials Data frame. Data frame of previously fetched/loaded social
#'   contact information. Data frame with three columns `type`, `value` and
#'   `github` to indicate the type and value of the contact information and the
#'   GitHub username identifying the individual.
#' @param github Character. Github username
#' @param open_browser Logical. Whether to open the profile page in a browser
#'   for confirmation.
#' @param quiet Logical. Whether to suppress progress messages.
#'
#' @details
#' Use `@inheritParams common_docs` to include the above in any function
#' documentation with a matching argument (will only include matching args)
#'
#' @keywords internal
#' @name common_docs
NULL
