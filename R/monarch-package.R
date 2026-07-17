#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang :=
#' @importFrom rlang .data
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
#' @param names Character. Names to fetch by.
#' @param value Character. Value to search by
#' @param type Character. Type of `value` (e.g., "github" for github handle)
#' @param pkg Character. (Optional) Repository name (package name).
#' @param owner Character. (Optional) Owner of the repository.
#' @param force_masto Logical. Whether to force a re-fetching of Mastodon handles.
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
