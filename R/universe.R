#' Get email from R-universe
#'
#' @param name Name of the package contributor
#' @param package Name of the package. If not present, we'll look for any person
#' with that name in the R-universe.
#' @param universe Name of the R-universe.
#'
#' @returns
#' An email
#'
#' @export
#' @examples
#' email_from_universe("Maëlle Salmon", universe = "ropensci")
#' email_from_universe("Maelle S Salmon", universe = "ropensci")
email_from_universe <- function(
  name,
  package = NULL,
  universe = "ropensci-staging"
) {
  universe <- httr2::request(
    sprintf("https://%s.r-universe.dev/api/packages", universe)
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  if (!is.null(package)) {
    package <- purrr::keep(universe, \(x) x[["Package"]] == package) |>
      unlist(recursive = FALSE)
  } else {
    package <- purrr::keep(universe, \(x) {
      x[["_maintainer"]][["name"]] == name
    }) |>
      unlist(recursive = FALSE)

    if (is.null(package)) {
      distances <- purrr::map_dbl(
        universe,
        \(x) stringdist::stringdist(x[["_maintainer"]][["name"]], name)
      )
      package <- universe[[which(distances == min(distances))[1]]]
    }
  }

  data.frame(
    email = package$`_maintainer`$email,
    name = package$`_maintainer`$name,
    package = package$Package
  )
}
