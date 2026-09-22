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
    package <- purrr::keep(universe, \(x) x[["Package"]] == package)
  } else {
    # TODO: add fuzzy string search?
    package <- purrr::keep(universe, \(x) x[["_maintainer"]][["name"]] == name)
  }

  return(package[[1]]$`_maintainer`$email)
}
