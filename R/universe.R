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
  universe <- get_universe(universe)

  if (!is.null(package)) {
    person_information <- purrr::keep(universe, \(x) {
      x[["Package"]] == package
    }) |>
      unlist(recursive = FALSE)
  } else {
    person_information <- purrr::keep(universe, \(x) {
      x[["_maintainer"]][["name"]] == name
    }) |>
      unlist(recursive = FALSE)

    if (is.null(person_information)) {
      distances <- purrr::map_dbl(
        universe,
        \(x) stringdist::stringdist(x[["_maintainer"]][["name"]], name)
      )
      person_information <- universe[[which(distances == min(distances))[1]]]
    }
  }

  # TODO: also use _authors when it exists as an API output field
  # to not only get maintainers
  data.frame(
    email = person_information$`_maintainer`$email,
    name = person_information$`_maintainer`$name,
    package = person_information$Package
  )
}

.get_universe <- function(universe) {
  httr2::request(
    sprintf("https://%s.r-universe.dev/api/packages", universe)
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

get_universe <- memoise::memoise(.get_universe)
