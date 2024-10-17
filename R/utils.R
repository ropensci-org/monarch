
check_websites <- function(socials) {
  sapply(socials$value[socials$type == "website"], utils::browseURL)
}

#' Create a cached version of the GH api calls
#'
#' @details `memoise::memoise(gh::gh)`
#'
#' @noRd
gh_cache <- memoise::memoise(gh::gh, omit_args = c(".max_rate"))


name_options <- function(names) {
  names <- stringr::str_remove_all(names, "\\.")
  c(names, stringr::str_remove_all(names, "\\b[A-Z]{1} ")) |>
    unique()
}
