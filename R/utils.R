
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
  names_split <- stringr::str_split_1(names, " ")
  if(length(names_split) > 2) {
    names_split <- names_split[c(1, length(names_split))]
  }
  c(names, 
    stringr::str_remove_all(names, "\\b[A-Z]{1} "), 
    names_split[2], # Family name only
    paste(names_split, collapse = " ")
  ) |>
    unique()
}
