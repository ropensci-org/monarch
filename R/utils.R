
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


#' Convert a mastodon user link to handle
#'
#' @param x Character. Link to user's profile
#'
#' @return Character user handle @user@instance
#' @export
#'
#' @examples
#' masto2user("https://fosstodon.org/@steffilazerte")
#' masto2user("steffi")
#' masto2user("@steffilazerte@fosstodon.org")
#' masto2user(NA)

masto2user <- function(x) {
  if (is.na(x) || stringr::str_count(x, "@") > 1) {
    n <- x
  } else if (stringr::str_detect(x, "http|@")) {
    n <- stringr::str_remove(x, "http(s?)://") |>
      stringr::str_split("/", simplify = TRUE) |>
      as.vector()
    n <- glue::glue("{n[2]}@{n[1]}")
  } else {
    n <- x
  }
  n
}
