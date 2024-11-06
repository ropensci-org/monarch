
#' Fetch a value
#'
#' Fetches a single value from your local socials data frame.
#'
#' @param values Character. Values of social data to match by.
#' @param type Character. Type of social data to fetch.
#' @param n Numeric. Maximum number of matches per value to return.
#'
#' @return Character string of the value of the type of data requested
#' @export
#'
#' @examples
#' fetch("Steffi LaZerte", type = "mastodon")
#' fetch(c("Steffi LaZerte", "Yanina Bellini Saibene"))
#' fetch("steffilazerte")


fetch <- function(values, type = "mastodon", n = 1) {

  if(!type %in% types) {
    stop("Incorrect `type`. Must be one of ", paste0(types, collapse = ", "),
         call. = FALSE)
  }

  socials <- cocoon_open()

  github <- purrr::map(
    stats::setNames(nm = values),
    \(x) socials$github[stringr::str_detect(socials$value, x)]) |>
    purrr::map(\(x) unique(x[!is.null(x) & !is.na(x)]))

  if(any(lengths(github) > 1)) {
    warning("Matched multiple ids to ",
            paste0(names(github)[lengths(github) > 1], collapse = ", "),
            call. = FALSE)
  }

  vals <- socials[socials$type == type, ]
  vals$value[match(github, vals$github)]
}


