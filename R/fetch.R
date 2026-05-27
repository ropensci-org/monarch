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
#' fetch("Steffi LaZerte", type = "linkedin")
#' fetch(c("Steffi LaZerte", NA, "Yanina Bellini Saibene"))
#' fetch("steffilazerte")
#' fetch("Maëlle Salmon", type = "github")

fetch <- function(values, type = "mastodon", n = 1) {
  if (!type %in% fmt_types()) {
    stop(
      "Incorrect `type`. Must be one of ",
      paste0(fmt_types(), collapse = ", "),
      call. = FALSE
    )
  }

  socials <- cocoon_open()

  github <- purrr::map(
    stats::setNames(nm = tolower(values)),
    \(x) {
      g <- socials$github[
        tolower(socials$value) == tolower(x) & !is.na(socials$value)
      ]
      if (!length(g)) {
        g <- socials$github[
          stringr::str_detect(tolower(socials$value), x) & !is.na(socials$value)
        ]
      }
      g
    }
  ) |>
    purrr::map(\(x) unique(x[!is.null(x) & !is.na(x)]))

  if (any(lengths(github) > 1)) {
    g <- github[lengths(github) > 1]
    g_ids <- purrr::map_chr(g, \(x) paste0(x, collapse = ", "))
    g <- paste(names(g), "(", g_ids, ")")
    warning(
      "Matched multiple ids to ",
      paste0(g, collapse = ", "),
      "Using returning the first match",
      call. = FALSE
    )
    github <- purrr::map(github, \(x) x[1])
  }

  s <- socials[socials$type == type, ]

  purrr::map_chr(github, \(g) {
    r <- s |>
      dplyr::filter(.data$github %in% .env$g) |>
      dplyr::arrange(dplyr::desc(nchar(.data$value))) |>
      dplyr::slice_head(n = n, by = "github") |>
      dplyr::pull(.data$value)
    if (length(r) == 0) {
      r <- NA
    }
    r
  })
}
