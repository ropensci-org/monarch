fmt_key_list <- function(l, keep = NULL) {
  if (!is.null(keep)) {
    l <- l[keep]
  }
  l <- l[sapply(l, \(x) !is.null(x))]
  l <- lapply(l, as.character)
  data.frame(l) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "type",
      values_to = "value"
    ) |>
    dplyr::filter(.data$value != "")
}

fmt_socials <- function(socials, github = NULL) {
  if (!"github" %in% names(socials)) {
    if (!"github" %in% socials$type) {
      if (is.null(github)) {
        warning("No GitHub username identified", call. = FALSE)
        socials$github <- NA
      } else {
        socials$github <- tolower(github)
      }
    } else {
      socials$github <- tolower(socials$value[socials$type == "github"])
    }
  }

  dplyr::mutate(
    socials,

    # Clean up types
    type = tolower(.data$type),
    type = dplyr::case_when(
      .data$type %in% c("blog", "url", "link") ~ "website",
      stringr::str_detect(.data$value, "orcid\\.org") ~ "orcid",
      stringr::str_detect(.data$value, "bsky\\.app") ~ "bluesky",
      stringr::str_detect(.data$value, "youtube") ~ "youtube",
      .default = .data$type
    ),

    # Clean up values
    value = stringr::str_replace_all(
      .data$value,
      c("https?://orcid.org/" = "", "/$" = "")
    ),
    value = dplyr::case_when(
      .data$type == "mastodon" ~ fmt_masto(.data$value),
      .data$type %in% c("twitter", "bluesky", "youtube") ~ fmt_handles(
        .data$value
      ),
      .data$type == "website" ~ fmt_website(.data$value),
      .data$type != "name" ~ tolower(.data$value),
      .default = .data$value
    )
  ) |>
    dplyr::filter(!.data$type %in% c("bio", "img")) |>
    fmt_arrange()
}


fmt_arrange <- function(socials) {
  socials |>
    dplyr::mutate(
      type = factor(.data$type, levels = fmt_types()),
      nchar = nchar(.data$value)
    ) |>
    dplyr::arrange(.data$github, .data$type, .data$nchar) |>
    dplyr::select(-"nchar")
}

fmt_types <- function() {
  c(
    "github",
    "name",
    "alias",
    "mastodon",
    "linkedin",
    "twitter",
    "bluesky",
    "instagram",
    "generic",
    "gitlab",
    "keybase",
    "website",
    "email",
    "orcid",
    "youtube",
    "slack"
  )
}


#' Convert a mastodon user link to handle
#'
#' @param x Character. Link to user's profile
#'
#' @return Character user handle @user@instance
#' @export
#'
#' @examplesIf local_eg()
#' fmt_masto("https://fosstodon.org/@steffilazerte")
#' fmt_masto("steffi lazerte")
#' fmt_masto("@steffilazerte@fosstodon.org")
#' fmt_masto(NA)
#' fmt_masto(c("https://fosstodon.org/@steffilazerte", "https://hackyderm.io/@ropensci"))
#' fmt_masto("none")

fmt_masto <- function(x) {
  x[is.na(x)] <- "none"

  stringr::str_replace_all(
    tolower(x),
    c(
      "https?://([^@]+)/(@.+$)" = "\\2@\\1",
      "^(?!@)" = "@",
      "^@none$" = "none" # Remove @ added in last line for 'none's
    )
  )
}

fmt_handles <- function(x) {
  x[is.na(x)] <- "none"

  stringr::str_replace_all(
    tolower(x),
    c(
      "https?://twitter.com/" = "@",
      "https?://bsky.app/profile/" = "@",
      "https?://www.youtube.com/" = "",
      "^(?!@)" = "@",
      "@none" = "none"
    )
  )
}

fmt_website <- function(x) {
  stringr::str_replace_all(
    tolower(x),
    c(
      "www\\." = "",
      "https?://" = "",
      #"^(?!https?://)" = "",
      "/$" = ""
    )
  )
}
