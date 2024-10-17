fmt_key_list <- function(l, keep = NULL) {
  if(!is.null(keep)) l <- l[keep]
  l <- l[sapply(l, \(x) !is.null(x))]
  l <- lapply(l, as.character)
  data.frame(l) |>
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "type", values_to = "value") |>
    dplyr::filter(value != "")
}

fmt_socials <- function(socials, github = NULL) {

  if(!"github" %in% names(socials)) {
    if(!"github" %in% socials$type) {
      if(is.null(github)) {
        warning("No GitHub username identified", call. = FALSE)
        socials$github <- NA
      } else socials$github <- tolower(github)
    } else socials$github <- tolower(socials$value[socials$type == "github"])
  }

  dplyr::mutate(
    socials,

    # Clean up types
    type = tolower(type),
    type = dplyr::case_when(
      type %in% c("blog", "url", "link") ~ "website",
      stringr::str_detect(value, "orcid\\.org") ~ "orcid",
      stringr::str_detect(value, "bsky\\.app") ~ "bluesky",
      .default = type),

    # Clean up values
    value = stringr::str_replace_all(
      value,
      c("https?://orcid.org/" = "",
        "/$" = "")),
    value = dplyr::case_when(
      type == "mastodon" ~ fmt_masto(value),
      type %in% c("twitter", "bluesky") ~ fmt_handles(value),
      type == "website" ~ fmt_website(value),
      type != "name" ~ tolower(value),
      .default = value)
  ) |>
    dplyr::filter(!type %in% c("bio", "img")) |>
    fmt_arrange()
}


fmt_arrange <- function(socials) {
  socials |>
    dplyr::mutate(type = factor(type, levels = types),
                  nchar = nchar(value)) |>
    dplyr::arrange(github, type, nchar) |>
    dplyr::select(-"nchar")
}

types <- c("github", "name", "alias", "mastodon", "linkedin", "twitter", "bluesky",
           "instagram", "generic", "gitlab", "keybase", "website", "email", "orcid")


#' Convert a mastodon user link to handle
#'
#' @param x Character. Link to user's profile
#'
#' @return Character user handle @user@instance
#' @export
#'
#' @examples
#' masto2user("https://fosstodon.org/@steffilazerte")
#' masto2user("steffi lazerte")
#' masto2user("@steffilazerte@fosstodon.org")
#' masto2user(NA)
#' masto2user(c("https://fosstodon.org/@steffilazerte", "https://hackyderm.io/@ropensci"))

fmt_masto <- function(x) {
  stringr::str_replace_all(
    tolower(x),
    c("https?://([^@]+)/(@.+$)" = "\\2@\\1",
      "^(?!@)" = "@"))
}

fmt_handles <- function(x) {
  stringr::str_replace_all(
    tolower(x),
    c("https?://twitter.com/" = "@",
      "https?://bsky.app/profile/" = "@",
      "^(?!@)" = "@"))
}

fmt_website <- function(x) {
  stringr::str_replace_all(
    tolower(x),
    c("www\\." = "",
      "https?://" = "",
      #"^(?!https?://)" = "",
      "/$" = ""))
}
