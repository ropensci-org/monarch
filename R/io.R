#' Find and insert missing social media handles
#'
#' @param df Data frame with columns appended with `_name`, `_github`, `_mastodon`.
#'
#' @returns Data frame with added names and social media handles.
#'
#' @export
#' @examples
#' d <- data.frame(author_name = "Steffi LaZerte")
#' add_handles(d, primary = "name", prefix = "author_")
#'
#' d <- data.frame(github = "steffilazerte")
#' add_handles(d)

add_handles <- function(
  df,
  primary = "github",
  pkg_col = NULL,
  owner_col = NULL,
  prefix = NULL,
  force_masto = FALSE
) {
  if (nrow(df) == 0) {
    return(data.frame())
  }

  primary_col <- paste0(prefix, primary)
  github_col <- paste0(prefix, "github")
  name_col <- paste0(prefix, "name")
  mastodon_col <- paste0(prefix, "mastodon")
  linkedin_col <- paste0(prefix, "linkedin") #Assigned to same as Name later

  if (primary != "github" && !github_col %in% names(df)) {
    cli::cli_abort(
      "If you have github handles you should use `primary = 'github'`"
    )
  }
  if (primary != "github" && primary != "name") {
    cli::cli_abort(
      "Cannot fetch handles without a 'github' or 'name' as the primary column"
    )
  }

  if (primary == "github") {
    df <- add_handles_github(
      df,
      github_col,
      name_col,
      mastodon_col,
      force_masto
    )
  } else if (primary == "name") {
    df <- add_handles_name(
      df,
      github_col,
      name_col,
      mastodon_col,
      pkg_col,
      owner_col,
      force_masto
    )
  }

  # Put in placeholders for missing

  df <- df |>
    dplyr::mutate(
      # Missing Name, use GitHub
      !!name_col := dplyr::if_else(
        is.na(.data[[name_col]]),
        .data[[github_col]],
        .data[[name_col]]
      ),

      # LinkedIn always Name
      !!linkedin_col := .data[[name_col]],

      # Missing Mastodon, use Name
      !!mastodon_col := dplyr::if_else(
        is.na(.data[[mastodon_col]]) | .data[[mastodon_col]] == "none",
        .data[[name_col]],
        .data[[mastodon_col]]
      )
    )

  df
}

add_handles_github <- function(
  df,
  github_col,
  name_col,
  mastodon_col,
  force_masto
) {
  # Add existing
  df <- dplyr::mutate(
    df,
    !!name_col := monarch::fetch(.data[[github_col]], type = "name"),
    !!mastodon_col := monarch::fetch(.data[[github_col]], type = "mastodon")
  )

  if (force_masto) {
    df[[mastodon_col]] <- NA
  }

  # Get missing mastodon/names from github
  chk <- df |>
    dplyr::select(github_col, name_col, mastodon_col) |>
    dplyr::filter(
      !is.na(.data[[github_col]]),
      (is.na(.data[[name_col]]) | is.na(.data[[mastodon_col]]))
    ) |>
    dplyr::distinct() |>
    dplyr::rename(dplyr::any_of(c("github" = github_col)))

  purrr::pwalk(chk, \(github, ...) {
    monarch::socials_fetch(github = github, force_masto = force_masto) |>
      monarch::cocoon_update()
  })

  # Add newly fetched existing
  df <- dplyr::mutate(
    df,
    !!name_col := monarch::fetch(.data[[github_col]], type = "name"),
    !!mastodon_col := monarch::fetch(.data[[github_col]], type = "mastodon")
  )

  df
}

add_handles_name <- function(
  df,
  github_col,
  name_col,
  mastodon_col,
  pkg_col,
  owner_col,
  force_masto
) {
  if (is.null(pkg_col) || is.null(owner_col)) {
    cli::cli_warn(
      "Finding GitHub handles without a package repository and repository owner can be very slow..."
    )
  }

  # Add existing
  df <- dplyr::mutate(
    df,
    !!github_col := monarch::fetch(.data[[name_col]], type = "github"),
    !!mastodon_col := monarch::fetch(.data[[name_col]], type = "mastodon")
  )

  if (force_masto) {
    df[[mastodon_col]] <- NA
  }

  # Get missing mastodon from existing github
  # (Those with missing github fetched when fetch github below)
  chk <- df |>
    dplyr::select(github_col, mastodon_col) |>
    dplyr::filter(!is.na(.data[[github_col]]), is.na(.data[[mastodon_col]])) |>
    dplyr::distinct() |>
    dplyr::rename(dplyr::any_of(c("github" = github_col)))

  purrr::pwalk(chk, \(github, ...) {
    monarch::socials_fetch(github = github, force_masto = force_masto) |>
      monarch::cocoon_update()
  })

  # Get missing github
  chk <- df |>
    dplyr::select(name_col, github_col, dplyr::any_of(c(pkg_col, owner_col))) |>
    dplyr::filter(!is.na(.data[[name_col]]), is.na(.data[[github_col]])) |>
    dplyr::distinct() |>
    dplyr::rename(dplyr::any_of(c(
      "name" = name_col,
      "pkg" = pkg_col,
      "owner" = owner_col
    )))

  if (nrow(chk) > 0) {
    purrr::pwalk(
      chk,
      \(name, pkg = NULL, owner = NULL, ...) {
        monarch::socials_fetch(
          name = name,
          pkg = pkg,
          owner = owner,
          force_masto = force_masto
        ) |>
          monarch::cocoon_update()
      }
    )
  }

  # Add newly fetched existing
  df <- dplyr::mutate(
    df,
    !!github_col := monarch::fetch(.data[[name_col]], type = "github"),
    !!mastodon_col := monarch::fetch(.data[[name_col]], type = "mastodon")
  )

  df
}
