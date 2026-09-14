#' Find and insert missing social media handles
#'
#' @param df Data frame with at least 1 column appended with `_name` or `_github`.
#' @param primary Character. Either "github" or "name", which ever column is the
#'   primary column by which social handles should be fetched.
#' @param which_cols Character vector. Social columns to return, any of
#'   "github", "name", "mastodon", "linkedin", "bluesky".
#' @param pkg_col Character. Name of the "pkg" column. Optional but recommended
#'   for fetching by name when `primary = "name"`.
#' @param owner_col Character. Name of the column containing repository owners
#'   for packages (`pkg_col`). Optional but recommended for fetching by name
#'   when `primary = "name"`.
#' @param prefix Character. Optional prefix on column names (e.g. "maintainer_"
#'   for "maintainer_github", "maintainer_name", etc.)
#' @param force Logical. Whether to force an update of all handles (not just
#'   those missing).
#'
#' @returns Data frame with added names and social media handles.
#'
#' @export
#'
#' @examplesIf local_eg()
#' d <- data.frame(author_name = "Steffi LaZerte")
#' add_handles(d, primary = "name", prefix = "author_")
#'
#' d <- data.frame(
#'   author_name = "Steffi LaZerte",
#'   pkg = "weathercan",
#'   owner = "ropensci",
#'   author_github = "steffilazerte"
#' )
#'
#' add_handles(
#'   d,
#'   primary = "name",
#'   prefix = "author_",
#'   pkg_col = "pkg",
#'   owner_col = "owner",
#'   which_cols = "github"
#' )
#' add_handles(
#'   d,
#'   primary = "name",
#'   prefix = "author_",
#'   pkg_col = "pkg",
#'   owner_col = "owner"
#' )
#'
#' d <- data.frame(github = "steffilazerte")
#' add_handles(d)
#'
#' # If all complete, do not overwrite unless force == TRUE
#' d <- data.frame(
#'   github = "steffilazerte",
#'   name = "test",
#'   mastodon = "test",
#'   linkedin = "test"
#' )
#' add_handles(d)
#' add_handles(d, force = TRUE)
#'
#' # Use name for LinkedIn (always)
#' d <- data.frame(github = "steffilazerte", name = "test", mastodon = "test")
#' add_handles(d)
#'
#' d <- data.frame(
#'   author_name = c("Steffi LaZerte", "Yanina Bellini Saibene"),
#'   author_github = c("steffilazerte2", NA)
#' )
#' # Keep original github
#' add_handles(d, primary = "name", prefix = "author_")
#' # Get stored github
#' add_handles(d, primary = "name", prefix = "author_", force = TRUE)

add_handles <- function(
  df,
  primary = "github",
  which_cols = c("github", "name", "mastodon", "bluesky", "linkedin"),
  pkg_col = "pkg",
  owner_col = "owner",
  prefix = "",
  force = FALSE
) {
  if (nrow(df) == 0) {
    return(data.frame())
  }

  if (primary != "github" && primary != "name") {
    cli::cli_abort(
      "Cannot fetch handles without a 'github' or 'name' as the primary column"
    )
  }

  # Define columns
  working_cols <- unique(c(which_cols, primary))
  if ("linkedin" %in% working_cols) {
    working_cols <- unique(c(working_cols[working_cols != "linkedin"], "name"))
  }
  fetch_cols <- working_cols[working_cols != primary]

  # Select and rename relevant columns
  h <- df |>
    dplyr::select(dplyr::any_of(stats::setNames(
      paste0(prefix, working_cols),
      working_cols
    ))) |>
    dplyr::distinct()

  # Add missing handles
  h <- add_missing_cols(h, working_cols)

  # Reset handles if forced
  if (force) {
    for (i in fetch_cols) {
      h[i] <- NA_character_
    }
  }

  # Keep sets missing handles
  h <- dplyr::filter(h, dplyr::if_any(dplyr::all_of(fetch_cols), is.na))

  if (nrow(h) > 0) {
    h <- add_handles_by(h, primary, pkg_col, owner_col)
    h <- add_missing_cols(h, which_cols)
    h <- add_placeholders(h)

    # Add handles to original df
    h <- dplyr::rename(
      h,
      stats::setNames(which_cols, paste0(prefix, which_cols))
    )

    df <- add_missing_cols(df, paste0(prefix, which_cols))
    df <- dplyr::rows_upsert(df, h, by = paste0(prefix, primary))
  }

  df
}


add_handles_by <- function(h, by, pkg_col, owner_col) {
  if (by == "name" && (is.null(pkg_col) || is.null(owner_col))) {
    cli::cli_warn(
      "Finding GitHub handles without a package repository and repository owner can be very slow..."
    )
  }

  h <- dplyr::filter(h, !is.na(.data[[by]]))

  # Add existing
  for (c in names(h)[names(h) != by]) {
    h <- add_existing(h, by, c)
  }

  # Add missing handles to cocoon
  missing <- !stats::complete.cases(dplyr::select(h, -dplyr::all_of(by)))
  if (any(missing)) {
    missing <- h[missing, ] |>
      dplyr::distinct()

    # Pretend non-interactive so do not do interactive setting of mastodon
    rlang::with_interactive(value = FALSE, {
      if (by == "name") {
        purrr::pwalk(missing, \(name, pkg = NULL, owner = NULL, ...) {
          socials_fetch(
            name = name,
            pkg = pkg_col,
            owner = owner_col,
            which_cols = names(h)[names(h) != "name"]
          ) |>
            cocoon_update()
        })
      } else if (by == "github") {
        purrr::pwalk(missing, \(github, ...) {
          socials_fetch(
            github = github,
            which_cols = names(h)[names(h) != "github"]
          ) |>
            cocoon_update()
        })
      }
    })

    # Add newly fetched existing
    for (c in names(h)[names(h) != by]) {
      h <- add_existing(h, by, c)
    }
  }

  h
}

add_existing <- function(h, by_col, type) {
  # Add existing - Only if missing
  hh <- dplyr::mutate(h, !!type := monarch::fetch(.data[[by_col]], type = type))
  dplyr::rows_patch(h, hh, by = by_col)
}


add_placeholders <- function(h) {
  pair <- list(
    c("name", "github"),
    c("linkedin", "name"),
    c("mastodon", "name"),
    c("bluesky", "name")
  )

  for (i in seq_along(pair)) {
    h1 <- pair[[i]][1]
    h2 <- pair[[i]][2]
    if (h1 %in% names(h)) {
      h <- dplyr::mutate(
        h,
        !!h1 := dplyr::if_else(
          is.na(.data[[h1]]) | .data[[h1]] == "none",
          .data[[h2]],
          .data[[h1]]
        )
      )
    }
  }

  h
}

add_missing_cols <- function(df, cols) {
  for (i in cols) {
    if (!i %in% names(df)) df[i] <- NA_character_
  }
  df
}
