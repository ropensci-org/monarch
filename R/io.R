#' Find and insert missing social media handles
#'
#' @param df Data frame with columns appended with `_name`, `_github`, `_mastodon`.
#' @param primary Character. Either "github" or "name", which ever column is the
#'   primary column by which social handles should be fetched.
#' @param which_cols Character vector. Social columns to return, any of
#'   "github", "name", "mastodon", "linkedin".
#' @param pkg_col Character. Name of the "pkg" column. Optional but recommended
#'   for fetching by name.
#' @param owner_col Character. Name of the column containing repository owners for packages (`pkg_col`). Optional but recommended for fetching by name.
#' @param prefix Character. Optional prefix on column names (e.g. "maintainer_" for "maintainer_github", "matinainer_name", etc.)
#' @param force_masto Logical. Whether to force an update of the Mastodon handle.
#' @param force Logical. Whether to force an update of all handles.
#'
#' @returns Data frame with added names and social media handles.
#'
#' @export
#' @examples
#' d <- data.frame(author_name = "Steffi LaZerte")
#' add_handles(d, primary = "name", prefix = "author_")
#'
#' d <- data.frame(
#'   author_name = "Steffi LaZerte",
#'   pkg = "weathercan",
#'   owner = "ropensci",
#'   author_github = NA
#' )
#'
#' add_handles(d, primary = "name", prefix = "author_", pkg_col = "pkg", owner_col = "owner")
#'
#' d <- data.frame(github = "steffilazerte")
#' add_handles(d)
#'
#' # If all complete, do not overwrite unless force == TRUE
#' d <- data.frame(github = "steffilazerte", name = "test", mastodon = "test", linkedin = "test")
#' add_handles(d)
#'
#' d <- data.frame(github = "steffilazerte", name = "test", mastodon = "test", linkedin = "test")
#' add_handles(d, force = TRUE)
#'
#' # Use name for linked in (always)
#' d <- data.frame(github = "steffilazerte", name = "test", mastodon = "test")
#' add_handles(d)
#'
#'  d <- data.frame(github = "steffilazerte", name = "test")
#' add_handles(d)

add_handles <- function(
  df,
  primary = "github",
  which_cols = c("github", "name", "mastodon", "linkedin"),
  pkg_col = "pkg",
  owner_col = "owner",
  prefix = NULL,
  force_masto = FALSE,
  force = FALSE
) {
  if (nrow(df) == 0) {
    return(data.frame())
  }

  primary_col <- paste0(prefix, primary)
  github_col <- paste0(prefix, "github")
  name_col <- paste0(prefix, "name")
  mastodon_col <- paste0(prefix, "mastodon")
  linkedin_col <- paste0(prefix, "linkedin") #Assigned to same as Name later

  cols <- c(
    "github" = github_col,
    "name" = name_col,
    "mastodon" = mastodon_col,
    "linkedin" = linkedin_col
  )

  # Which are done?
  complete <- data.frame()
  if (!force) {
    df_check <- dplyr::select(df, dplyr::any_of(unname(cols[which_cols])))
    if (ncol(df_check) == 0) {
      rows_complete <- NULL
    } else {
      rows_complete <- which(complete.cases(df_check))
    }

    if (length(rows_complete) > 0) {
      complete <- dplyr::slice(df, rows_complete)
      df <- dplyr::slice(df, -rows_complete)
    }
    if (nrow(df) == 0) {
      if (is.null(complete[[linkedin_col]]) && "linkedin" %in% which_cols) {
        complete[[linkedin_col]] <- complete[[name_col]]
      }
      return(complete[cols[which_cols]])
    }
  }

  if (
    primary != "github" &&
      github_col %in% names(df) &&
      !anyNA(df[[github_col]])
  ) {
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
      which_cols,
      force_masto
    )
  } else if (primary == "name") {
    df <- add_handles_name(
      df,
      github_col,
      name_col,
      mastodon_col,
      which_cols,
      pkg_col,
      owner_col,
      force_masto
    )
  }

  # Put in placeholders for missing

  if ("name" %in% which_cols) {
    df <- dplyr::mutate(
      df,
      # Missing Name, use GitHub
      !!name_col := dplyr::if_else(
        is.na(.data[[name_col]]),
        .data[[github_col]],
        .data[[name_col]]
      )
    )
  }
  if ("linkedin" %in% which_cols) {
    # LinkedIn always Name
    df <- dplyr::mutate(df, !!linkedin_col := .data[[name_col]])
  }

  if ("mastodon" %in% which_cols) {
    # Missing Mastodon, use Name
    df <- dplyr::mutate(
      df,
      !!mastodon_col := dplyr::if_else(
        is.na(.data[[mastodon_col]]) | .data[[mastodon_col]] == "none",
        .data[[name_col]],
        .data[[mastodon_col]]
      )
    )
  }

  # Add complete back in
  df <- dplyr::bind_rows(df, complete)

  df
}


add_handles_github <- function(
  df,
  github_col,
  name_col,
  mastodon_col,
  which_cols,
  force_masto
) {
  # Add existing
  df <- add_existing(df, name_col, github_col, "name", which_cols)
  df <- add_existing(df, mastodon_col, github_col, "mastodon", which_cols)

  if ("mastodon" %in% which_cols && force_masto) {
    df[[mastodon_col]] <- NA
  }

  cols <- c(
    "github" = github_col,
    c("name" = name_col, "mastodon" = mastodon_col)[which_cols]
  )

  # Get missing mastodon/names from github
  chk <- dplyr::select(df, dplyr::any_of(unname(cols))) |>
    dplyr::filter(!is.na(.data[[cols["github"]]]))

  complete <- complete.cases(chk[[cols[which_cols[which_cols != "github"]]]])

  chk <- chk[!complete, ] |>
    dplyr::distinct() |>
    dplyr::rename(dplyr::any_of(c("github" = github_col)))

  purrr::pwalk(chk, \(github, ...) {
    monarch::socials_fetch(
      github = github,
      which_cols = which_cols,
      force_masto = force_masto
    ) |>
      monarch::cocoon_update()
  })

  # Add newly fetched existing
  df <- add_existing(df, name_col, github_col, "name", which_cols)
  df <- add_existing(df, mastodon_col, github_col, "mastodon", which_cols)

  df
}

add_handles_name <- function(
  df,
  github_col,
  name_col,
  mastodon_col,
  which_cols,
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
  df <- add_existing(df, github_col, name_col, "github", which_cols)
  df <- add_existing(df, mastodon_col, name_col, "mastodon", which_cols)

  if ("mastodon" %in% which_cols && force_masto) {
    df[[mastodon_col]] <- NA

    # Get missing mastodon from existing github
    # (Those with missing github fetched when fetch github below)
    chk <- df |>
      dplyr::select(github_col, mastodon_col) |>
      dplyr::filter(
        !is.na(.data[[github_col]]),
        is.na(.data[[mastodon_col]])
      ) |>
      dplyr::distinct() |>
      dplyr::rename(dplyr::any_of(c("github" = github_col)))

    purrr::pwalk(chk, \(github, ...) {
      monarch::socials_fetch(
        github = github,
        which_cols = which_cols,
        force_masto = force_masto
      ) |>
        monarch::cocoon_update()
    })
  }

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
          which_cols = which_cols,
          force_masto = force_masto
        ) |>
          monarch::cocoon_update()
      }
    )
  }

  # Add newly fetched existing
  df <- add_existing(df, github_col, name_col, "github", which_cols)
  df <- add_existing(df, mastodon_col, name_col, "mastodon", which_cols)

  df
}

add_existing <- function(df, col, by_col, type, which_cols) {
  # Add existing

  if (type %in% c("github", which_cols)) {
    df <- dplyr::mutate(
      df,
      !!col := monarch::fetch(.data[[by_col]], type = type)
    )
  }
  df
}
