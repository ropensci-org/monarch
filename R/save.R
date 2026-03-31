#' Save/Update socials data to local file
#'
#' Save/Update your local copy of the contact information.
#'
#' @return Nothing
#' @export
#'
#' @examples
#' socials_fetch("steffilazerte") |>
#'   cocoon_update()
#'
#' # Or directly
#' cocoon_update("steffilazerte")
#'
#' # Or add parts
#' cocoon_update("steffilazerte", type = "name", value = "Stefanie LaZerte")
#'

cocoon_update <- function(socials, type = NULL, value = NULL) {
  if (!cache_check()) {
    stop("Cannot save socials data without a cache directory", call. = FALSE)
  }

  if (is.character(socials)) {
    if (!is.null(type) && !is.null(value)) {
      socials_new <- cocoon_fetch(value = socials)
      if (nrow(socials_new) == 0) {
        stop(socials, "doesn't exist in cocoon yet", .call = FALSE)
      }
      socials_new <- socials_update(socials_new, type = type, value = value)
    } else {
      socials_new <- socials_fetch(socials)
    }
  } else {
    socials_new <- socials
  }

  socials_update(cocoon_open(), fmt_socials(socials_new)) |>
    readr::write_csv(cache_file())

  cocoon_fetch(socials_new$github[1])
}

#' Fetch all details on a social contact
#'
#' @param value Character. Value to search by
#' @param type Character. Type of `value` (e.g., "github" for github handle)
#'
#' @returns
#' @export
#'
#' @examples
#' cocoon_fetch("steffilazerte")
#' cocoon_fetch("steffi LaZerte", type = "name")

cocoon_fetch <- function(value, type = "github") {
  if (!type %in% fmt_types()) {
    stop(
      "Incorrect `type`. Must be one of ",
      paste0(fmt_types(), collapse = ", "),
      call. = FALSE
    )
  }

  socials <- cocoon_open()
  if (type == "github") {
    gh <- value
  } else {
    gh <- dplyr::filter(
      socials,
      type == .env$type,
      tolower(value) == tolower(.env$value)
    ) |>
      dplyr::pull(.data$github)
  }

  if (length(gh) == 0) {
    stop("No user identified", call. = FALSE)
  }
  dplyr::filter(socials, github == .env$gh)
}

#' Load socials data from local file
#'
#' Reads your local socials data for perusal or modification.
#'
#' @return Socials data frame
#' @export
#'
#' @examples
#' cocoon_open()
cocoon_open <- function() {
  if (file.exists(cache_file())) {
    return(
      readr::read_csv(cache_file(), show_col_types = FALSE) |>
        fmt_socials()
    )
  } else {
    message("No socials book found")
    return(dplyr::tibble(
      type = character(),
      value = character(),
      github = character()
    ))
  }
}

#' Remove Socials by GitHub handle
#'
#' @inheritParams common_docs
#'
#' @returns Socials data frame
#'
#' @export
#' @examples
#' cocoon_fetch("steffilazerte")
#' cocoon_remove("steffilazerte")
#' cocoon_fetch("steffilazerte")

cocoon_remove <- function(github) {
  cocoon_open() |>
    dplyr::filter(.data$github != .env$github) |>
    readr::write_csv(cache_file())

  cocoon_open()
}

cache_file <- function() {
  file.path(cache_dir(), "chrysalis.csv")
}

cache_dir <- function() {
  tools::R_user_dir(package = "monarch")
}

#' Check Cached Dir
#'
#' Checks for the presence of a cache dir. If interactive and not present,
#' asks user if it should be created. If non-interactive and not present, it
#' is created automatically.
#'
#' @returns Logical. TRUE if it exists, FALSE if not.
#'
#' @export
#' @examples
#' cache_check()

cache_check <- function() {
  check <- FALSE
  if (!dir.exists(cache_dir())) {
    rlang::inform(paste0(
      "Cache directory (",
      cache_dir(),
      ") does not exist, create it?"
    ))
    if (interactive()) {
      create <- utils::menu(choices = c("Yes", "No"))
    } else {
      create <- 1
    }

    if (create == 1) {
      dir.create(cache_dir())
      check <- TRUE
    }
  } else {
    check <- TRUE
  }

  check
}

cache_remove <- function() {
  unlink(cache_dir(), recursive = TRUE)
}
