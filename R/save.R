
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

  if(!cache_check()) stop("Cannot save socials data without a cache directory", call. = FALSE)

  if(is.character(socials)) {
    if(!is.null(type) & !is.null(value)) {
      socials <- cocoon_fetch(value = socials)
      if(nrow(socials) == 0) stop(socials, "doesn't exist in cocoon yet", .call = FALSE)
      socials <- socials_update(socials, type = type, value = value)
    } else {
      socials <- socials_fetch(socials)
    }
  } else {
    if(file.exists(cache_file())) {
      socials <- readr::read_csv(cache_file(), show_col_types = FALSE) |>
        fmt_socials() |>
        socials_update(socials_new = socials)
    }
  }

  readr::write_csv(socials, cache_file())
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
  if(file.exists(cache_file())) {
    return(
      readr::read_csv(cache_file(), show_col_types = FALSE) |>
        fmt_socials()
      )
  } else message("No socials book found")
}

cache_file <- function() {
  file.path(cache_dir(), "chrysalis.csv")
}

cache_dir <- function() {
  tools::R_user_dir(package = "monarch")
}

cache_check <- function() {
  check <- FALSE
  if(!dir.exists(cache_dir())) {
    rlang::inform(paste0("Cache directory (", cache_dir(), ") does not exist, create it?"))
    create <- utils::menu(choices = c("Yes", "No"))
    if(create == 1) {
      dir.create(cache_dir())
      check <- TRUE
    }
  } else check <- TRUE

  check
}

cache_remove <- function() {
  unlink(cache_dir(), recursive = TRUE)
}
