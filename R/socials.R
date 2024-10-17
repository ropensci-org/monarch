#' Fetch contact information from social media
#'
#' This is a wrapper around the three `socials_xxx()` functions. `socials_gh()`
#' starts (ideally) with a GitHub username to fetch contact info from GitHub,
#' followed by `socials_ro()` which checks rOpenSci author pages and finally, if
#' Mastodon handles have not been found `socials_masto()`.
#'
#' This workflow generally assumes that the person has a GitHub username. You
#' can supply name, which will trigger a search for the GitHub username for you.
#' This will search all of the rOpenSci organization for users with that name
#' which can take a while. However, if you supply a name and a `pkg` (repository
#' in rOpenSci), it can be much faster.
#'
#' @param name Character. Alternatively, a full name (can be slow if `pkg` not
#'   also provided).
#' @param pkg Character. Repository name for an rOpenSci package (in the
#'   `ropensci` organization)
#'
#' @inheritParams common_docs
#'
#' @return Data frame of contact information
#' @export
#'
#' @examples
#' socials_fetch("steffilazerte")

socials_fetch <- function(github = NULL, name = NULL, pkg = NULL,
                          skip_masto = FALSE, quiet = FALSE) {

  if(is.null(github)) github <- gh_search(name, pkg)

  s <- socials_gh(github)
  if("name" %in% s$type) s <- socials_ro(s)
  if(!skip_masto) s <- socials_masto(s)
  s
}


#' Fetch contact information from GitHub
#'
#' Using a GitHub username fetch the contact information from their profile.
#'
#' @inheritParams common_docs
#'
#' @return Data frame of social contact information. Three columns `type`,
#'   `value` and `github` to indicate the type and value of the contact
#'   information and the GitHub username identifying the individual.
#' @export
#'
#' @examples
#'
#' socials_gh("steffilazerte")
socials_gh <- function(github, quiet = FALSE) {

  if(!quiet) message("Fetching socials from GitHub account")
  nm <- gh_cache("/users/{username}", username = github) |>
    fmt_key_list(keep = c("name", "email", "blog"))

  gh_cache("/users/{username}/social_accounts", username = github) |>
    purrr::map(data.frame) |>
    dplyr::bind_rows() |>
    dplyr::rename(dplyr::any_of(c("value" = "url", "type" = "provider"))) |>
    dplyr::bind_rows(nm) |>
    dplyr::bind_rows(data.frame(type = "github", value = github)) |>
    fmt_socials()
}



#' Fetch contact information from rOpenSci author pages
#'
#' Once initial contact information has been sourced via `socials_gh()`,
#' we can add to this by looking for additional details on the rOpenSci author
#' pages (<https://ropensci.org/author>).
#'
#' By default `socials_ro()` will use the `name` stored in the `socials` data frame.
#'
#' If you know this doesn't match exactly that on the rOpenSci author pages, you
#' can supply an alternative with `name`. Ideally however, you would add an
#' additional name to the socials data frame to keep track of minor variations in
#' names.
#'
#' @param name Character. Optional, supply the full name as on RO author pages.
#'   (Doesn't require the socials data frame)
#'
#' @inheritParams common_docs
#'
#' @return Data frame of social contact information. Three columns `type`,
#'   `value` and `github` to indicate the type and value of the contact
#'   information and the GitHub username identifying the individual.
#' @export
#'
#' @examples
#' socials_gh("steffilazerte") |>
#'   socials_ro()
#'
#' socials_ro(names = c("Stefanie LaZerte", "Steffi LaZerte"))

socials_ro <- function(socials = NULL, names = NULL, github = NULL, quiet = FALSE) {

  if (is.null(names) & !is.null(socials)) {
    names <- socials$value[socials$type == "name"]
    if(length(names) == 0) names <- NULL
  }

  if(!quiet) message("Fetching socials from rOpenSci Author pages")

  if(is.null(names)) {
    stop("Must provide either a `socials` data frame or `names`")
  }

  if(is.null(github) & !is.null(socials)) github <- socials$github[1]

  t <- ro_search(names)

  if(all(is.na(t))) {
    message("Could not find rOpenSci author page, try an alias?\n",
            "    (Tried: ", paste0(names, collapse = ", "), ")")
    if(!is.null(socials)) return(socials) else return(invisible())
  }
  t <- t[!is.na(t)]

  if(length(t) != 1) stop("Found multiple matches to rOpenSci author pages: ",
                          paste0(names(t), sep = ", "), call. = FALSE)

  # In case of accents, need to use the HTML encoding with the download origin
  ro <- paste0("https://raw.githubusercontent.com/",
               stringr::str_remove_all(t, "(https://github.com/)|(blob/)")) |>
    httr2::request() |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    yaml::read_yaml(text = _) |>
    fmt_key_list() |>
    fmt_socials(github = github)

  if(!is.null(socials)) {
    ro <- socials_update(socials, ro)
  } else if (length(names) > 1) {
    ro <- socials_update(ro, type = "name", value = names)
  }

  ro
}

#' Fetch Mastodon handle
#'
#' Useful if neither GitHub nor the rOpenSci author pages contain a Mastodon
#' handle. Because this can take a bit of time, if a mastodon handle is
#' detected in the `socials` data frame, `socials_masto()` will skip the search,
#' unless `force = TRUE`. Useful if you think the handle is incorrect.
#'
#' Unlike most other functions, `socials_masto()` permits searching for a handle
#' by name without providing a `socials` data frame or a `github` handle. In
#' this case, the Mastodon handle is returned as a character string and not as
#' a socials data frame.
#'
#' @param force Logical. Whether to force a search even if the handle already
#'   exists.
#'
#' @inheritParams common_docs
#'
#' @return If `socials` or `github` are provided, a data frame of social contact
#'   information. Three columns `type`, `value` and `github` to indicate the
#'   type and value of the contact information and the GitHub username
#'   identifying the individual. Otherwise a character string of the mastodon
#'   handle.
#' @export
#'
#' @examplesIf interactive()
#' socials_gh("steffilazerte") |>
#'  socials_masto(force = TRUE)
#'
#' socials_masto(github = "steffilazerte")
#' socials_masto(names = "steffi")
#'
#' socials_masto(names = "steffi", github = "steffilazerte")

socials_masto <- function(socials = NULL, names = NULL, github = NULL,
                          force = FALSE, open_browser = TRUE, quiet = FALSE) {

  #TODO: Can we get the Github profile information from a mastodon account?


  if(!force && !is.null(socials) && "mastodon" %in% socials$type) {
    if(!quiet) message("Mastodon handle already present")
    return(socials)
  } else if(!quiet) message("Searching for Mastodon handle by", appendLF = FALSE)

  if(is.null(socials) & is.null(names) & is.null(github)) {
    stop("Must provide a `socials` data frame, a `name`, or a `github` handle for searching",
         call. = FALSE)
  }

  # Name(s)
  if(!is.null(socials)) names <- socials$value[socials$type == "name"] |> stats::setNames(nm = _)
  if(!is.null(names)) {
    names_family <- stringr::str_split_i(names, " ", -1) |>
      unique() |>
      stats::setNames(nm = _)
  } else names_family = NULL

  # Websites and GitHub
  website <- NULL
  if(!is.null(socials)) {
    github <- socials$value[socials$type == "github"]
    if(any(socials$type == "website")) website <- socials$value[socials$type == "website"]
  }

  if(!quiet) {
    msg <- ""
    if(!is.null(names)) msg <- c(msg, paste0("    Name: ", paste0(names, collapse = ", ")))
    if(!is.null(github)) msg <- c(msg, paste0("    GitHub: ", github))
    message(paste0(msg, collapse = "\n"))
  }

  # Look by name (whole, then last)
  masto_options <- append(
    if(!is.null(names)) purrr::map(names, masto_search),
    if(!is.null(names_family)) purrr::map(names_family, masto_search)) |>
    append(list(masto_search(github)))

  masto <- masto_best(masto_options, github, website, open_browser)

  if(is.na(masto)) message("No handles found")

  if(!is.null(socials)) {
    socials <- socials_update(socials, type = "mastodon", value = masto)
  } else if(!is.null(github)) {
    socials <- fmt_key_list(list("mastodon" = masto, "github" = github))
  } else {
    socials <- masto
  }
  socials
}

#' Update a socials data frame with new values
#'
#' `socials_update()` takes an existing social data frame and adds or updates
#' new values. Only certain data types are permitted to have more than one
#' value (i.e. names and websites). In all other cases, if there is more than
#' one handle or id value, you will be asked to choose one to keep.
#'
#' @param socials_new Data frame. New data to add/update. If adding only a
#'   single value, use `type` and `value` instead.
#' @param type Character. Type of data to add (to add a single value,
#'   alternative to `socials_new`)
#' @param value Character. Value of data to add  (to add a single value,
#'   alternative to `socials_new`).
#'
#' @return Socials data frame with new values
#' @export
#'
#' @examples
#' socials_fetch("steffilazerte") |>
#'   socials_update(type = "name", value = "Stefanie LaZerte") # Add an alias

socials_update <- function(socials, socials_new = NULL, type = NULL, value = NULL) {

  if(is.null(socials_new) & !is.null(type) & !is.null(value)) {
    socials_new <- data.frame(type = type,
                              value = value,
                              github = tolower(socials$github[1])) |>
      fmt_socials()
  } else if(is.null(socials_new)) {
    stop("Require either a new socials data frame or individual type-value keys to add")
  }

  updateable <- dplyr::filter(socials_new, !type %in% c("website", "name"))
  duplicateable <- dplyr::filter(socials_new, type %in% c("name", "website"))

  if(dplyr::n_distinct(socials_new$type) !=
     dplyr::n_distinct(c(updateable$type, duplicateable$type))) {
    stop("Missing data types", call. = FALSE)
  }

  # Check updates
  add <- dplyr::anti_join(updateable, socials, by = c("type", "value", "github"))
  update <- dplyr::inner_join(add, socials, by = c("type", "github"), suffix = c("", "_orig"))
  add <- dplyr::anti_join(add, update, by = c("type", "github"))

  if(nrow(update) > 0) {
    u <- apply(update, MARGIN = 1, \(x) {
      usethis::ui_yeah(
        paste0(tools::toTitleCase(x[["type"]]), ": ",
               "Use the new value (", x[["value"]], ") ",
               "instead of the original one (", x[["value_orig"]], ")?"))
    })
    update <- update[u, ]
  }
  update <- dplyr::select(update, -"value_orig")

  dplyr::rows_insert(socials, add, by = c("type", "github")) |>
    dplyr::rows_update(update, by = c("type", "github")) |>
    dplyr::rows_upsert(duplicateable, by = c("type", "value", "github")) |>
    dplyr::distinct() |>
    fmt_arrange()
}

socials_build <- function(skip_masto = TRUE) {
  users <- gh_cache(endpoint = "/orgs/{owner}/members", owner = "ropensci", .limit = Inf) |>
    purrr::map_chr("login")

  # Don't re-fetch users we already have
  users <- users[!tolower(users) %in% unique(cocoon_open()$github)]

  lapply(users, \(x) {
    message("GitHub user: ", x, "---------------")
    socials_fetch(x, skip_masto = skip_masto) |>
      cocoon_update()
  })
}



#' Find GH username from repository and full name
#'
#' Look up users of a repository and match to a name. Try with and without
#' initials.
#'
#' @param name Character. Full or partial name of the person for whom you want
#'   to fetch the GitHub username.
#' @param pkg Character. (Optional) Repository name (package name).
#' @param owner Character. (Optional) Owner of the repository.
#'
#' @inheritParams common_docs
#'
#' @return Accepted user name
#' @export
#'
#' @examples
#'
#' gh_user(name = "Steffi E. LaZerte", owner = "ropensci", pkg = "weathercan")
#' gh_user(name = "Steffi", owner = "ropensci", pkg = "weathercan")

gh_search <- function(name, pkg = NULL, owner = "ropensci", open_browser = TRUE) {

  message("Finding GitHub username from name")

  # Get potential users
  if(!is.null(pkg)) {
    endpoint <- "/repos/{owner}/{pkg}/contributors"
  } else endpoint <- "/orgs/{owner}/members"

  users <- gh_cache(endpoint = endpoint, owner = "ropensci", pkg = pkg,
                    .limit = Inf) |>
    purrr::map_chr("login")

  # Names to check - Try also without initials
  n <- name_options(name)

  u <- purrr::map(users, \(x) gh_cache("/users/{username}", username = x)) |>
    stats::setNames(users) |>
    purrr::map(\(x) fmt_key_list(x, keep = c("name", "email", "blog", "login"))) |>
    dplyr::bind_rows(.id = "github") |>
    dplyr::mutate(match = stringr::str_detect(tolower(value), tolower(n))) |>
    dplyr::filter(any(match), .by = "github") |>
    dplyr::filter(type == "name") |>
    dplyr::mutate(url = paste0("https://github.com/", github)) |>
    dplyr::select(github, name = value, url) |>
    dplyr::distinct()

  if(nrow(u) > 0) {
    if(nrow(u) > 5) q <- 1:5 else q <- seq_len(nrow(u))

    if(open_browser) {
      stats::setNames(u$url[q], u$name[q]) |>
        lapply(utils::browseURL)
    }

    repeat {
      opts <- u$github[q]
      if(nrow(u) > i) opts <- c(opts, "Try some more")
      opts <- c(opts, "None of the above")

      rlang::inform("Which handle is correct?")
      github <- utils::menu(opts)
      if(opts[github] == "Try some more") {
        i <- max(q) + 1
      } else {
        github <- opts[github]
        break
      }
    }
    return(github)
  } else {
    message("No handles found")
  }
}



#' Search for rOpenSci author pages
#'
#' @param names Character. Name(s) to search for.
#'
#' @return File path of the author page.
#'
#' @noRd
ro_search <- function(names) {

  names <- name_options(names)
  names <- stringr::str_replace_all(names, " ", "-")
  names <- tolower(names)

  sapply(names, \(x) {
    t <- try(gh_cache("/repos/ropensci/roweb3/contents/content/author/{name}/_index.md",
                      name = x)[["html_url"]], silent = TRUE)
    if(inherits(t, "try-error")) t <- NA else t
  })
}

#' Search for Mastodon accounts
#'
#' @param name Character, Name to search for in Mastodon accounts.
#'
#' @return Data frame of Mastodon accounts and related info
#'
#' @noRd

masto_search <- function(name) {
  m <- rtoot::search_accounts(name)

  if(nrow(m) > 0) {
    m <- m |>
      dplyr::filter(!bot) |>
      dplyr::mutate(
        github = purrr::map(fields, \(x) fields_match(x, "github", "(?<=>github.com/)[a-zA-Z-]+")),
        website = purrr::map(fields, \(x) fields_match(x, "web|blog|homepage", "http[^\"]+")),
        rstats = stringr::str_detect(tolower(note), "rstats")) |>
      dplyr::select("id", "username", "acct", "display_name", "github", "website", "rstats", "url") |>
      tidyr::unnest(cols = c("github", "website"), keep_empty = TRUE) |>
      dplyr::mutate(sort = factor(id, levels = id),
                    acct = fmt_masto(acct)) |>
      dplyr::arrange(dplyr::desc(rstats), id)
  }
  m
}

masto_best <- function(masto_options, github, website, open_browser) {

  if(all(lengths(masto_options) == 0)) return(NA)

  # Get best options - Best if match github, otherwise most common
  best <- masto_compare(masto_options, github, website)

  if(nrow(best) == 0) {
    # If we couldn't match by github handle, omit all accounts with a handle (because wouldn't be the same)
    if(!is.null(github)) masto_options <- purrr::map(masto_options, \(x) dplyr::filter(x, is.na(github)))
    best <- masto_common(masto_options)
  }


  # No options
  if(nrow(best) == 0) {

    return(NA)

  } else if(nrow(best) == 1 && !is.na(best$github) && best$github == github) {
    # If matches github, return without asking

    message("Found ", best$acct)
    masto <- best$acct

  } else if(nrow(best) > 0) {
    # Otherwise ask for confirmation

    if(nrow(best) > 5) q <- 1:5 else q <- seq_len(nrow(best))

    repeat {
      if(open_browser) {
        stats::setNames(best$url[q], best$acct[q]) |>
          lapply(utils::browseURL)
      }

      opts <- best$acct[q]
      if(nrow(best) > max(q)) opts <- c(opts, "Try some more")
      opts <- c(opts, "None of the above")

      rlang::inform("Which handle is correct?")
      masto <- utils::menu(opts)
      if(opts[masto] == "Try some more") {
        q <- seq(max(q) + 1, length.out = 5)
      } else {
        masto <- opts[masto]
        break
      }
    }

    if(masto == "None of the above") return(NA)

  }

  masto
}

masto_compare <- function(opts, github, website) {

  m0 <- dplyr::bind_rows(opts) |>
    dplyr::distinct()

  m <- data.frame()
  if(!is.null(github)) m <- dplyr::filter(m0, .data$github == .env$github)
  if(nrow(m) == 0 && !is.null(website)) {
    m <- dplyr::filter(
      m0,
      stringr::str_remove_all(.data$website, "(https?://)|(www\\.)|(/$)") ==
        stringr::str_remove_all(.env$website, "(https?://)|(www\\.)|(/$)"))
  }

  m
}

#' Recursive function to find common Mastodon accounts
#'
#' Recurses down the list of data frames with Mastodon account information.
#' Captures common handles in each list and passes them on.
#'
#' @param masto
#'
#' @return Data frame of Mastodon account information with only common accounts.
#'
#' @noRd
masto_common <- function(masto) {

  if(length(masto) == 1) return (masto[[1]])

  if(nrow(masto[[1]]) > 0 & nrow(masto[[2]]) > 0) {
    m <- dplyr::inner_join(masto[[1]], masto[[2]], by = names(masto[[1]]))
    if(nrow(m) == 0) m <- dplyr::bind_rows(masto[[1]], masto[[2]])
  } else if(nrow(masto[[1]]) > 0) {
    m <- masto[[1]]
  } else m <- masto[[2]]

  if(length(masto) > 2) masto_common(append(list(m), masto[-c(1:2)]))

  m
}

fields_match <- function(x, subset, pattern) {
  if(length(x) > 0) {
    # Arrange and take first to prioritize 'webpage' over 'blog' and github over gihub gpg where there may be more than one
    x <- x[order(x$name, decreasing = TRUE), ]
    x <- x$value[stringr::str_detect(tolower(x$name), subset)][1]
    stringr::str_extract(x, pattern)
  } else NA
}
