% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatting.R
\name{fmt_masto}
\alias{fmt_masto}
\title{Convert a mastodon user link to handle}
\usage{
fmt_masto(x)
}
\arguments{
\item{x}{Character. Link to user's profile}
}
\value{
Character user handle @user@instance
}
\description{
Convert a mastodon user link to handle
}
\examples{
masto2user("https://fosstodon.org/@steffilazerte")
masto2user("steffi lazerte")
masto2user("@steffilazerte@fosstodon.org")
masto2user(NA)
masto2user(c("https://fosstodon.org/@steffilazerte", "https://hackyderm.io/@ropensci"))
}
