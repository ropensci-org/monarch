
<!-- README.md is generated from README.Rmd. Please edit that file -->

# monarch

<!-- badges: start -->
<!-- badges: end -->

monarch provides tools to find and keep track of contact info and social
media handles of rOpenSci community members.

monarch is in very early development.

## Installation

You can install the development version of monarch like so:

``` r
pak::pkg_install("ropensci-org/monarch")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(monarch)
socials_fetch(github = "steffilazerte")
#> Fetching socials from GitHub account
#> Fetching socials from rOpenSci Author pages
#> Mastodon handle already present
#>       type                        value        github
#> 1   github                steffilazerte steffilazerte
#> 2     name               Steffi LaZerte steffilazerte
#> 3 mastodon @steffilazerte@fosstodon.org steffilazerte
#> 4 linkedin               steffi-lazerte steffilazerte
#> 5  twitter               @steffilazerte steffilazerte
#> 6  website             steffilazerte.ca steffilazerte
#> 7    email         sel@steffilazerte.ca steffilazerte
#> 8    orcid          0000-0002-7690-8360 steffilazerte
```
