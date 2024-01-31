
# `login`: User authentication for Shiny applications

**Author:** Jason Bryer, Ph.D. <jason@bryer.org>  
**Website:** <https://github.com/jbryer/login/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/jbryer/login/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jbryer/login/actions/workflows/R-CMD-check.yaml)
[![](https://img.shields.io/badge/devel%20version-0.1.0-blue.svg)](https://github.com/jbryer/login)
[![](https://www.r-pkg.org/badges/version/login)](https://cran.r-project.org/package=login)
<!-- badges: end -->

This is certainly not the first (and probably not the last)
authentication scheme for Shiny applications.
[ShinyManager](https://datastorm-open.github.io/shinymanager/) is a
great solution if you want to protect the entire application (you pass
your UI object so only authenticated users can view the application).
The
[`shinyauthr`](https://github.com/PaulC91/shinyauthr?tab=readme-ov-file)
is another excellent package for providing simple user authentication.
This package offers a number of features those packages do not,
including:

- The ability for a users to create their own account.
- The ability for users to reset their password using an email system.
- UI elements to control what is visible when logged in or not logged
  in.
- Encrypting of the users’ passwords client side (i.e. the Shiny server
  only sees the encrypted password using MD5).

## Installation

You can download the latest development version using the `remotes`
package:

``` r
remotes::install_github('jbryer/login')
```

## Getting Started

There is a template located at
[`inst/login_template/app.R`](inst/login_template/app.R) that
demonstrates the basic funcationality.

## Disclaimer

I am not a security expert. I have made every effort to use the best
practices to my knowledge. I highly recommend deploying your application
using https (e.g. use nginx and letsencrypt) to ensure all communication
between the client and server are encrypted. Any issues or concerns can
be filed as a Github issue. Use at your own risk.
