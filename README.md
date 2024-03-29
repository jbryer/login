
# <img src="man/figures/login_hex.png" align="right" width="120" align="right" /> login: User Authentication for Shiny Applications

**Author:** Jason Bryer, Ph.D. <jason@bryer.org>  
**Website:** <https://github.com/jbryer/login/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/jbryer/login/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jbryer/login/actions/workflows/R-CMD-check.yaml)
[![](https://img.shields.io/badge/devel%20version-0.9.0-blue.svg)](https://github.com/jbryer/login)
[![](https://www.r-pkg.org/badges/version/login)](https://cran.r-project.org/package=login)
[![](https://img.shields.io/badge/doi-10.5281/zenodo.10633322-blue.svg)](https://doi.org/10.5281/zenodo.10633322)

<!-- badges: end -->

This package provides a framework for adding user authentication to
[Shiny](https://shiny.posit.co) applications. This is unique to other
authentication frameworks such as
[ShinyManager](https://datastorm-open.github.io/shinymanager/) and
[shinyauthr](https://github.com/PaulC91/shinyauthr?tab=readme-ov-file)
in that it provides tools for users to create their own accounts and
reset passwords. This is particularly useful for Shiny applications used
to collect data without a pre-existing user management system. User
credentials are stored in any database that supports the
[DBI](https://dbi.r-dbi.org) interface. Passwords are hashed using MD5
in the browser so that unencrypted passwords are never available to the
Shiny server. For an extra layer of security, you can
[salt](https://en.wikipedia.org/wiki/Salt_(cryptography)) the password
before storing it in the database.

## Installation

You can download the latest development version using the `remotes`
package:

``` r
remotes::install_github('jbryer/login')
```

## Getting Started

There is a template located at
[`inst/login_template/app.R`](inst/login_template/app.R) that
demonstrates the basic functionality. You can copy-and-paste this
template for starting a new Shiny application with authentication. The
following sections will describe the various functions required for the
authentication to work.

## Shiny Server

The `login_server()` function handles all the server side functionality
and must be added to your Shiny server function. The following examples
provides the basic functionality using a SQLite database and a basic
SMTP email server.

``` r
APP_ID <- 'my_login_app' # It is recommended that this is defined in global.R
```

``` r
USER <- login::login_server(
    id = APP_ID,
    db_conn = RSQLite::dbConnect(RSQLite::SQLite(), 'users.sqlite'),
    emailer = emayili_emailer(
        email_host = email_host,
        email_port = email_port,
        email_username = email_username,
        email_password = email_password,
        from_email = reset_password_from_email
    ),
    additional_fields = c('first_name' = 'First Name',
                          'last_name' = 'Last Name'),
    cookie_name = 'loginusername',
    salt = APP_ID
)
```

The `APP_ID` must be a unique identifier for the Shiny module. It is
possible to have more than one `login` module per Shiny application. For
this example, we are only using one and it is important to use the same
ID for all server and UI components.

The `login_server()` returns a `shiny::reactiveValues()` object with two
elements you can use: `logged_in` which is TRUE if the user is logged in
or FALSE otherwise and `username` which contains the username when
logged in.

The `db_conn` parameter can take any database connection from the [`DBI`
R package](https://dbi.r-dbi.org).

The `emailer` parameter is a function with three parameters: `to_email`,
`subject`, and `message`. The `emayili_emailer` is a helper function
that wraps the [`emayili` R
package](https://datawookie.github.io/emayili/) for sending emails
through SMTP. The source code for this function is included below to
serve as a template for wrapping other methods of sending email.

``` r
emayili_emailer <- function(
        email_host = NULL,
        email_port = NULL,
        email_username = NULL,
        email_password = NULL,
        from_email = NULL
) {
    function(to_email, subject, message) {
        email <- emayili::envelope() |>
            emayili::from(from_email) |>
            emayili::to(to_email) |>
            emayili::subject(subject) |>
            emayili::text(message)
        smtp <- emayili::server(
            email_host,
            email_port,
            email_username,
            email_password
        )
        smtp(email, verbose = FALSE)
    }
}
```

The `additional_fields` is a character vector defining other data that
will be collected when users create an account. These data are not used
by the `login` package but will be stored in the users table (defined by
the `users_table` parameter). The values will be used as the input
labels and the names will the column names.

The `cookie_username` specifies the name of the cookie saved so users do
not have to enter the username and password os subsequent visits. Set
this to NULL to disable cookies. Users can still opt out of cookies by
not checking the “Remmeber me” checkbox in the login UI.

The `salt` parameter defines an extra layer encryption for storing the
passwords in the database. At minimum, passwords are hashed using a MD5
algorithm. More details are provided below.

## Shiny UI

On the UI side only the `login_ui()` function is required. This will
create a login box. This will also ensure the JavaScript and CSS assets
are loaded to ensure the password is encrypted client size. It is
important that you use the same `id` across all `login` functions.

``` r
login::login_ui(id = APP_ID)
```

<figure>
<img src="man/figures/screenshot_login.png"
alt="Screenshot for logging in" />
<figcaption aria-hidden="true">Screenshot for logging in</figcaption>
</figure>

To add a log out button use the `logout_button()` function. This will
only display if you are logged in. You can customize the button using
the `label`, `icon`, and `style` parameters which are passed to the
`shiny::actionButton()` function.

``` r
login::logout_button(id = APP_ID)
```

Optionally, you can write your logout procedure by setting
`USER$logged_in <- FALSE` in the server logic.

The `new_user_ui()` function will provide an interface for users to
create a new account. If the `emailer` parameter is set for
`login_server()` and `verify_email = TRUE` then users will need to
verify their email address before the account is created. This is done
by sending a code (six digits by default) to the email address that they
need to enter.

<figure>
<img src="man/figures/screenshot_new_account.png"
alt="Screenshot for creating a new account" />
<figcaption aria-hidden="true">Screenshot for creating a new
account</figcaption>
</figure>

The `reset_password_ui()` function provides UI elements for users to
reset their password. This requires that the `emailer` parameter has
been set for the `login_server()` function. Users enter their email
address and request a code. Once the code has been sent the UI will
changed asking the user to enter the code emailed to them. If they enter
the correct code they can then enter a new password.

<figure>
<img src="man/figures/screenshot_reset_password1.png"
alt="Screenshot of step one for resetting a password" />
<figcaption aria-hidden="true">Screenshot of step one for resetting a
password</figcaption>
</figure>

<figure>
<img src="man/figures/screenshot_reset_password2.png"
alt="Screenshot of step two for resetting a password" />
<figcaption aria-hidden="true">Screenshot of step two for resetting a
password</figcaption>
</figure>

<figure>
<img src="man/figures/screenshot_reset_password3.png"
alt="Screenshot of step three for resetting a password" />
<figcaption aria-hidden="true">Screenshot of step three for resetting a
password</figcaption>
</figure>

## Adapting your Shiny application to the user

On the server side you check the value of `USER$logged_in`, for example,
if you are rendering UI components on the server use the following
pattern:

``` r
output$my_ui <- renderUI({
  if(USER$logged_in) {
    # UI elements visible to users who are logged in.
  } else {
    # UI elements visible to users who are NOT logged in.
  }
})
```

On the UI side, the `is_logged_in()` and `is_not_logged_in()` functions
allow you encapsulate Shiny UI elements so they are only visible when
the user is or is not logged in, respectively.

``` r
login::is_logged_in(
  id = APP_ID,
  div("This only shows when you are logged in!")
)
```

``` r
login::is_not_logged_in(
  id = APP_ID,
  div("This only shows when you are NOT logged in!")
)
```

## Salting

Salting is the process of adding an additional input to the hash to
guard against attacks using precomputed tables of passwords. The
`login_server()` function has two parameters to control salting: the
`salt` parameter defines the word or phrase that will be concatenated to
the password and `salt_algo` defines the algorithm used to encrypt the
password (by default the `sha512` algorithm will be used). The
`digest::digest()` function is used and you can consult the
documentation for details on the hashing algorithms.

Consider, for example, a user who uses “test” for their password and the
`login_server` has been configured to use “login_demo” for the salt.

``` r
password <- "test"
salt <- APP_ID
```

The password is hashed using the md5 algorithm by the browser before
being sent to the Shiny server. Without specifying a salt, this is the
password that would be stored in the database.

``` r
password_md5 <- digest::digest(password, algo = 'md5', serialize = FALSE) 
```

However, sense we specified a salt, the following hash is what will be
stored in the database.

``` r
paste0(salt, password_md5) |> digest::digest(algo = 'sha512', serialize = FALSE) 
#> [1] "d86d29e1c61ec2fb11debbc1ea9b260f73d94a1e044964bd62eede36cbe99136c92431a0269555df7399cc939aa8f9a4364daa23d3c21f838f0d309f62ce3913"
```

Although the password is encrypted client side it is recommended that
you configure your Shiny server to use https. This can be done using
Posit’s enterprise solutions or configure Shiny server behind another
webserver such as [nginx](https://www.nginx.com).

## User management outside of the `login` module

One of the primary motivating factors for creating this package was to
have an authentication framework where users can create and manage their
own accounts. However, it is possible to use only the login feature and
manage users through your own custom code. However, it will require
hashing the password before storing in the database. The following
command using the `digest::digest()` function will produced the same
hash as the MD5 algorithm run in the users browser. If you wish to salt
the passwords an additional step will be required as described in the
salting section above.

``` r
password_md5 <- digest::digest(password, algo = 'md5', serialize = FALSE) 
```

## Acknowledgements

This package relies on the work of [Huidong
Tian](https://github.com/withr) who wrote the JavaScript code to hash
the passwords client side. The original code is available here:
<https://gist.github.com/withr/9001831>

## Disclaimer

I am not a security expert. I have made every effort to use the best
practices as I understand them. I highly recommend deploying your
application using https (e.g. use nginx and letsencrypt) to ensure all
communication between the client and server are encrypted. Any issues or
concerns can be filed as a Github issue. Use at your own risk.
